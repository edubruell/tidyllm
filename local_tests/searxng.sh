#!/usr/bin/env bash
# Local SearXNG instance for the websearch() SearXNG backend, run with Apple's
# `container` tool (brew install container; macOS 26, Apple silicon).
#
# Usage: bash local_tests/searxng.sh <command>
#   start    start the container services and SearXNG, create them if needed
#   stop     stop SearXNG, then the container services if this script started
#            them and nothing else runs
#   status   show whether the services and SearXNG are running
#   test     run one JSON search:  bash local_tests/searxng.sh test "some query"
#   logs     show the SearXNG log  (logs -f to follow)
#   update   pull the latest image and recreate the container
#   remove   delete the container and its image; settings are kept
#
# Settings live in local_context/searxng/settings.yml (gitignored; written on
# first start with a random secret key, JSON output on, limiter off).
# Port: SEARXNG_PORT, default 8888. Point tidyllm at it with
#   Sys.setenv(SEARXNG_SERVER = "http://127.0.0.1:8888")

set -euo pipefail

NAME=searxng
IMAGE=docker.io/searxng/searxng:latest
PORT="${SEARXNG_PORT:-8888}"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
CONF_DIR="$ROOT/local_context/searxng"
SETTINGS="$CONF_DIR/settings.yml"
URL="http://127.0.0.1:$PORT"
STARTED_MARKER="$CONF_DIR/.services_started_by_script"

die() { echo "ERROR: $*" >&2; exit 1; }

for tool in container curl python3 openssl; do
  command -v "$tool" >/dev/null 2>&1 || die "$tool not found"
done

services_up() { container system status >/dev/null 2>&1; }
exists()      { services_up && container list --all --quiet 2>/dev/null | grep -qx "$NAME"; }
running()     { services_up && container list --quiet 2>/dev/null | grep -qx "$NAME"; }

write_settings() {
  [ -f "$SETTINGS" ] && return
  local secret
  secret="$(openssl rand -hex 32)"
  [ ${#secret} -eq 64 ] || die "could not generate a secret key with openssl"
  mkdir -p "$CONF_DIR"
  cat > "$SETTINGS" <<EOF
use_default_settings: true
server:
  secret_key: "$secret"
  limiter: false
  bind_address: "0.0.0.0"
search:
  formats: [html, json]
EOF
  echo "Wrote $SETTINGS"
}

wait_ready() {
  echo "Waiting for SearXNG on $URL ..."
  for _ in $(seq 1 60); do
    if curl -sf -m 5 "$URL/healthz" >/dev/null 2>&1; then
      echo "  ready"
      return 0
    fi
    sleep 1
  done
  die "SearXNG did not answer within 60s; see: bash local_tests/searxng.sh logs"
}

cmd_start() {
  write_settings
  if ! services_up; then
    echo "Starting container services ..."
    container system start --enable-kernel-install
    touch "$STARTED_MARKER"
  fi
  if running; then
    echo "SearXNG already running"
  elif exists; then
    echo "Starting existing SearXNG container ..."
    container start "$NAME" >/dev/null
  else
    if lsof -i ":$PORT" -sTCP:LISTEN >/dev/null 2>&1; then
      die "port $PORT is already in use; set SEARXNG_PORT to another port"
    fi
    echo "Creating SearXNG container from $IMAGE ..."
    container run -d --name "$NAME" \
      -p "127.0.0.1:$PORT:8080" \
      -v "$CONF_DIR:/etc/searxng" \
      "$IMAGE" >/dev/null
  fi
  wait_ready
  curl -s -m 30 "$URL/search?q=warm+up&format=json" >/dev/null 2>&1 || true
  echo "Sys.setenv(SEARXNG_SERVER = \"$URL\")"
}

cmd_stop() {
  if running; then
    echo "Stopping SearXNG ..."
    container stop "$NAME" >/dev/null
  fi
  if services_up; then
    if [ ! -f "$STARTED_MARKER" ]; then
      echo "Container services were started outside this script; left on"
    elif [ -n "$(container list --quiet 2>/dev/null)" ]; then
      echo "Other containers are running; container services left on"
    else
      echo "Stopping container services ..."
      container system stop
      rm -f "$STARTED_MARKER"
    fi
  fi
  echo "Stopped"
}

cmd_status() {
  if ! services_up; then
    echo "container services: stopped"
    echo "SearXNG: stopped"
    return
  fi
  echo "container services: running"
  if running; then
    echo "SearXNG: running on $URL"
    curl -sf -m 5 "$URL/healthz" >/dev/null 2>&1 && echo "  health check: ok" || echo "  health check: no answer"
  elif exists; then
    echo "SearXNG: stopped (container exists)"
  else
    echo "SearXNG: not created"
  fi
}

cmd_test() {
  running || die "SearXNG is not running; run: bash local_tests/searxng.sh start"
  local query="${1:-tidyllm R package}"
  curl -sf -m 30 -G "$URL/search" --data-urlencode "q=$query" -d format=json |
    python3 -c '
import json, sys
d = json.load(sys.stdin)
print(len(d["results"]), "results; unresponsive engines:", d.get("unresponsive_engines") or "none")
for r in d["results"][:5]:
    print("-", r.get("title"), "|", r.get("url"), "|", r.get("engine"))
' || die "search failed; check settings.yml has json in search.formats"
}

cmd_logs() {
  services_up || die "container services are stopped; run start first"
  exists || die "SearXNG container does not exist"
  container logs "$@" "$NAME"
}

cmd_update() {
  services_up || die "container services are stopped; run start first"
  echo "Pulling $IMAGE ..."
  container image pull "$IMAGE"
  if exists; then
    container stop "$NAME" >/dev/null 2>&1 || true
    container delete "$NAME" >/dev/null
  fi
  cmd_start
}

cmd_remove() {
  services_up || die "container services are stopped; run start first"
  if exists; then
    container stop "$NAME" >/dev/null 2>&1 || true
    container delete "$NAME" >/dev/null
    echo "Deleted container $NAME"
  fi
  container image delete "$IMAGE" >/dev/null 2>&1 && echo "Deleted image $IMAGE" || true
  echo "Settings kept in $SETTINGS"
}

case "${1:-}" in
  start)  cmd_start ;;
  stop)   cmd_stop ;;
  status) cmd_status ;;
  test)   shift; cmd_test "${1:-}" ;;
  logs)   shift; cmd_logs "$@" ;;
  update) cmd_update ;;
  remove) cmd_remove ;;
  *) sed -n '/^# Usage/,/^#$/p' "$0" | sed -e '$d' -e 's/^# \{0,1\}//'; exit 1 ;;
esac

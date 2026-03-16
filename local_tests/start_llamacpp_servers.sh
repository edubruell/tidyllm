#!/usr/bin/env bash
# Start llama.cpp servers needed for local_tests/providers/llamacpp.R
#
# Chat server  — port 8080 — Qwen3-0.6B-Q8_0.gguf
# Embed server — port 8081 — Qwen3-Embedding-0.6B-Q8_0.gguf
#
# Usage: bash local_tests/start_llamacpp_servers.sh [model_dir]
# Default model_dir: ~/models  (override via LLAMACPP_MODEL_DIR env var)

MODEL_DIR="${1:-${LLAMACPP_MODEL_DIR:-$HOME/models}}"
CHAT_MODEL="$MODEL_DIR/Qwen3-0.6B-Q8_0.gguf"
EMBED_MODEL="$MODEL_DIR/Qwen3-Embedding-0.6B-Q8_0.gguf"
CHAT_PORT=8080
EMBED_PORT=8081

die() { echo "ERROR: $*" >&2; exit 1; }

command -v llama-server >/dev/null 2>&1 || die "llama-server not found (brew install llama.cpp)"
[ -f "$CHAT_MODEL" ]  || die "Chat model not found: $CHAT_MODEL"
[ -f "$EMBED_MODEL" ] || die "Embed model not found: $EMBED_MODEL\n  Download with:\n  Rscript -e \"devtools::load_all(); llamacpp_download_model('Qwen/Qwen3-Embedding-0.6B-GGUF', 'Qwen3-Embedding-0.6B-Q8_0.gguf', '$MODEL_DIR')\""

# ── Chat server ───────────────────────────────────────────────────────────────

if lsof -i ":$CHAT_PORT" -sTCP:LISTEN >/dev/null 2>&1; then
  echo "Chat server already running on port $CHAT_PORT — skipping"
else
  echo "Starting chat server on port $CHAT_PORT ..."
  llama-server \
    -m "$CHAT_MODEL" \
    -c 8192 \
    --port "$CHAT_PORT" \
    --host 127.0.0.1 \
    --log-disable \
    > /tmp/llamacpp_chat.log 2>&1 &
  CHAT_PID=$!
  echo "  PID $CHAT_PID — log: /tmp/llamacpp_chat.log"
fi

# ── Embed server ──────────────────────────────────────────────────────────────

if lsof -i ":$EMBED_PORT" -sTCP:LISTEN >/dev/null 2>&1; then
  echo "Embed server already running on port $EMBED_PORT — skipping"
else
  echo "Starting embed server on port $EMBED_PORT ..."
  llama-server \
    -m "$EMBED_MODEL" \
    -c 512 \
    --port "$EMBED_PORT" \
    --host 127.0.0.1 \
    --embeddings \
    --log-disable \
    > /tmp/llamacpp_embed.log 2>&1 &
  EMBED_PID=$!
  echo "  PID $EMBED_PID — log: /tmp/llamacpp_embed.log"
fi

# ── Wait for both servers to be ready ────────────────────────────────────────

echo "Waiting for servers to become healthy ..."
for PORT in $CHAT_PORT $EMBED_PORT; do
  for i in $(seq 1 30); do
    STATUS=$(curl -s "http://localhost:$PORT/health" 2>/dev/null | grep -o '"ok"' || true)
    if [ "$STATUS" = '"ok"' ]; then
      echo "  port $PORT: ok"
      break
    fi
    sleep 1
    if [ "$i" -eq 30 ]; then
      echo "  port $PORT: timed out — check /tmp/llamacpp_$([ "$PORT" = "$CHAT_PORT" ] && echo chat || echo embed).log" >&2
    fi
  done
done

echo "Done. Run: source(\"local_tests/providers/llamacpp.R\")"

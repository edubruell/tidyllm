# Offline streaming regression suite.
#
# Replays the recorded wire bytes in local_tests/fixtures/streams/ through the
# real `handle_stream()` methods over a real HTTP connection (local webfakes
# server) and asserts the result still matches the pre-refactor baseline in
# local_tests/fixtures/stream_baseline.rds.
#
# This is the safety net for the 0.6.0 Phase A refactor. It is a LOCAL test by
# design: no fixture and no webfakes dependency enters tests/testthat or
# DESCRIPTION.
#
#   Rscript -e 'devtools::load_all("."); source("local_tests/features/stream_replay.R")'
#
# Needs no API key and no network beyond localhost.

source("local_tests/test_harness.R")
source("local_tests/stream_replay.R")

llt_suite("stream_replay")

if (!requireNamespace("webfakes", quietly = TRUE)) {
  stop("webfakes is needed to replay recorded streams: install.packages('webfakes')")
}

baseline <- readRDS("local_tests/fixtures/stream_baseline.rds")
server   <- start_stream_replay_server()
on.exit(server$stop(), add = TRUE)

# Fixtures the current implementation cannot read. Gemini's parser understands
# only the chunked-JSON-array endpoint, so the alt=sse recordings error today.
# Phase A migrates Gemini to alt=sse; when it does, these move into the parity
# block above and this vector empties out.
KNOWN_UNPARSEABLE <- c("gemini_plain_sse", "gemini_multibyte_sse", "gemini_thinking_sse")

# ── Parity with the pre-refactor baseline ─────────────────────────────────────

for (nm in setdiff(names(server$fixtures), KNOWN_UNPARSEABLE)) {
  local({
    fixture_name <- nm
    base <- baseline[[fixture_name]]

    llt_test(sprintf("%s replays to the baseline reply", fixture_name), {
      res <- replay_fixture(server, fixture_name)
      llt_expect_true(identical(res$reply, base$reply),
                      sprintf("reply drifted\n  baseline: %s\n  now:      %s",
                              base$reply, res$reply))
      llt_expect_true(identical(res$n_events, base$n_events),
                      sprintf("event count drifted: baseline %d, now %d",
                              base$n_events, res$n_events))
    })

    if (is.null(base$metadata$error)) {
      llt_test(sprintf("%s replays to the baseline token counts", fixture_name), {
        res <- replay_fixture(server, fixture_name)
        for (f in c("model", "prompt_tokens", "completion_tokens", "total_tokens")) {
          llt_expect_true(identical(res$metadata[[f]], base$metadata[[f]]),
                          sprintf("%s drifted: baseline %s, now %s", f,
                                  format(base$metadata[[f]]), format(res$metadata[[f]])))
        }
        llt_expect_true(isTRUE(res$metadata$stream), "stream flag is not TRUE")
      })
    }
  })
}

# ── Multibyte across chunk boundaries ─────────────────────────────────────────

# The recorded chunks are replayed at their original boundaries, so a parser
# that splits UTF-8 sequences shows up here rather than in production.
for (nm in grep("multibyte", setdiff(names(server$fixtures), KNOWN_UNPARSEABLE), value = TRUE)) {
  local({
    fixture_name <- nm
    llt_test(sprintf("%s survives multibyte chunk boundaries", fixture_name), {
      res <- replay_fixture(server, fixture_name)
      llt_expect_true(grepl("日本語テスト", res$reply, fixed = TRUE),
                      "Japanese text missing or mangled")
      llt_expect_true(grepl("Grüße", res$reply, fixed = TRUE),
                      "German umlauts missing or mangled")
      llt_expect_true(!grepl("�", res$reply, fixed = TRUE),
                      "reply contains a Unicode replacement character")
    })
  })
}

# ── Gemini alt=sse: the parity gap Phase A closes ─────────────────────────────

for (nm in KNOWN_UNPARSEABLE) {
  local({
    fixture_name <- nm
    llt_test(sprintf("%s is still unreadable by the json-array parser (pre-Phase A)", fixture_name), {
      res <- tryCatch(replay_fixture(server, fixture_name),
                      error = function(e) list(error = conditionMessage(e)))
      llt_expect_true(!is.null(res$error),
                      paste0("gemini alt=sse now parses. If Phase A landed the SSE ",
                             "migration, move this fixture into the parity block ",
                             "and drop it from KNOWN_UNPARSEABLE."))
    })
  })
}

llt_report("stream_replay")

# ── Termination behaviour, reported rather than asserted ──────────────────────
#
# Truncated streams: the connection closes without the provider's terminal
# event. Four of the six loops spin forever on this today, which is why this is
# a printed report and not a test; a suite that hangs is worse than one that
# tells you where the hangs are. 0.6.0 acceptance criterion 4 turns every row
# below into "raises", at which point these become real llt_test() cases.

stream_termination_report <- function(.server, .timeout = 8) {
  rows <- lapply(names(.server$fixtures), function(nm) {
    t0 <- Sys.time()
    setTimeLimit(elapsed = .timeout, transient = TRUE)
    res <- tryCatch({ replay_fixture(.server, nm, .chunks = 1); "completed" },
                    error = function(e) {
                      if (grepl("elapsed time limit|Zeitlimit", conditionMessage(e))) "HANGS"
                      else "raises"
                    })
    setTimeLimit()
    data.frame(
      fixture  = nm,
      provider = .server$fixtures[[nm]]$provider,
      truncated_behaviour = res,
      seconds  = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
    )
  })
  do.call(rbind, rows)
}

message("\n── Truncated-stream behaviour (pre-Phase A) ──────────────────────────")
message("Every row must read 'raises' once acceptance criterion 4 is met.\n")
print(stream_termination_report(server), row.names = FALSE)

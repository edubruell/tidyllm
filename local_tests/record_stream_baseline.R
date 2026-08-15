# Record what the CURRENT streaming implementation makes of each fixture.
#
# This is the actual safety net for the Phase A refactor. The fixtures are the
# provider's bytes; this file is tidyllm's reading of them, captured before the
# pump replaces six hand-rolled loops. After the refactor,
# local_tests/features/stream_replay.R replays the same bytes and asserts the
# same reply, event count and token counts come out.
#
#   Rscript -e 'devtools::load_all("."); source("local_tests/record_stream_baseline.R")'
#
# Re-run it ONLY when the fixtures themselves are re-recorded. Re-running it
# after a refactor overwrites the baseline with whatever the new code does,
# which is precisely the thing it exists to catch.

source("local_tests/stream_replay.R")

BASELINE_PATH <- "local_tests/fixtures/stream_baseline.rds"

if (file.exists(BASELINE_PATH)) {
  message("Baseline already exists at ", BASELINE_PATH, ".")
  message("Delete it deliberately if the fixtures were re-recorded; refusing to overwrite.")
} else {
  server <- start_stream_replay_server()
  on.exit(server$stop(), add = TRUE)

  fixtures <- names(server$fixtures)
  message("\n── Recording pre-refactor stream baseline ────────────────────────────")

  baseline <- list()
  for (nm in fixtures) {
    cat(sprintf("  %-34s", nm))
    res <- tryCatch(replay_fixture(server, nm), error = function(e) {
      list(error = conditionMessage(e))
    })
    baseline[[nm]] <- res
    if (!is.null(res$error)) {
      cat("ERROR:", res$error, "\n")
    } else {
      cat(sprintf("%3d events, reply: %s\n",
                  res$n_events,
                  substr(gsub("\n", " ", res$reply), 1, 40)))
    }
  }

  attr(baseline, "recorded_at") <- as.character(Sys.time())
  attr(baseline, "git_sha") <- tryCatch(
    system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE),
    error = function(e) NA_character_
  )
  saveRDS(baseline, BASELINE_PATH)
  message("\nWrote ", BASELINE_PATH)
}

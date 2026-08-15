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

# Gemini's legacy fixtures. Phase A migrated the request to `alt=sse`, so the
# chunked-JSON-array recordings are now a shape the package no longer asks for
# and no longer parses. They are kept because they are what the pre-refactor
# baseline was recorded from, and the SSE parity block below compares against
# their replies.
GEMINI_LEGACY <- c("gemini_plain_jsonarray", "gemini_multibyte_jsonarray")

# Fixtures with no baseline entry that predates the refactor, because the code
# could not read them when the baseline was taken.
NO_BASELINE <- c("gemini_plain_sse", "gemini_multibyte_sse", "gemini_thinking_sse")

SKIP_PARITY <- c(GEMINI_LEGACY, NO_BASELINE)

# ── Parity with the pre-refactor baseline ─────────────────────────────────────

for (nm in setdiff(names(server$fixtures), SKIP_PARITY)) {
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
for (nm in grep("multibyte", setdiff(names(server$fixtures), GEMINI_LEGACY), value = TRUE)) {
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

# ── Gemini: parity across the alt=sse migration ───────────────────────────────

# The strongest available proof that the migration did not change what a user
# sees: the same prompt, recorded from both endpoints, must yield the same
# reply. The json-array reply comes from the pre-refactor baseline, since the
# buffer-and-match parser that produced it no longer exists.
GEMINI_PARITY <- list(
  c(sse = "gemini_plain_sse",     legacy = "gemini_plain_jsonarray"),
  c(sse = "gemini_multibyte_sse", legacy = "gemini_multibyte_jsonarray")
)

for (pair in GEMINI_PARITY) {
  local({
    p <- pair
    llt_test(sprintf("%s reply matches the pre-migration %s baseline", p[["sse"]], p[["legacy"]]), {
      res <- replay_fixture(server, p[["sse"]])
      llt_expect_true(identical(res$reply, baseline[[p[["legacy"]]]]$reply),
                      sprintf("alt=sse changed the reply\n  json-array: %s\n  sse:        %s",
                              baseline[[p[["legacy"]]]]$reply, res$reply))
    })
  })
}

llt_test("gemini alt=sse reports token counts", {
  res <- replay_fixture(server, "gemini_plain_sse")
  llt_expect_true(is.numeric(res$metadata$total_tokens) && res$metadata$total_tokens > 0,
                  "no total_tokens from the sse stream")
  llt_expect_true(isTRUE(res$metadata$stream), "stream flag is not TRUE")
})

llt_test("gemini separates thinking parts from reply text", {
  res <- replay_fixture(server, "gemini_thinking_sse")
  llt_expect_true(nzchar(res$reply), "empty reply from the thinking stream")
  llt_expect_true(is.numeric(res$metadata$specific_metadata$thinking_tokens),
                  "thinking_tokens missing from metadata")
})

llt_test("the legacy gemini json-array shape is no longer parsed", {
  res <- tryCatch(replay_fixture(server, "gemini_plain_jsonarray"),
                  error = function(e) list(error = conditionMessage(e)))
  llt_expect_true(!is.null(res$error) || !identical(res$reply, baseline$gemini_plain_jsonarray$reply),
                  "the json-array endpoint still parses; is alt=sse actually being sent?")
})

# ── Every provider streams through the shared pump ────────────────────────────

llt_test("every provider streams through the one shared pump", {
  shared <- S7::method(handle_stream, list(APIProvider, new_S3_class("httr2_response")))
  for (nm in names(server$fixtures)) {
    api <- stream_fixture_api(server$fixtures[[nm]])
    dispatched <- S7::method(handle_stream, list(S7::S7_class(api), new_S3_class("httr2_response")))
    llt_expect_true(identical(dispatched, shared),
                    sprintf("%s dispatches to its own handle_stream method rather than the shared pump",
                            server$fixtures[[nm]]$provider))
  }
})

llt_test("stream transports are what each provider actually speaks", {
  for (nm in names(server$fixtures)) {
    fx  <- server$fixtures[[nm]]
    api <- stream_fixture_api(fx)
    wire <- if (grepl("event-stream", fx$content_type)) "sse" else "lines"
    # The json-array fixtures predate the migration and are not what the
    # package asks for any more, so they say nothing about the transport.
    if (nm %in% GEMINI_LEGACY) next
    llt_expect_true(identical(api@stream_transport, wire),
                    sprintf("%s: transport is %s but the wire is %s",
                            nm, api@stream_transport, wire))
  }
})

# ── Termination: a truncated stream must raise, never hang ────────────────────

# 0.6.0 acceptance criterion 4. Replaying only the first chunk closes the
# connection without the provider's terminal event. Before the shared pump,
# ChatCompletions, OpenAI and Claude span forever on this; the pump checks
# `resp_stream_is_complete()` on every empty read, so every provider now raises.
#
# The time limit is the point of the test: if a provider regresses to spinning,
# this fails in 10 seconds instead of hanging the suite.
for (nm in setdiff(names(server$fixtures), GEMINI_LEGACY)) {
  local({
    fixture_name <- nm
    llt_test(sprintf("%s raises on a truncated stream", fixture_name), {
      setTimeLimit(elapsed = 10, transient = TRUE)
      res <- tryCatch({
        replay_fixture(server, fixture_name, .chunks = 1)
        "completed"
      }, error = function(e) {
        if (grepl("elapsed time limit|Zeitlimit", conditionMessage(e))) "HUNG"
        else conditionMessage(e)
      })
      setTimeLimit()

      llt_expect_true(!identical(res, "HUNG"),
                      "the loop span until the time limit instead of raising")
      llt_expect_true(!identical(res, "completed"),
                      "a truncated stream was reported as a complete reply")
    })
  })
}

# ── Perplexity: parser-level only ─────────────────────────────────────────────
#
# This account has no Perplexity credits, so no fixture could be recorded and
# the pump cannot be driven end to end. Its parser is at least exercised
# directly against hand-written events matching the documented shape. Replace
# this block with a recorded fixture if credits ever appear.

local({
  api <- api_perplexity(short_name = "perplexity", long_name = "Perplexity",
                        api_key_env_var = "PERPLEXITY_API_KEY")
  # Literal wire JSON, not toJSON() of an R list: a NULL field round-trips to
  # `{}` rather than `null`, which is not what the provider actually sends.
  sse <- function(json) list(data = json)

  llt_test("perplexity parser emits text deltas", {
    ev <- parse_stream_event(api, sse(
      '{"choices":[{"delta":{"content":"hello"},"finish_reason":null}]}'
    ))
    llt_expect_true(identical(ev$kind, "text"), "delta was not classified as text")
    llt_expect_true(identical(ev$text, "hello"), "delta text lost")
    llt_expect_true(!isTRUE(ev$done), "a plain delta ended the stream")
    llt_expect_true(isTRUE(ev$keep), "a delta event was not kept for metadata")
  })

  llt_test("perplexity parser terminates on any finish_reason", {
    for (reason in c("stop", "length")) {
      ev <- parse_stream_event(api, sse(sprintf(
        '{"choices":[{"delta":{"content":""},"finish_reason":"%s"}]}', reason
      )))
      llt_expect_true(isTRUE(ev$done),
                      sprintf("finish_reason '%s' did not end the stream", reason))
    }
  })

  llt_test("perplexity parser drops events without choices", {
    ev <- parse_stream_event(api, sse('{"choices":[]}'))
    llt_expect_true(!isTRUE(ev$keep), "an event with no choices was kept")
    llt_expect_true(!isTRUE(ev$done), "an event with no choices ended the stream")
  })
})

llt_report("stream_replay")

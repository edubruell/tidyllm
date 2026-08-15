# Offline tests for the non-blocking chat driver.
#
# The job machinery is driven against the recorded stream fixtures over a real
# HTTP connection, so the event loop, the file-descriptor wait, the pump and the
# job's status model are all exercised without an API key.
#
# What this cannot cover is the part that makes async worth having, namely that
# a *slow* provider yields between chunks: the replay server answers instantly,
# so a fixture usually completes inside one tick. The incremental behaviour is
# checked live instead, in local_tests/features/async_chat_live.R.
#
#   Rscript -e 'devtools::load_all("."); source("local_tests/features/async_chat_replay.R")'
#
# Needs no API key and no network beyond localhost.

source("local_tests/test_harness.R")
source("local_tests/stream_replay.R")

llt_suite("async_chat_replay")

for (pkg in c("webfakes", "later", "promises")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(pkg, " is needed for the async replay tests: install.packages('", pkg, "')")
  }
}

server <- start_stream_replay_server()
on.exit(server$stop(), add = TRUE)

#' Build a job against a replayed fixture.
#'
#' `send_chat()` cannot be used here: it goes through a provider function, which
#' would send the request to the provider rather than to the replay server. The
#' built request is assembled by hand so that the job machinery itself is what
#' is under test.
replay_job <- function(name, on_chunk = NULL, mode = "stream") {
  fx  <- server$fixtures[[name]]
  api <- stream_fixture_api(fx)

  built <- tidyllm:::new_chat_request(
    .request = httr2::request(server$url(name)) |>
      httr2::req_body_json(list(model = "replay", stream = TRUE)),
    .api     = api,
    .llm     = llm_message("hi"),
    .body    = list(model = "replay"),
    .mode    = mode,
    .timeout = 20
  )
  tidyllm:::new_chat_job(built, on_chunk)
}

drive <- function(job, seconds = 10) {
  # No scheduling here: the job starts itself, which is what `send_chat()`
  # relies on. Driving it is only a matter of turning the event loop.
  deadline <- Sys.time() + seconds
  while (identical(job$env$status, "running") && Sys.time() < deadline) {
    later::run_now(0.05)
  }
  job$env$status
}

# ── The job reaches the same LLMMessage the blocking path would ───────────────

llt_test("a replayed job completes and yields an LLMMessage", {
  job <- replay_job("claude_plain")
  llt_expect_true(identical(check_job(job), "running") || identical(check_job(job), "done"),
                  "a fresh job should be running or already done, never anything else")

  llt_expect_true(identical(drive(job), "done"),
                  paste0("job did not finish: ", job$env$status))

  reply <- fetch_job(job)
  llt_expect_s7(reply, LLMMessage)
  llt_expect_reply(reply)
})

llt_test("an async job and a blocking one produce the same reply", {
  # The claim that async differs only in how the bytes arrive. Anything else
  # would mean the driver had grown its own interpretation of a response, which
  # is exactly what this release removed for streaming.
  job <- replay_job("claude_plain")
  drive(job)
  async_reply <- get_reply(fetch_job(job))

  blocking <- replay_fixture(server, "claude_plain")

  llt_expect_true(identical(async_reply, blocking$reply),
                  sprintf("async and blocking replies differ\n  async:    %s\n  blocking: %s",
                          async_reply, blocking$reply))
})

llt_test("metadata comes back with the stream flag set", {
  job <- replay_job("claude_plain")
  drive(job)
  meta <- get_metadata(fetch_job(job))

  llt_expect_true(isTRUE(meta$stream), "an async streamed job should report stream = TRUE")
  llt_expect_true(!is.na(meta$completion_tokens), "token counts missing from an async job")
})

# ── The push and pull views of the same text ──────────────────────────────────

llt_test(".on_chunk receives the deltas", {
  seen <- character()
  job  <- replay_job("claude_plain", on_chunk = function(d) seen <<- c(seen, d))
  drive(job)

  llt_expect_true(length(seen) > 1,
                  sprintf("expected several deltas, got %d", length(seen)))
  llt_expect_true(identical(paste0(seen, collapse = ""), get_partial(job)),
                  "the pushed deltas and get_partial() disagree")
})

llt_test("get_partial on a finished job returns the whole reply", {
  # So that a polling loop needs no special case for the last iteration.
  job <- replay_job("claude_plain")
  drive(job)

  llt_expect_true(identical(get_partial(job), get_reply(fetch_job(job))),
                  "get_partial() on a finished job does not match its reply")
})

llt_test("a non-streaming job reports no partial text until it is done", {
  job <- replay_job("claude_plain", mode = "value")
  llt_expect_true(identical(get_partial(job), ""),
                  "a non-streaming job claimed partial text")
  # The fixture is an SSE body, so this job fails to parse as JSON. What is
  # under test is the status model, not the parse: an error must land in the
  # job rather than escaping into the caller's session from a later callback.
  status <- drive(job)
  llt_expect_true(status %in% c("done", "error"),
                  paste0("a non-streaming job hung in status ", status))
})

# ── Failure and cancellation are states, not surprises ────────────────────────

llt_test("a truncated stream ends the job in error, not in a hang", {
  # The same abnormal termination the blocking pump raises on. In a callback
  # there is no caller to raise into, so it has to become the job's status.
  fx  <- server$fixtures[["claude_plain"]]
  api <- stream_fixture_api(fx)
  built <- tidyllm:::new_chat_request(
    .request = httr2::request(server$url("claude_plain", chunks = 2)) |>
      httr2::req_body_json(list(model = "replay")),
    .api = api, .llm = llm_message("hi"), .body = list(), .mode = "stream", .timeout = 20
  )
  job <- tidyllm:::new_chat_job(built, NULL)

  llt_expect_true(identical(drive(job), "error"),
                  paste0("expected an error status, got ", job$env$status))
  llt_expect_true(grepl("without a completion signal", conditionMessage(job$env$error)),
                  paste0("wrong error: ", conditionMessage(job$env$error)))

  err <- tryCatch({ fetch_job(job); NA_character_ }, error = function(e) conditionMessage(e))
  llt_expect_true(!is.na(err), "fetch_job() on a failed job returned instead of raising")
})

llt_test("a cancelled job stays cancelled and cannot be fetched", {
  job <- replay_job("claude_plain")
  cancel_job(job)

  llt_expect_true(identical(check_job(job), "cancelled"),
                  paste0("expected cancelled, got ", check_job(job)))

  err <- tryCatch({ fetch_job(job); NA_character_ }, error = function(e) conditionMessage(e))
  llt_expect_true(grepl("cancelled", err %||% ""),
                  paste0("fetch_job() on a cancelled job said: ", err))
})

llt_test("cancelling a finished job is a no-op", {
  job <- replay_job("claude_plain")
  drive(job)
  cancel_job(job)

  llt_expect_true(identical(check_job(job), "done"),
                  "cancelling a finished job changed its status")
})

# ── Dispatch ──────────────────────────────────────────────────────────────────

llt_test("check_job and fetch_job reject objects that are not jobs", {
  for (verb in list(check_job, fetch_job)) {
    err <- tryCatch({ verb(list(1, 2)); NA_character_ }, error = function(e) conditionMessage(e))
    llt_expect_true(grepl("send_chat|send_batch|deep_research", err %||% ""),
                    paste0("unhelpful error for a non-job: ", err))
  }
})

llt_test("every provider with a builder registers it for send_chat", {
  # `send_chat()` reaches the builder through the provider's own registration,
  # so a provider that gains a builder but forgets the `build = ` entry would
  # be silently unavailable to it.
  providers <- c("claude", "openai", "gemini", "ollama", "groq", "mistral",
                 "deepseek", "openrouter", "llamacpp", "perplexity",
                 "chat_completions", "azure_openai")
  for (p in providers) {
    meta <- do.call(get(p), list(.called_from = "metadata"))
    llt_expect_true("build" %in% names(meta$supported_args),
                    sprintf("%s() registers no build action, so send_chat() cannot reach it", p))
  }
})

# -- get_stream() and as.promise() --------------------------------------------

llt_test("get_stream yields every delta, buffer first then live", {
  job <- replay_job("claude_plain")

  collected <- character()
  consume <- coro::async(function() {
    for (delta in coro::await_each(get_stream(job))) collected <<- c(collected, delta)
  })
  consume()

  deadline <- Sys.time() + 10
  while (identical(job$env$status, "running") && Sys.time() < deadline) later::run_now(0.05)
  # The generator needs a few more turns after the job ends to drain its buffer.
  for (i in 1:50) later::run_now(0.02)

  llt_expect_true(length(collected) > 1,
                  sprintf("expected several deltas, got %d", length(collected)))
  llt_expect_true(identical(paste0(collected, collapse = ""), get_partial(job)),
                  "the streamed deltas do not reassemble into the job's text")
})

llt_test("get_stream on a finished job replays the whole reply", {
  # The buffer-first half of the contract: asking late must not lose what has
  # already arrived, or a Shiny app that attaches after the first token would
  # render a truncated reply.
  job <- replay_job("claude_plain")
  drive(job)

  collected <- character()
  consume <- coro::async(function() {
    for (delta in coro::await_each(get_stream(job))) collected <<- c(collected, delta)
  })
  consume()
  for (i in 1:50) later::run_now(0.02)

  llt_expect_true(identical(paste0(collected, collapse = ""), get_reply(fetch_job(job))),
                  "a stream taken after completion did not replay the reply")
})

llt_test("get_stream refuses a non-streaming job", {
  job <- replay_job("claude_plain", mode = "value")
  err <- tryCatch({ get_stream(job); NA_character_ }, error = function(e) conditionMessage(e))
  llt_expect_true(grepl("stream = FALSE|no deltas", err %||% ""),
                  paste0("unclear error for a non-streaming job: ", err))
  drive(job)
})

llt_test("a job can be used as a promise", {
  # What Shiny's ExtendedTask consumes. Registered at load time rather than in
  # NAMESPACE, so this also checks that the registration actually happened.
  job <- replay_job("claude_plain")

  resolved <- NULL
  promises::then(promises::as.promise(job), function(value) resolved <<- value)

  deadline <- Sys.time() + 10
  while (is.null(resolved) && Sys.time() < deadline) later::run_now(0.05)

  llt_expect_true(!is.null(resolved), "the job's promise never resolved")
  llt_expect_s7(resolved, LLMMessage)
})

llt_report("async_chat_replay")

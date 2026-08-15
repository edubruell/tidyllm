# Live tests for send_chat().
#
# The offline suite (async_chat_replay.R) covers the status model against
# recorded fixtures, but the replay server answers instantly, so it cannot show
# the property the feature exists for: that a slow provider yields the session
# back between chunks. That needs a real generation, which is what this file is
# for.
#
#   Rscript -e 'devtools::load_all("."); source("local_tests/features/async_chat_live.R")'
#
# Costs a handful of small requests against claude() and openai().

source("local_tests/test_harness.R")
llt_suite("async_chat_live")

if (!requireNamespace("later", quietly = TRUE)) {
  stop("later is needed for send_chat(): install.packages('later')")
}

LONG <- "Write exactly 200 words about the history of the R language."

llt_test("send_chat returns before the reply is finished", {
  job <- llm_message(LONG) |> send_chat(claude(), .stream = TRUE)

  llt_expect_true(inherits(job, "tidyllm_chat_job"), "send_chat() did not return a job")
  llt_expect_true(identical(check_job(job), "running"),
                  paste0("a 200-word reply was already ", check_job(job),
                         " when send_chat() returned; nothing was asynchronous about it"))

  reply <- fetch_job(job)
  llt_expect_reply(reply)
  llt_expect_true(identical(check_job(job), "done"), "job not marked done after fetch")
})

llt_test("text arrives progressively while the session keeps working", {
  # The claim in one assertion: the caller's own code runs between chunks, and
  # the reply grows while it does. A driver that blocked would show one jump
  # from nothing to everything.
  job <- llm_message(LONG) |> send_chat(claude(), .stream = TRUE)

  lengths <- integer()
  work    <- 0
  while (check_job(job) == "running") {
    work <- work + sum(runif(500))
    lengths <- c(lengths, nchar(get_partial(job)))
  }

  growth <- length(unique(lengths[lengths > 0]))
  llt_expect_true(growth >= 3,
                  sprintf("partial text took %d distinct non-empty values; expected progressive growth",
                          growth))
  llt_expect_true(!is.unsorted(lengths), "partial text shrank at some point")
  llt_expect_true(identical(get_partial(job), get_reply(fetch_job(job))),
                  "the accumulated partial text does not match the final reply")
})

llt_test("two jobs run at the same time", {
  # One event loop, two connections. The assertion is that one job makes
  # progress while the other is still in flight, not that both produce text at
  # once: a reasoning model can spend ten seconds before its first visible
  # token, which is a property of the model and would make that version of the
  # test fail for the wrong reason.
  a <- llm_message(LONG) |> send_chat(claude(), .stream = TRUE)
  b <- llm_message(LONG) |> send_chat(openai(), .stream = TRUE)

  growth_while_other_ran <- 0L
  last <- 0L
  while (check_job(a) == "running" || check_job(b) == "running") {
    if (identical(check_job(b), "running")) {
      now <- nchar(get_partial(a))
      if (now > last) growth_while_other_ran <- growth_while_other_ran + 1L
      last <- now
    }
  }

  llt_expect_true(growth_while_other_ran >= 3,
                  sprintf("one job advanced only %d times while the other was in flight",
                          growth_while_other_ran))
  llt_expect_reply(fetch_job(a))
  llt_expect_reply(fetch_job(b))
})

llt_test(".on_chunk is called as the text arrives, not at the end", {
  deltas <- character()
  job <- llm_message(LONG) |>
    send_chat(claude(), .stream = TRUE, .on_chunk = function(d) deltas <<- c(deltas, d))

  seen_early <- FALSE
  while (check_job(job) == "running") {
    if (length(deltas) > 1) seen_early <- TRUE
  }

  llt_expect_true(seen_early, ".on_chunk produced nothing until the job was finished")
  llt_expect_true(identical(paste0(deltas, collapse = ""), get_reply(fetch_job(job))),
                  "the pushed deltas do not reassemble into the reply")
})

llt_test("a job with tools completes through the shared loop", {
  calls <- 0L
  temp_tool <- tidyllm_tool(
    function(city) {
      calls <<- calls + 1L
      paste0(city, ": ", nchar(city), " degrees")
    },
    "Get the current temperature in a city",
    city = field_chr("City name")
  )

  job <- llm_message("What is the temperature in Berlin? Use the tool.") |>
    send_chat(claude(), .stream = TRUE, .tools = temp_tool)
  reply <- fetch_job(job)

  llt_expect_reply(reply)
  llt_expect_true(calls >= 1, "the async job produced no executed tool call")
})

llt_test("a non-streaming job still returns immediately", {
  job <- llm_message(LONG) |> send_chat(openai(), .stream = FALSE)

  llt_expect_true(identical(check_job(job), "running"),
                  "a non-streaming job blocked until it was finished")
  llt_expect_true(identical(get_partial(job), ""),
                  "a non-streaming job reported partial text")

  reply <- fetch_job(job)
  llt_expect_reply(reply)
  llt_expect_true(isFALSE(get_metadata(reply)$stream),
                  "a non-streaming job reported stream = TRUE")
})

llt_test("cancelling a live job stops it", {
  job <- llm_message(LONG) |> send_chat(claude(), .stream = TRUE)
  later::run_now(0.3)
  cancel_job(job)

  llt_expect_true(identical(check_job(job), "cancelled"),
                  paste0("expected cancelled, got ", check_job(job)))

  # Nothing may resurrect it: the status must survive further turns of the loop.
  later::run_now(0.5)
  llt_expect_true(identical(check_job(job), "cancelled"),
                  "a cancelled job changed status after further event loop turns")
})

llt_test("get_stream delivers deltas while the job is still running", {
  # The offline suite can only show that every delta arrives; the replay server
  # answers instantly, so it cannot show that they arrive *early*. That is the
  # whole point of an async generator for shinychat, so it is asserted here.
  job <- llm_message(LONG) |> send_chat(claude(), .stream = TRUE)

  collected <- character()
  seen_while_running <- FALSE
  consume <- coro::async(function() {
    for (delta in coro::await_each(get_stream(job))) {
      collected <<- c(collected, delta)
      if (identical(check_job(job), "running")) seen_while_running <<- TRUE
    }
  })
  consume()

  deadline <- Sys.time() + 60
  while (identical(check_job(job), "running") && Sys.time() < deadline) later::run_now(0.05)
  for (i in 1:100) later::run_now(0.02)

  llt_expect_true(seen_while_running,
                  "every delta arrived only after the job had finished; the generator is not streaming")
  llt_expect_true(identical(paste0(collected, collapse = ""), get_reply(fetch_job(job))),
                  "the streamed deltas do not reassemble into the reply")
})

llt_report("async_chat_live")

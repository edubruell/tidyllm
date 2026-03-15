# local_tests/batch_fetch.R
# Fetch completed batches and verify results.
# Tests both plain text replies and json_schema structured output.
#
# Prerequisites:
#   1. source("local_tests/batch_submit.R")   — submits jobs
#   2. check_all_async_jobs()                  — wait until all show "completed"
#   3. source("local_tests/batch_fetch.R")    — this file
#
# Each test skips gracefully if the job is still pending.
# On success, calls complete_async_job() to clean up the state file.

devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
source("local_tests/async_tracker.R")

llt_suite("batch_fetch")

# ── Helpers ────────────────────────────────────────────────────────────────────

.assert_plain_fetch <- function(results) {
  llt_expect_true(is.list(results), "fetch_batch should return a list")
  llt_expect_true(length(results) == 2, "Should have 2 results")
  for (nm in names(results)) {
    llt_expect_s7(results[[nm]], LLMMessage)
    llt_expect_reply(results[[nm]])
  }
}

.assert_schema_fetch <- function(results) {
  llt_expect_true(is.list(results), "fetch_batch should return a list")
  llt_expect_true(length(results) == 2, "Should have 2 results")
  for (nm in names(results)) {
    llt_expect_s7(results[[nm]], LLMMessage)
    llt_expect_reply(results[[nm]])
    data <- get_reply_data(results[[nm]])
    llt_expect_true(is.list(data), "get_reply_data() should return a list")
    llt_expect_true("pers_name" %in% names(data), "Should have pers_name field")
    llt_expect_true("age" %in% names(data), "Should have age field")
    llt_expect_true(is.numeric(data$age), "age should be numeric")
  }
}

.skip_if_not_completed <- function(name) {
  jobs <- .load_jobs_index()
  entry <- Filter(function(j) j$name == name, jobs)
  if (length(entry) == 0) {
    cat(sprintf("  [skip] %s — not found in job index\n", name))
    return(TRUE)
  }
  if (entry[[1]]$status != "completed") {
    cat(sprintf("  [skip] %s — status is '%s', not completed\n", name, entry[[1]]$status))
    return(TRUE)
  }
  FALSE
}

# ── Claude ─────────────────────────────────────────────────────────────────────

llt_test("claude: fetch plain batch replies", {
  if (.skip_if_not_completed("claude_plain_batch")) return(invisible(NULL))
  batch <- load_async_job("claude_plain_batch")
  results <- fetch_batch(batch, claude())
  .assert_plain_fetch(results)
  complete_async_job("claude_plain_batch")
})

llt_test("claude: fetch schema batch — get_reply_data() works", {
  if (.skip_if_not_completed("claude_schema_batch")) return(invisible(NULL))
  batch <- load_async_job("claude_schema_batch")
  results <- fetch_batch(batch, claude())
  .assert_schema_fetch(results)
  complete_async_job("claude_schema_batch")
})

# ── OpenAI ─────────────────────────────────────────────────────────────────────

llt_test("openai: fetch plain batch replies", {
  if (.skip_if_not_completed("openai_plain_batch")) return(invisible(NULL))
  batch <- load_async_job("openai_plain_batch")
  results <- fetch_batch(batch, openai())
  .assert_plain_fetch(results)
  complete_async_job("openai_plain_batch")
})

llt_test("openai: fetch schema batch — get_reply_data() works", {
  if (.skip_if_not_completed("openai_schema_batch")) return(invisible(NULL))
  batch <- load_async_job("openai_schema_batch")
  results <- fetch_batch(batch, openai())
  .assert_schema_fetch(results)
  complete_async_job("openai_schema_batch")
})

# ── Gemini ─────────────────────────────────────────────────────────────────────

llt_test("gemini: fetch plain batch replies", {
  if (.skip_if_not_completed("gemini_plain_batch")) return(invisible(NULL))
  batch <- load_async_job("gemini_plain_batch")
  results <- fetch_batch(batch, gemini())
  .assert_plain_fetch(results)
  complete_async_job("gemini_plain_batch")
})

llt_test("gemini: fetch schema batch — get_reply_data() works", {
  if (.skip_if_not_completed("gemini_schema_batch")) return(invisible(NULL))
  batch <- load_async_job("gemini_schema_batch")
  results <- fetch_batch(batch, gemini())
  .assert_schema_fetch(results)
  complete_async_job("gemini_schema_batch")
})

# ── Mistral ────────────────────────────────────────────────────────────────────

llt_test("mistral: fetch plain batch replies", {
  if (.skip_if_not_completed("mistral_plain_batch")) return(invisible(NULL))
  batch <- load_async_job("mistral_plain_batch")
  results <- fetch_batch(batch, mistral())
  .assert_plain_fetch(results)
  complete_async_job("mistral_plain_batch")
})

llt_test("mistral: fetch schema batch — get_reply_data() works", {
  if (.skip_if_not_completed("mistral_schema_batch")) return(invisible(NULL))
  batch <- load_async_job("mistral_schema_batch")
  results <- fetch_batch(batch, mistral())
  .assert_schema_fetch(results)
  complete_async_job("mistral_schema_batch")
})

# ── Groq ───────────────────────────────────────────────────────────────────────

llt_test("groq: fetch plain batch replies", {
  if (.skip_if_not_completed("groq_plain_batch")) return(invisible(NULL))
  batch <- load_async_job("groq_plain_batch")
  results <- fetch_batch(batch, groq())
  .assert_plain_fetch(results)
  complete_async_job("groq_plain_batch")
})

llt_test("groq: fetch schema batch — get_reply_data() works", {
  if (.skip_if_not_completed("groq_schema_batch")) return(invisible(NULL))
  batch <- load_async_job("groq_schema_batch")
  results <- fetch_batch(batch, groq())
  .assert_schema_fetch(results)
  complete_async_job("groq_schema_batch")
})

llt_report()

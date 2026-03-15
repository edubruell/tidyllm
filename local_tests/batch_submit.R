# local_tests/batch_submit.R
# Submit plain and json_schema batches for all batch-capable providers.
# Only submits if no pending job exists under that name — safe to re-run.
#
# Usage:
#   source("local_tests/batch_submit.R")
#
# After running, wait for batches to complete, then check status with:
#   source("local_tests/async_tracker.R"); check_all_async_jobs()
#
# Then run fetch tests with:
#   source("local_tests/batch_fetch.R")

devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
source("local_tests/async_tracker.R")

llt_suite("batch_submit")

# ── Shared fixtures ────────────────────────────────────────────────────────────

.plain_msgs <- list(
  plain_1 = llm_message("What is the capital of France? Answer in one word."),
  plain_2 = llm_message("What is 7 multiplied by 8? Answer with just the number.")
)

.schema_msgs <- list(
  schema_1 = llm_message('Extract the person info: "Alice Smith is 34 years old."'),
  schema_2 = llm_message('Extract the person info: "Bob Jones is 52 years old."')
)

.person_schema <- tidyllm_schema(
  pers_name = field_chr("The person's full name"),
  age       = field_dbl("The person's age as a number")
)

# ── Claude ─────────────────────────────────────────────────────────────────────

llt_test("claude: submit plain batch", {
  if (job_is_pending("claude_plain_batch")) {
    cat("  [skip] job already pending\n"); return(invisible(NULL))
  }
  batch <- .plain_msgs |> send_batch(claude())
  llt_expect_true(!is.null(attr(batch, "batch_id")), "Should have batch_id")
  save_async_job(batch, "claude_plain_batch", "claude")
})

llt_test("claude: submit schema batch", {
  if (job_is_pending("claude_schema_batch")) {
    cat("  [skip] job already pending\n"); return(invisible(NULL))
  }
  batch <- .schema_msgs |> send_batch(claude(.json_schema = .person_schema))
  llt_expect_true(!is.null(attr(batch, "batch_id")), "Should have batch_id")
  llt_expect_true(isTRUE(attr(batch, "json")), "json attr should be TRUE")
  save_async_job(batch, "claude_schema_batch", "claude")
})

# ── OpenAI ─────────────────────────────────────────────────────────────────────

llt_test("openai: submit plain batch", {
  if (job_is_pending("openai_plain_batch")) {
    cat("  [skip] job already pending\n"); return(invisible(NULL))
  }
  batch <- .plain_msgs |> send_batch(openai())
  llt_expect_true(!is.null(attr(batch, "batch_id")), "Should have batch_id")
  save_async_job(batch, "openai_plain_batch", "openai")
})

llt_test("openai: submit schema batch", {
  if (job_is_pending("openai_schema_batch")) {
    cat("  [skip] job already pending\n"); return(invisible(NULL))
  }
  batch <- .schema_msgs |> send_batch(openai(.json_schema = .person_schema))
  llt_expect_true(!is.null(attr(batch, "batch_id")), "Should have batch_id")
  llt_expect_true(isTRUE(attr(batch, "json")), "json attr should be TRUE")
  save_async_job(batch, "openai_schema_batch", "openai")
})

# ── Gemini ─────────────────────────────────────────────────────────────────────

llt_test("gemini: submit plain batch", {
  if (job_is_pending("gemini_plain_batch")) {
    cat("  [skip] job already pending\n"); return(invisible(NULL))
  }
  batch <- .plain_msgs |> send_batch(gemini())
  llt_expect_true(!is.null(attr(batch, "batch_id")), "Should have batch_id")
  save_async_job(batch, "gemini_plain_batch", "gemini")
})

llt_test("gemini: submit schema batch", {
  if (job_is_pending("gemini_schema_batch")) {
    cat("  [skip] job already pending\n"); return(invisible(NULL))
  }
  batch <- .schema_msgs |> send_batch(gemini(.json_schema = .person_schema))
  llt_expect_true(!is.null(attr(batch, "batch_id")), "Should have batch_id")
  llt_expect_true(isTRUE(attr(batch, "json")), "json attr should be TRUE")
  save_async_job(batch, "gemini_schema_batch", "gemini")
})

# ── Mistral ────────────────────────────────────────────────────────────────────

llt_test("mistral: submit plain batch", {
  if (job_is_pending("mistral_plain_batch")) {
    cat("  [skip] job already pending\n"); return(invisible(NULL))
  }
  batch <- .plain_msgs |> send_batch(mistral())
  llt_expect_true(!is.null(attr(batch, "batch_id")), "Should have batch_id")
  save_async_job(batch, "mistral_plain_batch", "mistral")
})

llt_test("mistral: submit schema batch", {
  if (job_is_pending("mistral_schema_batch")) {
    cat("  [skip] job already pending\n"); return(invisible(NULL))
  }
  batch <- .schema_msgs |> send_batch(mistral(.json_schema = .person_schema))
  llt_expect_true(!is.null(attr(batch, "batch_id")), "Should have batch_id")
  llt_expect_true(isTRUE(attr(batch, "json")), "json attr should be TRUE")
  save_async_job(batch, "mistral_schema_batch", "mistral")
})

# ── Groq ───────────────────────────────────────────────────────────────────────

llt_test("groq: submit plain batch", {
  if (job_is_pending("groq_plain_batch")) {
    cat("  [skip] job already pending\n"); return(invisible(NULL))
  }
  batch <- .plain_msgs |> send_batch(groq())
  llt_expect_true(!is.null(attr(batch, "batch_id")), "Should have batch_id")
  save_async_job(batch, "groq_plain_batch", "groq")
})

llt_test("groq: submit schema batch", {
  if (job_is_pending("groq_schema_batch")) {
    cat("  [skip] job already pending\n"); return(invisible(NULL))
  }
  batch <- .schema_msgs |> send_batch(groq(.json_schema = .person_schema))
  llt_expect_true(!is.null(attr(batch, "batch_id")), "Should have batch_id")
  llt_expect_true(isTRUE(attr(batch, "json")), "json attr should be TRUE")
  save_async_job(batch, "groq_schema_batch", "groq")
})

llt_report()

cat("\nBatches submitted. Check status with:\n")
cat("  source(\"local_tests/async_tracker.R\"); check_all_async_jobs()\n")
cat("Then fetch with:\n")
cat("  source(\"local_tests/batch_fetch.R\")\n\n")

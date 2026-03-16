# local_tests/features/perplexity_deep_research.R
# Tests for Perplexity deep_research() — blocking and background modes.
#
# What this tests:
#   - deep_research() blocking: returns LLMMessage with reply and citations
#   - deep_research() background: returns tidyllm_research_job, check_job() and fetch_job() work
#
# WARNING: These tests are slow (5–10 minutes). Run manually, not as part of the fast suite.

devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("perplexity_deep_research")

# ── Blocking mode ─────────────────────────────────────────────────────────────

llt_test("deep_research blocking returns LLMMessage with reply", {
  result <- llm_message("What are the main causes of World War I?") |>
    deep_research(perplexity(), .timeout = 600)
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
  meta <- get_metadata(result)
  llt_expect_true(
    !is.null(meta$api_specific[[1]]$citations) && length(meta$api_specific[[1]]$citations) > 0,
    "deep_research reply should have citations"
  )
})

# ── Background mode ───────────────────────────────────────────────────────────

llt_test("deep_research background mode returns research job, check and fetch work", {
  job <- llm_message("Briefly explain quantum entanglement.") |>
    deep_research(perplexity(), .background = TRUE)
  llt_expect_true(inherits(job, "tidyllm_research_job"), "Should be a tidyllm_research_job")
  llt_expect_true(nzchar(job$job_id), "job_id should be non-empty")

  Sys.sleep(10)
  checked <- check_job(job)
  llt_expect_true(inherits(checked, "tidyllm_research_job"), "check_job should return research job")

  if (identical(checked$status, "completed")) {
    result <- fetch_job(checked)
    llt_expect_s7(result, LLMMessage)
    llt_expect_reply(result)
  } else {
    message("Job still pending after 10s — check status manually with check_job()/fetch_job()")
  }
})

# ── Structured output ─────────────────────────────────────────────────────────

llt_test("deep_research with json_schema returns structured data", {
  schema <- tidyllm_schema(
    name = "Causes",
    main_cause  = field_chr("The single most cited main cause"),
    other_causes = field_chr("Two or three other key causes, comma-separated")
  )
  result <- llm_message("What were the main causes of World War I?") |>
    deep_research(perplexity(), .json_schema = schema, .timeout = 600)
  llt_expect_s7(result, LLMMessage)
  data <- get_reply_data(result)
  llt_expect_true(is.list(data), "get_reply_data() should return a list")
  llt_expect_true("main_cause" %in% names(data), "Should have main_cause field")
})

llt_report()

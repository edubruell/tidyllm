devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("openai_deep_research")

# Slow tests (~5-30 min each) — run deliberately, not in the default suite.
# Usage: source("local_tests/features/openai_deep_research.R")

# ── Background mode + check_job + fetch_job ───────────────────────────────────

llt_test("deep_research background mode submits job and check_job tracks status", {
  job <- llm_message("What open-source R packages are most commonly used for Bayesian statistics?") |>
    deep_research(openai(.model = "o4-mini-deep-research"), .background = TRUE)

  llt_expect_true(inherits(job, "tidyllm_research_job"), "Should return tidyllm_research_job")
  llt_expect_true(nzchar(job$job_id), "job_id should be non-empty")
  llt_expect_true(identical(job$provider, "openai"), "provider should be 'openai'")
  llt_expect_true(S7_inherits(job$message, LLMMessage), "message should be LLMMessage")

  checked <- check_job(job)
  llt_expect_true(checked$status %in% c("queued", "in_progress", "completed"),
                  "status should be a known value")
})

# ── Blocking mode ─────────────────────────────────────────────────────────────

llt_test("deep_research blocking mode returns LLMMessage with reply (5-30 min)", {
  result <- llm_message(
    "Summarise in 3 bullet points what the R programming language is used for in academia."
  ) |> deep_research(openai(.model = "o4-mini-deep-research"), .timeout = 1800)

  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
  meta <- get_metadata(result)
  llt_expect_true(!is.na(meta$completion_tokens[1]), "Should have completion token count")
})

# ── JSON schema structured output ─────────────────────────────────────────────

llt_test("deep_research with json_schema returns structured output", {
  schema <- tidyllm_schema(
    name    = "ResearchSummary",
    summary = field_chr("2-sentence summary of findings"),
    sources = "character[]"
  )
  result <- llm_message(
    "What are the top 3 R packages for machine learning? Give a brief summary and list sources."
  ) |> deep_research(
    openai(.model = "o4-mini-deep-research", .json_schema = schema),
    .timeout = 1800
  )

  llt_expect_s7(result, LLMMessage)
  data <- get_reply_data(result)
  llt_expect_true(is.list(data), "get_reply_data should return list")
  llt_expect_true("summary" %in% names(data), "Should have summary field")
})

# ── Background + manual poll + fetch ─────────────────────────────────────────

llt_test("full background workflow: submit, poll, fetch", {
  job <- llm_message(
    "Name the 3 most cited statistical papers from 2020-2024 in one sentence each."
  ) |> deep_research(openai(.model = "o4-mini-deep-research"), .background = TRUE)

  llt_expect_true(inherits(job, "tidyllm_research_job"), "Job created")

  # Poll until completed (max 30 min)
  deadline <- proc.time()[["elapsed"]] + 1800
  repeat {
    job <- check_job(job)
    if (identical(job$status, "completed")) break
    if (proc.time()[["elapsed"]] > deadline) stop("Timed out waiting for job")
    message("Status: ", job$status, " — waiting 30s...")
    Sys.sleep(30)
  }
  llt_expect_true(identical(job$status, "completed"), "Job should be completed")

  result <- fetch_job(job)
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

llt_report()

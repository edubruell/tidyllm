devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("perplexity")

# This account has no Perplexity credits (Perplexity enforces a high minimum API
# spend), so every live call returns insufficient_quota. Bail out with a clear
# message instead of reporting a wall of failures that are not tidyllm defects.
probe <- tryCatch({
  llm_message("hi") |> chat(perplexity(.max_tokens = 8))
  NULL
}, error = function(e) conditionMessage(e))

perplexity_available <- is.null(probe) ||
  !grepl("quota|billing|credit", probe, ignore.case = TRUE)

if (!perplexity_available) {
  cat("  ! skipped: the Perplexity account has no quota\n    ", probe, "\n", sep = "")
}

if (perplexity_available) {

# ── Basic chat ────────────────────────────────────────────────────────────────

llt_test("basic chat returns LLMMessage", {
  result <- llm_message("What is the Eiffel Tower?") |> chat(perplexity())
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

llt_test("get_metadata returns token counts", {
  result <- llm_message("Hello") |> chat(perplexity())
  llt_expect_metadata(result, c("prompt_tokens", "completion_tokens"))
})

llt_test("streaming returns non-empty reply", {
  result <- llm_message("Count to 3.") |> chat(perplexity(.stream = TRUE))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

# ── Citations ─────────────────────────────────────────────────────────────────

llt_test("api_specific contains citations for factual query", {
  result <- llm_message("What year was the Eiffel Tower built?") |>
    chat(perplexity())
  meta <- get_metadata(result)
  llt_expect_true(
    !is.null(meta$api_specific[[1]]$citations),
    "api_specific should have citations field"
  )
  llt_expect_true(
    length(meta$api_specific[[1]]$citations) > 0,
    "Should have at least one citation URL"
  )
  llt_expect_true(
    is.character(unlist(meta$api_specific[[1]]$citations)),
    "Citations should be character strings (URLs)"
  )
})

llt_test("streaming api_specific contains citations", {
  result <- llm_message("Who invented the telephone?") |>
    chat(perplexity(.stream = TRUE))
  meta <- get_metadata(result)
  llt_expect_true(
    !is.null(meta$api_specific[[1]]$citations),
    "Streaming api_specific should have citations field"
  )
})

llt_test("search_recency_filter accepted without error", {
  result <- llm_message("Any recent AI news?") |>
    chat(perplexity(.search_recency_filter = "week"))
  llt_expect_reply(result)
})

# ── Structured output ─────────────────────────────────────────────────────────

llt_test("json_schema returns structured data", {
  schema <- tidyllm_schema(
    name = "Inventor",
    inventor = field_chr("Name of the inventor"),
    year      = field_dbl("Year of the invention")
  )
  result <- llm_message("Who invented the telephone and in what year?") |>
    chat(perplexity(), .json_schema = schema)
  data <- get_reply_data(result)
  llt_expect_true(is.list(data), "get_reply_data() should return a list")
  llt_expect_true("inventor" %in% names(data), "Should have inventor field")
  llt_expect_true("year" %in% names(data), "Should have year field")
})

}

if (perplexity_available) llt_report()

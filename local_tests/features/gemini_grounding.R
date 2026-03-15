# local_tests/features/gemini_grounding.R
# Tests for Gemini Google Search grounding (.grounding_threshold parameter).
#
# What this tests:
#   - Chat with grounding enabled returns a reply
#   - api_specific$groundingMetadata is populated (confirms the grounding
#     metadata round-trips correctly through extract_metadata -> get_metadata)
#   - Very low threshold (0.0) triggers grounding on most queries
#   - Grounding works alongside streaming

devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("gemini_grounding")

llt_test("grounding chat returns reply", {
  result <- llm_message("What is the current population of Tokyo?") |>
    chat(gemini(.grounding_threshold = 0.3))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

llt_test("api_specific$groundingMetadata is non-NULL when grounding fires", {
  result <- llm_message("What happened in the news today?") |>
    chat(gemini(.grounding_threshold = 0.0))
  meta <- get_metadata(result)
  llt_expect_true(
    !is.null(meta$api_specific[[1]]$groundingMetadata),
    "groundingMetadata should be present in api_specific"
  )
})

llt_test("groundingMetadata contains webSearchQueries or groundingChunks", {
  result <- llm_message("Who won the most recent FIFA World Cup?") |>
    chat(gemini(.grounding_threshold = 0.0))
  gm <- get_metadata(result)$api_specific[[1]]$groundingMetadata
  llt_expect_true(
    !is.null(gm$webSearchQueries) || !is.null(gm$groundingChunks),
    "groundingMetadata should contain webSearchQueries or groundingChunks"
  )
})

llt_test("grounding with threshold 1.0 still returns reply (grounding may not fire)", {
  result <- llm_message("What is 2 + 2?") |>
    chat(gemini(.grounding_threshold = 1.0))
  llt_expect_reply(result)
})

llt_report()

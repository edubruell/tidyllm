devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("openrouter")

# Default chat model: anthropic/claude-sonnet-4-6
# Free model for some tests: meta-llama/llama-3.3-70b-instruct:free

# ── list_models ───────────────────────────────────────────────────────────────

llt_test("list_models returns tibble with expected columns", {
  models <- list_models(openrouter())
  llt_expect_true(tibble::is_tibble(models), "list_models() should return a tibble")
  llt_expect_true(nrow(models) > 100, "Should return many models")
  for (col in c("id", "name", "context_length", "prompt_price_per_million", "completion_price_per_million")) {
    llt_expect_true(col %in% names(models), paste0("Should have '", col, "' column"))
  }
})

llt_test("free models appear in list_models", {
  models <- list_models(openrouter())
  free <- models[models$prompt_price_per_million == 0 & !is.na(models$prompt_price_per_million), ]
  llt_expect_true(nrow(free) > 0, "Should have at least one free model")
})

# ── Basic chat ────────────────────────────────────────────────────────────────

llt_test("basic chat returns LLMMessage", {
  result <- llm_message("Say hello in one word.") |>
    chat(openrouter(.model = "google/gemini-2.5-flash"))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

llt_test("get_metadata returns token counts and model", {
  result <- llm_message("Hello") |>
    chat(openrouter(.model = "google/gemini-2.5-flash"))
  llt_expect_metadata(result, c("prompt_tokens", "completion_tokens", "model"))
  meta <- get_metadata(result)
  llt_expect_true(meta$model == "google/gemini-2.5-flash", "model field should match requested model")
})

llt_test("streaming returns non-empty reply", {
  result <- llm_message("Count to 3.") |>
    chat(openrouter(.model = "google/gemini-2.5-flash", .stream = TRUE))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

# ── Multi-turn ────────────────────────────────────────────────────────────────

llt_test("multi-turn conversation works", {
  result <- llm_message("My favourite colour is blue. Just say 'got it'.") |>
    chat(openrouter(.model = "google/gemini-2.5-flash")) |>
    llm_message("What is my favourite colour?") |>
    chat(openrouter(.model = "google/gemini-2.5-flash"))
  llt_expect_reply(result)
  reply <- get_reply(result)
  llt_expect_true(grepl("blue", reply, ignore.case = TRUE), "Should remember favourite colour")
})

# ── Provider routing ──────────────────────────────────────────────────────────

llt_test("fallback routing with .route and .models works", {
  result <- llm_message("Say hello in one word.") |>
    chat(openrouter(
      .model  = "google/gemini-2.5-flash",
      .route  = "fallback",
      .models = c("google/gemini-2.5-flash", "anthropic/claude-haiku-4-5")
    ))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

# ── Tool use ──────────────────────────────────────────────────────────────────

llt_test("single tool call returns reply", {
  get_weather <- function(location) paste0("Weather in ", location, ": Sunny, 22C")
  weather_tool <- tidyllm_tool(get_weather, "Get weather for a location",
                               location = field_chr("City name"))
  result <- llm_message("What's the weather in Paris?") |>
    chat(openrouter(.model = "google/gemini-2.5-flash"), .tools = weather_tool)
  llt_expect_reply(result)
})

llt_report()

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

# ── Reasoning ─────────────────────────────────────────────────────────────────

llt_test("reasoning with effort level returns reply and reasoning in metadata", {
  result <- llm_message("Is 9.11 or 9.9 larger? Think carefully.") |>
    chat(openrouter(
      .model     = "anthropic/claude-sonnet-4-6",
      .reasoning = list(effort = "low")
    ))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
  meta <- get_metadata(result)
  llt_expect_true(!is.null(meta$api_specific[[1]]$reasoning_tokens) ||
                  !is.null(meta$api_specific[[1]]$reasoning),
                  "Should have reasoning tokens or text in metadata")
})

llt_test("reasoning with exclude=TRUE is forwarded and still returns an answer", {
  # Checked 2026-07-30: tidyllm forwards reasoning$exclude verbatim, but upstream
  # Anthropic routes now return a short reasoning summary anyway. Assert what
  # tidyllm controls (the request body and a usable reply), not the upstream policy.
  dry <- llm_message("Is 9.11 or 9.9 larger?") |>
    chat(openrouter(
      .model     = "anthropic/claude-sonnet-4-6",
      .reasoning = list(effort = "low", exclude = TRUE),
      .dry_run   = TRUE
    ))
  llt_expect_true(isTRUE(dry$body$data$reasoning$exclude),
                  "reasoning$exclude must reach the request body")

  result <- llm_message("Is 9.11 or 9.9 larger?") |>
    chat(openrouter(
      .model     = "anthropic/claude-sonnet-4-6",
      .reasoning = list(effort = "low", exclude = TRUE)
    ))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

llt_test("multi-turn with reasoning passes reasoning back correctly", {
  first <- llm_message("My favourite colour is blue. Just say 'got it'.") |>
    chat(openrouter(
      .model     = "anthropic/claude-sonnet-4-6",
      .reasoning = list(effort = "low")
    ))
  result <- first |>
    llm_message("What is my favourite colour?") |>
    chat(openrouter(
      .model     = "anthropic/claude-sonnet-4-6",
      .reasoning = list(effort = "low")
    ))
  llt_expect_reply(result)
  reply <- get_reply(result)
  llt_expect_true(grepl("blue", reply, ignore.case = TRUE), "Should remember favourite colour across reasoning turns")
})

# ── Credits ───────────────────────────────────────────────────────────────────

llt_test("openrouter_credits returns balance info", {
  credits <- openrouter_credits()
  llt_expect_true(is.list(credits), "Should return a list")
  llt_expect_true(!is.null(credits$total_credits), "Should have total_credits")
  llt_expect_true(!is.null(credits$remaining), "Should have remaining")
  llt_expect_true(is.numeric(credits$remaining), "remaining should be numeric")
})

# ── Generation details ────────────────────────────────────────────────────────

llt_test("openrouter_generation returns cost and provider", {
  result <- llm_message("Say hi.") |>
    chat(openrouter(.model = "google/gemini-2.5-flash"))
  gen_id <- get_metadata(result)$api_specific[[1]]$id
  llt_expect_true(nzchar(gen_id), "Generation ID should be non-empty")
  # generation stats are eventually consistent; a 404 right after the call is normal
  gen <- NULL
  for (attempt in 1:12) {
    Sys.sleep(15)
    gen <- tryCatch(openrouter_generation(gen_id), error = function(e) NULL)
    if (!is.null(gen)) break
  }
  llt_expect_true(!is.null(gen), "Generation stats never became available")
  llt_expect_true(!is.null(gen$total_cost), "Should have total_cost")
  llt_expect_true(is.numeric(gen$total_cost), "total_cost should be numeric")
  llt_expect_true(!is.null(gen$provider), "Should have provider")
})

# ── Embeddings ────────────────────────────────────────────────────────────────

llt_test("embed returns correct dimensions", {
  result <- embed(c("Hello world", "Goodbye world"),
                  openrouter(.model = "openai/text-embedding-3-small"))
  llt_expect_true(tibble::is_tibble(result), "embed() should return a tibble")
  llt_expect_true(nrow(result) == 2, "Should have 2 rows")
  llt_expect_true(length(result$embeddings[[1]]) == 1536, "text-embedding-3-small produces 1536-dim vectors")
})


# -- Streaming with tools (0.6.0) ---------------------------------------------

llt_test("streamed tool use assembles and completes", {
  # The stream has to be folded back into a response body before the tool loop
  # can read it, and the follow-up round then streams too. Both halves are
  # asserted without reading the model's prose: a counter proves the tool really
  # ran, and a non-empty reply proves the follow-up streamed round completed.
  # Matching the tool's answer in the text instead would be model-dependent:
  # models paraphrase and convert units, and a bare number also matches a token
  # count or a plausible hallucination.
  calls <- 0L
  temp_tool <- tidyllm_tool(
    function(city) {
      calls <<- calls + 1L
      paste0(city, ": ", nchar(city), " degrees")
    },
    "Get the current temperature in a city",
    city = field_chr("City name")
  )
  result <- llm_message("What is the temperature in Berlin and in Reykjavik? Use the tool for both.") |>
    chat(openrouter(), .tools = temp_tool, .stream = TRUE)

  llt_expect_reply(result)
  llt_expect_true(calls >= 1,
                  "the streamed response produced no executed tool call")
})

llt_report()

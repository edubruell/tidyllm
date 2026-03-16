devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("gemini")

# ── Basic chat ────────────────────────────────────────────────────────────────

llt_test("basic chat returns LLMMessage", {
  result <- llm_message("Say hello in one word.") |> chat(gemini())
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

llt_test("get_metadata returns token counts", {
  result <- llm_message("Hello") |> chat(gemini())
  llt_expect_metadata(result, c("prompt_tokens", "completion_tokens"))
})

llt_test("streaming returns non-empty reply", {
  result <- llm_message("Count to 3.") |> chat(gemini(.stream = TRUE))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

# ── Structured output ─────────────────────────────────────────────────────────

llt_test("structured output returns valid data", {
  schema <- tidyllm_schema(
    name = "Person",
    pers_name = field_chr("Person's name"),
    age        = field_dbl("Person's age")
  )
  result <- llm_message("Extract: Jane Doe is 28 years old.") |>
    chat(gemini(), .json_schema = schema)
  data <- get_reply_data(result)
  llt_expect_true(is.list(data), "get_reply_data() should return a list")
  llt_expect_true("pers_name" %in% names(data), "Should have pers_name field")
})

# ── Embeddings ────────────────────────────────────────────────────────────────

llt_test("embed returns tibble with embeddings", {
  result <- c("Hello world", "Goodbye world") |> embed(gemini())
  llt_expect_tibble(result, min_rows = 2)
  llt_expect_true("embeddings" %in% names(result), "Should have embeddings column")
})

llt_test("list_models returns tibble", {
  result <- list_models(gemini())
  llt_expect_tibble(result, min_rows = 5)
})

# ── Tool use ──────────────────────────────────────────────────────────────────

llt_test("single tool call returns reply", {
  get_weather <- function(location) paste0("Weather in ", location, ": Sunny, 22C")
  weather_tool <- tidyllm_tool(get_weather, "Get weather for a location",
                               location = field_chr("City name"))
  result <- llm_message("What's the weather in Paris?") |>
    chat(gemini(), .tools = weather_tool)
  llt_expect_reply(result)
})

llt_test("multi-turn tool use completes", {
  get_city_info <- function(city) {
    info <- list(
      Zathora   = list(country = "Elyndor", population = "412,000"),
      Brimvault = list(country = "Korvel",  population = "1.8 million")
    )
    if (city %in% names(info)) jsonlite::toJSON(info[[city]], auto_unbox = TRUE)
    else "City not found"
  }
  get_capital <- function(country) {
    caps <- list(Elyndor = "Zathora", Korvel = "Brimvault")
    caps[[country]] %||% "Capital not found"
  }
  city_tool    <- tidyllm_tool(get_city_info, "Get city info", city = field_chr("City name"))
  capital_tool <- tidyllm_tool(get_capital, "Get capital", country = field_chr("Country name"))

  result <- llm_message(
    "First, look up the city Zathora. Then find the capital of its country.",
    .system = "Always use the provided tools."
  ) |> chat(gemini(), .tools = list(city_tool, capital_tool), .max_tool_rounds = 5)
  llt_expect_reply(result)
})

llt_test("parallel tool calls work", {
  get_weather <- function(city) {
    w <- list(Paris = "18C cloudy", London = "12C rainy", Tokyo = "25C sunny")
    w[[city]] %||% "Unknown"
  }
  weather_tool <- tidyllm_tool(get_weather, "Get weather", city = field_chr("City name"))
  result <- llm_message("Weather in Paris, London, and Tokyo?") |>
    chat(gemini(), .tools = weather_tool)
  llt_expect_reply(result)
})

llt_test("max_tool_rounds raises error", {
  counter_tool <- tidyllm_tool(
    function(n) paste0("Counter: ", n, ". Call again with ", n + 1),
    "Increment counter — always call again",
    n = field_dbl("Counter value")
  )
  llt_expect_no_error(
    tryCatch(
      llm_message("Start counting from 1, keep going!") |>
        chat(gemini(), .tools = counter_tool, .max_tool_rounds = 2),
      error = function(e) {
        llt_expect_true(grepl("tool round", tolower(e$message)),
                        paste("Unexpected error message:", e$message))
      }
    )
  )
})

# ── Thinking mode ─────────────────────────────────────────────────────────────

llt_test("thinking mode returns thinking_tokens in metadata", {
  result <- llm_message("What is 17 * 23? Think step by step.") |>
    chat(gemini(.model = "gemini-2.5-flash", .thinking_budget = 1024))
  llt_expect_reply(result)
  meta <- get_metadata(result)
  llt_expect_true(
    !is.null(meta$api_specific[[1]]$thinking_tokens) && meta$api_specific[[1]]$thinking_tokens > 0,
    "api_specific$thinking_tokens should be > 0"
  )
})

llt_report()

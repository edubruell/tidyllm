devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("ollama")

# ── Basic chat ────────────────────────────────────────────────────────────────

llt_test("basic chat returns LLMMessage", {
  result <- llm_message("Say hello in one word.") |> chat(ollama())
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

llt_test("get_metadata returns token counts", {
  result <- llm_message("Hello") |> chat(ollama())
  llt_expect_metadata(result, c("prompt_tokens", "completion_tokens"))
})

llt_test("streaming returns non-empty reply", {
  result <- llm_message("Count to 3.") |> chat(ollama(.stream = TRUE))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

# ── Embeddings ────────────────────────────────────────────────────────────────

llt_test("embed returns tibble with embeddings", {
  result <- c("Hello world", "Goodbye world") |> embed(ollama())
  llt_expect_tibble(result, min_rows = 2)
  llt_expect_true("embeddings" %in% names(result), "Should have embeddings column")
})

# ── Structured output ─────────────────────────────────────────────────────────

llt_test("structured output returns valid data", {
  schema <- tidyllm_schema(
    name = "Person",
    pers_name = field_chr("Person's name"),
    age        = field_dbl("Person's age")
  )
  result <- llm_message("Extract: Jane Doe is 28 years old.") |>
    chat(ollama(), .json_schema = schema)
  data <- get_reply_data(result)
  llt_expect_true(is.list(data), "get_reply_data() should return a list")
  llt_expect_true("pers_name" %in% names(data), "Should have pers_name field")
})

# ── Tool use ──────────────────────────────────────────────────────────────────

llt_test("single tool call returns reply", {
  get_weather <- function(location) paste0("Weather in ", location, ": Sunny, 22C")
  weather_tool <- tidyllm_tool(get_weather, "Get weather for a location",
                               location = field_chr("City name"))
  result <- llm_message("What's the weather in Paris?") |>
    chat(ollama(), .tools = weather_tool)
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
    "Look up Zathora, then find the capital of its country.",
    .system = "Always use the provided tools."
  ) |> chat(ollama(), .tools = list(city_tool, capital_tool), .max_tool_rounds = 5)
  llt_expect_reply(result)
})

llt_test("parallel tool calls work", {
  get_weather <- function(city) {
    w <- list(Paris = "18C cloudy", London = "12C rainy", Tokyo = "25C sunny")
    w[[city]] %||% "Unknown"
  }
  weather_tool <- tidyllm_tool(get_weather, "Get weather", city = field_chr("City name"))
  result <- llm_message("Weather in Paris, London, and Tokyo?") |>
    chat(ollama(), .tools = weather_tool)
  llt_expect_reply(result)
})

llt_test("calculator tool works", {
  calculate <- function(expression) {
    tryCatch(as.character(eval(parse(text = expression))),
             error = function(e) paste("Error:", e$message))
  }
  calc_tool <- tidyllm_tool(calculate, "Evaluate a mathematical expression",
                             expression = field_chr("Math expression, e.g. '2 + 2'"))
  result <- llm_message("What is 15 * 7 + 23?") |>
    chat(ollama(), .tools = calc_tool)
  llt_expect_reply(result)
})

llt_report()

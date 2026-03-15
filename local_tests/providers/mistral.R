devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("mistral")

# ── Basic chat ────────────────────────────────────────────────────────────────

llt_test("basic chat returns LLMMessage", {
  result <- llm_message("Say hello in one word.") |> chat(mistral())
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

llt_test("get_metadata returns token counts", {
  result <- llm_message("Hello") |> chat(mistral())
  llt_expect_metadata(result, c("prompt_tokens", "completion_tokens"))
})

llt_test("streaming returns non-empty reply", {
  result <- llm_message("Count to 3.") |> chat(mistral(.stream = TRUE))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

# ── Embeddings ────────────────────────────────────────────────────────────────

llt_test("embed returns tibble with embeddings", {
  result <- c("Hello world", "Goodbye world") |> embed(mistral())
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
    chat(mistral(), .json_schema = schema)
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
    chat(mistral(), .tools = weather_tool)
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
  ) |> chat(mistral(), .tools = list(city_tool, capital_tool), .max_tool_rounds = 5)
  llt_expect_reply(result)
})

llt_test("enum field reaches model in tool schema", {
  classify <- function(text, sentiment) sentiment
  sentiment_tool <- tidyllm_tool(
    classify,
    "Classify the sentiment of text as positive, negative, or neutral. Always call this tool.",
    text      = field_chr("The text to classify"),
    sentiment = field_fct("Sentiment", .levels = c("positive", "negative", "neutral"))
  )
  result <- llm_message('Classify: "I love this!"') |>
    chat(mistral(), .tools = sentiment_tool)
  llt_expect_reply(result)
})

llt_report()

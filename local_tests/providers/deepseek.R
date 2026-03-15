devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("deepseek")

# ── Basic chat ────────────────────────────────────────────────────────────────

llt_test("basic chat returns LLMMessage", {
  result <- llm_message("Say hello in one word.") |> chat(deepseek())
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

llt_test("get_metadata returns token counts", {
  result <- llm_message("Hello") |> chat(deepseek())
  llt_expect_metadata(result, c("prompt_tokens", "completion_tokens"))
})

llt_test("streaming returns non-empty reply", {
  result <- llm_message("Count to 3.") |> chat(deepseek(.stream = TRUE))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

# ── Tool use ──────────────────────────────────────────────────────────────────

llt_test("single tool call returns reply", {
  get_weather <- function(location) paste0("Weather in ", location, ": Sunny, 22C")
  weather_tool <- tidyllm_tool(get_weather, "Get weather for a location",
                               location = field_chr("City name"))
  result <- llm_message("What's the weather in Paris?") |>
    chat(deepseek(), .tools = weather_tool)
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
  ) |> chat(deepseek(), .tools = list(city_tool, capital_tool), .max_tool_rounds = 5)
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
    chat(deepseek(), .tools = sentiment_tool)
  llt_expect_reply(result)
})

# ── Reasoning model ───────────────────────────────────────────────────────────

llt_test("deepseek-reasoner returns reply", {
  result <- llm_message("What is 17 * 23?") |>
    chat(deepseek(.model = "deepseek-reasoner"))
  llt_expect_reply(result)
})

llt_test("thinking mode returns reasoning trace in metadata", {
  result <- llm_message("What is 17 * 23? Show your reasoning.") |>
    chat(deepseek(.thinking = TRUE))
  llt_expect_reply(result)
  meta <- get_metadata(result)
  llt_expect_true(
    !is.null(meta$api_specific[[1]]$thinking) && nzchar(meta$api_specific[[1]]$thinking),
    "api_specific$thinking should be non-empty"
  )
})

llt_report()

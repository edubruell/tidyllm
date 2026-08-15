devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("claude")

# ── Basic chat ────────────────────────────────────────────────────────────────

llt_test("basic chat returns LLMMessage", {
  result <- llm_message("Say hello in one word.") |> chat(claude())
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

llt_test("get_metadata returns token counts", {
  result <- llm_message("Hello") |> chat(claude())
  llt_expect_metadata(result, c("prompt_tokens", "completion_tokens"))
})

llt_test("streaming returns non-empty reply", {
  result <- llm_message("Count to 3.") |> chat(claude(.stream = TRUE))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

llt_test("multi-turn conversation works", {
  result <- llm_message("My name is Alice.") |>
    chat(claude()) |>
    llm_message("What is my name?") |>
    chat(claude())
  llt_expect_reply(result)
  llt_expect_true(grepl("Alice", get_reply(result), ignore.case = TRUE),
                  "Reply should mention Alice")
})

# ── Structured output ─────────────────────────────────────────────────────────

llt_test("structured output returns valid data", {
  schema <- tidyllm_schema(
    name = "Person",
    pers_name = field_chr("Person's name"),
    age        = field_dbl("Person's age")
  )
  result <- llm_message("Extract: John Smith is 35 years old.") |>
    chat(claude(), .json_schema = schema)
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
    chat(claude(), .tools = weather_tool)
  llt_expect_reply(result)
})

llt_test("multi-turn tool use completes", {
  get_city_info <- function(city) {
    info <- list(
      Zathora  = list(country = "Elyndor", population = "412,000"),
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
  ) |> chat(claude(), .tools = list(city_tool, capital_tool), .max_tool_rounds = 5)
  llt_expect_reply(result)
})

llt_test("parallel tool calls work", {
  get_weather <- function(city) {
    w <- list(Paris = "18C cloudy", London = "12C rainy", Tokyo = "25C sunny")
    w[[city]] %||% "Unknown"
  }
  weather_tool <- tidyllm_tool(get_weather, "Get weather", city = field_chr("City name"))
  result <- llm_message("Weather in Paris, London, and Tokyo?") |>
    chat(claude(), .tools = weather_tool)
  llt_expect_reply(result)
})

llt_test("max_tool_rounds limit raises error", {
  counter_tool <- tidyllm_tool(
    function(n) paste0("Counter: ", n, ". Call again with ", n + 1),
    "Increment counter — always call again",
    n = field_dbl("Counter value")
  )
  llt_expect_no_error(
    tryCatch(
      llm_message("Start counting from 1, keep going!") |>
        chat(claude(), .tools = counter_tool, .max_tool_rounds = 2),
      error = function(e) {
        llt_expect_true(grepl("tool round", tolower(e$message)),
                        paste("Unexpected error message:", e$message))
      }
    )
  )
})

# ── Thinking, effort, sampling gates (0.5.1) ──────────────────────────────────

llt_test("adaptive thinking works on default model", {
  result <- llm_message("What is 27 * 453? Reply with just the number.") |>
    chat(claude(.thinking = TRUE))
  llt_expect_reply(result)
  llt_expect_true(grepl("12231", get_reply(result)), "Reply should contain 12231")
})

llt_test("effort parameter is accepted", {
  result <- llm_message("Say hello in one word.") |>
    chat(claude(.effort = "low"))
  llt_expect_reply(result)
})

llt_test("budget thinking still works on older model", {
  result <- llm_message("What is 12 * 12? Reply with just the number.") |>
    chat(claude(.model = "claude-haiku-4-5", .thinking = TRUE, .thinking_budget = 1024,
                .max_tokens = 2048))
  llt_expect_reply(result)
})

llt_test("temperature on new model errors client-side", {
  err <- tryCatch(
    llm_message("Hi") |> chat(claude(.temperature = 0.5)),
    error = function(e) e
  )
  llt_expect_true(inherits(err, "error"), "Should raise an error")
  llt_expect_true(grepl("sampling parameters", conditionMessage(err)),
                  "Error should mention sampling parameters")
})

llt_test("temperature still passes through on older model", {
  result <- llm_message("Say hi in one word.") |>
    chat(claude(.model = "claude-haiku-4-5", .temperature = 0.5))
  llt_expect_reply(result)
})

# ── Prompt caching (0.5.1) ────────────────────────────────────────────────────

llt_test("cache=TRUE request succeeds and reports cache metadata", {
  long_system <- paste(rep(
    "You are a meticulous assistant for a fictional archive of Elyndorian trade records.",
    300), collapse = " ")
  msg <- llm_message("Answer in one word: what is the capital of France?",
                     .system = long_system)
  r1 <- msg |> chat(claude(.cache = TRUE))
  meta1 <- get_metadata(r1)
  llt_expect_true("cache_creation_input_tokens" %in% names(meta1$api_specific[[1]]),
                  "Metadata should carry cache_creation_input_tokens")
  r2 <- msg |> chat(claude(.cache = TRUE))
  meta2 <- get_metadata(r2)
  read_tokens <- meta2$api_specific[[1]]$cache_read_input_tokens
  llt_expect_true(!is.null(read_tokens) && read_tokens > 0,
                  "Second request should read from cache")
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
    chat(claude(), .tools = temp_tool, .stream = TRUE)

  llt_expect_reply(result)
  llt_expect_true(calls >= 1,
                  "the streamed response produced no executed tool call")
})

llt_test("streamed thinking with tools completes", {
  # Thinking blocks are assembled from their own deltas and sent back verbatim
  # by append_tool_messages(); one that lost its text or its signature makes
  # Claude reject the continued turn. Only reachable since .thinking, .stream
  # and .tools can be combined.
  calls <- 0L
  temp_tool <- tidyllm_tool(
    function(city) {
      calls <<- calls + 1L
      paste0(city, ": ", nchar(city), " degrees")
    },
    "Get the current temperature in a city",
    city = field_chr("City name")
  )
  result <- llm_message("What is the temperature in Berlin? Use the tool.") |>
    chat(claude(.thinking = TRUE), .tools = temp_tool, .stream = TRUE)

  llt_expect_reply(result)
  llt_expect_true(calls >= 1, "the streamed thinking response ran no tool call")
})

llt_report()

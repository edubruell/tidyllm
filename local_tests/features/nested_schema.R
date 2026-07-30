devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("nested_schema")

# Regression suite for the 0.5.2 schema fix (BUGS.md item 1):
# tidyllm_schema() must set additionalProperties = FALSE on every object node,
# including the items of an array produced by field_object(.vector = TRUE).
# Strict-mode providers (OpenAI, Azure via OpenRouter) reject the schema otherwise;
# Gemini rejects the key entirely and needs a recursive strip.

nested_schema <- tidyllm_schema(
  name = "Classification",
  classifications = field_object(
    "One entry per title",
    title    = field_chr("The title, copied verbatim"),
    relevant = field_lgl("TRUE if the title describes research"),
    .vector  = TRUE
  ),
  summary = field_object(
    "Aggregate counts",
    n_total    = field_dbl("Number of titles seen"),
    n_relevant = field_dbl("Number of relevant titles")
  )
)

nested_prompt <- paste(
  "Classify these titles:",
  "1. Evaluation der Arbeitsmarktpolitik",
  "2. Kochrezepte fuer Anfaenger",
  sep = "\n"
)

# ── schema shape (offline) ────────────────────────────────────────────────────

llt_test("nested object nodes carry additionalProperties = FALSE", {
  llt_expect_true(isFALSE(nested_schema$additionalProperties),
                  "root should have additionalProperties = FALSE")
  llt_expect_true(isFALSE(nested_schema$properties$classifications$items$additionalProperties),
                  "array items should have additionalProperties = FALSE")
  llt_expect_true(isFALSE(nested_schema$properties$summary$additionalProperties),
                  "nested object should have additionalProperties = FALSE")
  llt_expect_true(identical(attr(nested_schema, "name"), "Classification"),
                  "schema name attribute must survive normalization")
})

llt_test("gemini strip removes additionalProperties recursively", {
  stripped <- tidyllm:::remove_extra_fields_key(nested_schema)
  has_key <- function(node) {
    if (!is.list(node)) return(FALSE)
    if ("additionalProperties" %in% names(node)) return(TRUE)
    any(vapply(node, has_key, logical(1)))
  }
  llt_expect_true(!has_key(stripped), "no node may keep additionalProperties")
})

# ── live round trips ──────────────────────────────────────────────────────────

check_nested_reply <- function(result) {
  llt_expect_s7(result, LLMMessage)
  data <- get_reply_data(result)
  llt_expect_true(!is.null(data), "structured reply must parse")
  llt_expect_true(!is.null(data$classifications), "reply must contain classifications")
  llt_expect_true(NROW(data$classifications) >= 2, "expected one entry per title")
  llt_expect_true(!is.null(data$summary), "reply must contain the nested summary object")
}

nested_case <- function(label, provider_call) {
  llt_test(label, {
    result <- llm_message(nested_prompt) |>
      chat(provider_call, .json_schema = nested_schema)
    check_nested_reply(result)
  })
}

nested_case("openai: array of objects",
            openai(.model = "gpt-4o-mini"))

nested_case("openrouter -> openai (strict mode)",
            openrouter(.model = "openai/gpt-4o-mini"))

nested_case("openrouter -> gemini",
            openrouter(.model = "google/gemini-2.5-flash"))

nested_case("gemini: array of objects",
            gemini(.model = "gemini-2.5-flash"))

nested_case("claude: array of objects",
            claude(.model = "claude-haiku-4-5"))

nested_case("groq: array of objects",
            groq(.model = "openai/gpt-oss-120b"))

nested_case("mistral: array of objects",
            mistral(.model = "mistral-small-latest"))

# deepseek() has no .json_schema argument (JSON object mode only), so it is not
# covered here. perplexity() cannot be called live (no credits on this account),
# so its schema path is checked offline on the request body instead.
llt_test("raw list schemas get a usable schema name on every provider (dry run)", {
  raw_schema <- list(
    type = "object",
    properties = list(answer = list(type = "string")),
    required = list("answer")
  )
  bodies <- list(
    openai      = llm_message("hi") |> chat(openai(.model = "gpt-4o-mini", .dry_run = TRUE),
                                            .json_schema = raw_schema),
    mistral     = llm_message("hi") |> chat(mistral(.model = "mistral-small-latest", .dry_run = TRUE),
                                            .json_schema = raw_schema),
    groq        = llm_message("hi") |> chat(groq(.dry_run = TRUE), .json_schema = raw_schema),
    openrouter  = llm_message("hi") |> chat(openrouter(.model = "openai/gpt-4o-mini", .dry_run = TRUE),
                                            .json_schema = raw_schema),
    perplexity  = llm_message("hi") |> chat(perplexity(.model = "sonar", .dry_run = TRUE),
                                            .json_schema = raw_schema)
  )
  for (provider in names(bodies)) {
    body <- bodies[[provider]]$body$data
    nm <- body$text$format$name %||% body$response_format$json_schema$name
    llt_expect_true(is.character(nm) && length(nm) == 1 && nzchar(nm),
                    paste(provider, "must send a single non-empty schema name, got:",
                          paste(nm, collapse = ", ")))
  }
})

llt_test("perplexity: array of objects (dry run)", {
  dry <- llm_message(nested_prompt) |>
    chat(perplexity(.model = "sonar", .dry_run = TRUE), .json_schema = nested_schema)
  schema <- dry$body$data$response_format$json_schema$schema
  llt_expect_true(isFALSE(schema$additionalProperties),
                  "root should have additionalProperties = FALSE")
  llt_expect_true(isFALSE(schema$properties$classifications$items$additionalProperties),
                  "array items should have additionalProperties = FALSE")
  llt_expect_true(identical(dry$body$data$response_format$json_schema$name, "Classification"),
                  "schema name must be the attribute, not the property names")
})

# ── raw list schemas (no "name" attribute) ────────────────────────────────────

llt_test("raw list schema without a name attribute is accepted", {
  # attr(x, "name") partial-matches "names" unless exact = TRUE, which used to put
  # the property names on the wire as the schema name and 400 on strict providers.
  raw_schema <- list(
    type = "object",
    properties = list(answer = list(type = "string", description = "The answer")),
    required = list("answer")
  )
  result <- llm_message("Answer with the single word: ok") |>
    chat(openrouter(.model = "openai/gpt-4o-mini"), .json_schema = raw_schema)
  llt_expect_true(!is.null(get_reply_data(result)$answer), "structured reply must parse")
})

# ── tool schemas with nested object arguments ─────────────────────────────────

llt_test("openai tool with a nested object argument round trips", {
  weather_tool <- tidyllm_tool(
    function(loc) paste("sunny in", loc$city),
    "get_weather",
    loc = field_object("A location",
                       city    = field_chr("City name"),
                       country = field_chr("Country name"))
  )
  result <- llm_message("What is the weather in Berlin, Germany?") |>
    chat(openai(.model = "gpt-4o-mini"), .tools = weather_tool)
  llt_expect_reply(result)
})

llt_report("nested_schema")

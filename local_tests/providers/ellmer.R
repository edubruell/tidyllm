devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("ellmer")

if (!requireNamespace("ellmer", quietly = TRUE)) {
  cat("  [skip] ellmer package not installed\n")
  llt_report()
  stop("ellmer required", call. = FALSE)
}

# ── Basic chat via ellmer backend ─────────────────────────────────────────────

llt_test("chat_ellmer with anthropic backend returns LLMMessage", {
  ec <- ellmer::chat_anthropic(echo = FALSE)
  result <- llm_message("Say hello in one word.") |>
    chat(ellmer(.ellmer_chat = ec))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

llt_test("chat_ellmer with openai backend returns LLMMessage", {
  ec <- ellmer::chat_openai(echo = FALSE)
  result <- llm_message("Say hello in one word.") |>
    chat(ellmer(.ellmer_chat = ec))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

llt_test("get_metadata returns tokens and model from ellmer backend", {
  ec <- ellmer::chat_anthropic(echo = FALSE)
  result <- llm_message("Hello") |>
    chat(ellmer(.ellmer_chat = ec))
  llt_expect_metadata(result, c("prompt_tokens", "completion_tokens"))
  meta <- get_metadata(result)
  llt_expect_true(nzchar(meta$model), "model should be non-empty string")
})

llt_test("multi-turn conversation preserves history through ellmer", {
  ec <- ellmer::chat_anthropic(echo = FALSE)
  result <- llm_message("My name is Zara.") |>
    chat(ellmer(.ellmer_chat = ec)) |>
    llm_message("What is my name?") |>
    chat(ellmer(.ellmer_chat = ec))
  reply <- tolower(get_reply(result))
  llt_expect_true(grepl("zara", reply), paste("Expected name in reply, got:", reply))
})

# ── ellmer_tool() conversion ──────────────────────────────────────────────────

llt_test("ellmer_tool converts basic ToolDef to TOOL", {
  et <- ellmer::tool(
    function(a, b) a + b,
    "Add two numbers",
    arguments = list(
      a = ellmer::type_number("First number"),
      b = ellmer::type_number("Second number")
    )
  )
  tt <- ellmer_tool(et)
  llt_expect_true(S7_inherits(tt, TOOL), "Should be a TOOL object")
  llt_expect_true(length(tt@input_schema) == 2, "Should have 2 fields")
  llt_expect_true(tt@input_schema$a@type == "number", "a should be type number")
})

llt_test("ellmer_tool preserves enum (type_enum -> field_fct)", {
  et <- ellmer::tool(
    function(unit) unit,
    "Return unit",
    arguments = list(
      unit = ellmer::type_enum("Temperature unit", values = c("celsius", "fahrenheit"))
    )
  )
  tt <- ellmer_tool(et)
  llt_expect_true(length(tt@input_schema$unit@enum) == 2, "Should have 2 enum values")
  llt_expect_true("celsius" %in% tt@input_schema$unit@enum, "Should include celsius")
})

llt_test("ellmer_tool converted tool works in real chat (claude)", {
  et <- ellmer::tool(
    function(a, b) a + b,
    "Add two numbers. Always use this tool for arithmetic.",
    arguments = list(
      a = ellmer::type_number("First number"),
      b = ellmer::type_number("Second number")
    )
  )
  tt <- ellmer_tool(et)
  result <- llm_message("What is 17 + 25? Use the add tool.") |>
    chat(claude(), .tools = tt)
  llt_expect_reply(result)
  reply <- get_reply(result)
  llt_expect_true(grepl("42", reply), paste("Expected 42 in reply, got:", reply))
})

# ── claude_websearch() builtin tool ──────────────────────────────────────────

llt_test("claude_websearch() returns a TOOL object", {
  ws <- claude_websearch()
  llt_expect_true(S7_inherits(ws, TOOL), "Should be a TOOL object")
  llt_expect_true(length(ws@builtin) > 0, "Should have non-empty builtin list")
  llt_expect_true(ws@builtin[[1]]$type == "web_search_20250305", "Should have correct type")
})

llt_test("claude_websearch() tool used in chat returns reply with web results", {
  result <- llm_message("What is the current year? Use web search.") |>
    chat(claude(), .tools = claude_websearch())
  llt_expect_reply(result)
})

llt_report()

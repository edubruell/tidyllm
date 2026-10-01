
test_that("a tool without arguments serialises its properties as an empty JSON object", {
  no_args <- TOOL(
    description = "Takes nothing",
    input_schema = list(),
    func = function() "x",
    name = "no_args"
  )
  expect_identical(
    as.character(jsonlite::toJSON(tool_properties(no_args), auto_unbox = TRUE)),
    "{}"
  )
  claude_req <- llm_message("hi") |> chat(claude(), .tools = no_args, .dry_run = TRUE)
  expect_match(rawToChar(claude_req$body$data |> jsonlite::toJSON(auto_unbox = TRUE) |> charToRaw()), "\"properties\":{}", fixed = TRUE)
})

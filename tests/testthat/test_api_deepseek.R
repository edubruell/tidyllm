testthat::skip_if_not_installed("httptest2")
library(httptest2)

# Test request construction and dry run
test_that("deepseek function constructs a correct request and dry runs it", {
  llm <- llm_message("Write a poem about a (stochastic) parrot")
  
  request <- llm |> chat(deepseek, .dry_run = TRUE)
  dry_run <- request |> httr2::req_dry_run(redact_headers = TRUE, quiet = TRUE)
  
  expect_equal(dry_run$method, "POST")
  expect_true(grepl("/chat/completions", dry_run$path))
  
  headers <- dry_run$headers
  expect_type(headers, "list")
  expect_true("authorization" %in% names(headers))
  expect_true("content-type" %in% names(headers))
  expect_equal(headers$`content-type`, "application/json")
  
  body_json <- request$body |> jsonlite::toJSON() |> as.character()
  expect_true(grepl("deepseek-chat", body_json))
})

# Test response handling
test_that("deepseek returns expected response", {
  with_mock_dir("deepseek", expr = {
    
    if (Sys.getenv("DEEPSEEK_API_KEY") == "") {
      Sys.setenv(DEEPSEEK_API_KEY = "DUMMY_KEY_FOR_TESTING")
    }
    
    result <- deepseek_chat(
      .llm = llm_message("Hello, world"),
      .temperature = 0,
      .stream = FALSE
    )

    reply <- result |>
      get_reply()
    
    if (Sys.getenv("DEEPSEEK_API_KEY") == "DUMMY_KEY_FOR_TESTING") {
      Sys.setenv(DEEPSEEK_API_KEY = "")
    }
    
    expect_true(S7_inherits(result, LLMMessage))
    expect_equal(reply, "Hello! How can I assist you today?")

  }, simplify = FALSE)
})

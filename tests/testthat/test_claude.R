testthat::skip_if_not_installed("httptest2")
library(httptest2)


test_that("claude function constructs a correct request and dry runs it", {
  # Create a mock LLMMessage object
  llm <- LLMMessage$new()
  llm$add_message("user", "Make a poem about anteaters!")
  
  # Call claude with .dry_run = TRUE and perform the dry run
  request <- llm |>
    claude(.dry_run = TRUE) 
  
  dry_run <- request |>
    httr2::req_dry_run(redact_headers = TRUE, quiet = TRUE)
  
  # Check the structure of the returned dry run object
  expect_type(dry_run, "list")
  expect_named(dry_run, c("method", "path", "headers"))
  
  # Check that the method is POST
  expect_equal(dry_run$method, "POST")
  
  # Check that the path is correct
  expect_equal(dry_run$path, "/v1/messages")
  
  # Inspect headers
  headers <- dry_run$headers
  expect_type(headers, "list")
  
  # Check that the required headers are present
  expect_true("accept" %in% names(headers))
  expect_true("accept-encoding" %in% names(headers))
  expect_true("anthropic-version" %in% names(headers))
  expect_true("content-type" %in% names(headers))
  expect_true("x-api-key" %in% names(headers))
  
  # Check that the content-type is JSON
  expect_equal(headers$`content-type`, "application/json; charset=utf-8")
  
  # Now check the body content to ensure the JSON is constructed as expected
  body_json <- request$body |> jsonlite::toJSON()  |> as.character()
  
  expected_json <- "{\"data\":{\"model\":[\"claude-3-5-sonnet-20240620\"],\"max_tokens\":[1024],\"messages\":[{\"role\":[\"user\"],\"content\":[{\"type\":[\"text\"],\"text\":[\"Make a poem about anteaters! \"]}]}],\"system\":[\"You are a helpful assistant\"],\"stream\":[false]},\"type\":[\"json\"],\"content_type\":[\"application/json\"],\"params\":{\"auto_unbox\":[true],\"digits\":[22],\"null\":[\"null\"]}}"

  # Check if the JSON  matches the expected JSON
  expect_equal(body_json, expected_json)
  
})


test_that("claude returns expected response", {
  with_mock_dir("claude",expr = {
    
    # Make sure the environment starts clean
    if (exists("claude", envir = .tidyllm_rate_limit_env)) {
      .tidyllm_rate_limit_env[["claude"]] <- NULL
    }
    
    llm <- LLMMessage$new()
    llm$add_message("user", "Hello, world")
    
    result <- claude(
      .llm = llm,
      .model = "claude-3-5-sonnet-20240620",
      .max_tokens = 1024,
      .temperature = 0,
      .stream = FALSE
    )
    
    # Assertions based on the message in the captured mock response
    expect_true(inherits(result, "LLMMessage"))
    expect_equal(
      result$message_history[[length(result$message_history)]]$content,
      "Hello! It's nice to meet you. How can I assist you today? Is there anything specific you'd like to talk about or any questions you have?"    )
    expect_equal(result$message_history[[length(result$message_history)]]$role, "assistant")
    
    # Now, check that the rate limit environment has been populated with correct values
    expect_true(exists("claude", envir = .tidyllm_rate_limit_env))
    
    # Get the rate limit info for "claude"
    rl_info <- rate_limit_info("claude")
    
    # Assertions for rate limit values based on the mocked response
    expect_equal(rl_info$api, "claude")
    expect_equal(as.POSIXct(rl_info$last_request, tz = "GMT"), 
                 as.POSIXct("2024-10-15 09:48:23", tz = "GMT"),
                 ignore_attr = TRUE)
    expect_equal(rl_info$requests_remaining, 49)
    expect_equal(as.POSIXct(rl_info$requests_reset_time, tz = "GMT"),
                 as.POSIXct("2024-10-15 09:49:10", tz = "GMT"),
                 ignore_attr = TRUE)
    expect_equal(rl_info$tokens_remaining, 40000)
    expect_equal(as.POSIXct(rl_info$tokens_reset_time, tz = "GMT"),
                 as.POSIXct("2024-10-15 09:48:23", tz = "GMT"),
                 ignore_attr = TRUE)
  },simplify = FALSE)
})




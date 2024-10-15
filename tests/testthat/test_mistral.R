testthat::skip_if_not_installed("httptest2")
library(httptest2)

test_that("mistral function constructs a correct request and dry runs it", {
  # Call mistral with .dry_run = TRUE and perform the dry run
  request <- llm_message("Write a poem about the Gallic Rooster") |> mistral(.dry_run = TRUE)
  
  dry_run <- request |>
    httr2::req_dry_run(redact_headers = TRUE, quiet = TRUE)
  
  # Check the structure of the returned dry run object
  expect_type(dry_run, "list")
  expect_named(dry_run, c("method", "path", "headers"))
  
  # Check that the method is POST
  expect_equal(dry_run$method, "POST")
  
  # Check that the path is correct
  expect_equal(dry_run$path, "/v1/chat/completions")
  
  # Inspect headers
  headers <- dry_run$headers
  expect_type(headers, "list")
  
  # Check that the required headers are present
  expect_true("authorization" %in% names(headers))
  expect_true("content-type" %in% names(headers))
  
  # Check that the content-type is JSON
  expect_equal(headers$`content-type`, "application/json")
  
  # Now check the body content to ensure the JSON is constructed as expected
  body_json <- request$body |> jsonlite::toJSON() |> as.character()
  
  expected_json <- "{\"data\":{\"model\":[\"mistral-large-latest\"],\"messages\":[{\"role\":[\"user\"],\"content\":[\"Write a poem about the Gallic Rooster \"]}],\"max_tokens\":[1024],\"stream\":[false]},\"type\":[\"json\"],\"content_type\":[\"application/json\"],\"params\":{\"auto_unbox\":[true],\"digits\":[22],\"null\":[\"null\"]}}"
  # Check if the JSON matches the expected JSON
  expect_equal(body_json, expected_json)
})

test_that("mistral returns expected response",{ 
  with_mock_dir("mistral",expr = {
    
    # Make sure the environment starts clean
    if (exists("mistral", envir = .tidyllm_rate_limit_env)) {
      .tidyllm_rate_limit_env[["mistral"]] <- NULL
    }
    
    llm <- llm_message("user", "Hello, world")
    
    result <- mistral(
      .llm = llm,
      .max_tokens = 1024,
      .temperature = 0,
    )
    
    # Assertions based on the message in the captured mock response
    expect_true(inherits(result, "LLMMessage"))
    expect_equal(
      result$message_history[[length(result$message_history)]]$content,
      "Hello! How can I assist you today? Let's chat about anything you'd like. 😊"
    )
    expect_equal(result$message_history[[length(result$message_history)]]$role, "assistant")
    
    # Now, check that the rate limit environment has been populated with correct values
    expect_true(exists("mistral", envir = .tidyllm_rate_limit_env))
    
    # Retrieve rate limit info for chatgpt
    rl_info <- rate_limit_info("mistral")
    
    # Assertions for rate limit values based on the mocked response
    expect_equal(rl_info$tokens_remaining, 498973)
    expect_equal(as.POSIXct(rl_info$last_request, tz = "GMT"), 
                 as.POSIXct("2024-10-15 22:46:18", tz = "GMT"),
                 ignore_attr = TRUE)
  },simplify = FALSE)
  
})
 

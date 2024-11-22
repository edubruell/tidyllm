testthat::skip_if_not_installed("httptest2")
library(httptest2)

test_that("groq function constructs a correct request and dry runs it", {
  # Make sure the environment starts clean
  if (exists("groq", envir = .tidyllm_rate_limit_env)) {
    .tidyllm_rate_limit_env[["groq"]] <- NULL
  }
  
  # Call groq with .dry_run = TRUE and perform the dry run
  request <- llm_message("Write a poem about meerkats") |> chat(groq,.dry_run = TRUE)
  
  dry_run <- request |>
    httr2::req_dry_run(redact_headers = TRUE, quiet = TRUE)
  
  # Check the structure of the returned dry run object
  expect_type(dry_run, "list")
  expect_named(dry_run, c("method", "path", "headers"))
  
  # Check that the method is POST
  expect_equal(dry_run$method, "POST")
  
  # Check that the path is correct
  expect_equal(dry_run$path, "/openai/v1/chat/completions")
  
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
  
  expected_json <- "{\"data\":{\"model\":[\"llama-3.2-11b-vision-preview\"],\"max_tokens\":[1024],\"messages\":[{\"role\":[\"user\"],\"content\":[\"Write a poem about meerkats \"]}],\"stream\":[false]},\"type\":[\"json\"],\"content_type\":[\"application/json\"],\"params\":{\"auto_unbox\":[true],\"digits\":[22],\"null\":[\"null\"]}}"
  # Check if the JSON matches the expected JSON
  expect_equal(body_json, expected_json)
})

test_that("groq returns expected response", {
  with_mock_dir("groq",expr = {
    
    # Make sure the environment starts clean
    if (exists("groq", envir = .tidyllm_rate_limit_env)) {
      .tidyllm_rate_limit_env[["groq"]] <- NULL
    }
    
    llm <- llm_message("Hello, world")
    
    # Set a dummy key if none exists
    if (Sys.getenv("GROQ_API_KEY") == "") {
      Sys.setenv(GROQ_API_KEY = "DUMMY_KEY_FOR_TESTING")
    }
    
    result <- groq_chat(
      .llm = llm,
      .max_tokens = 1024,
      .temperature = 0,
    )
    
    # Set a dummy key if none exists
    if (Sys.getenv("GROQ_API_KEY") == "DUMMY_KEY_FOR_TESTING") {
      Sys.setenv(GROQ_API_KEY = "")
    }
    
    # Assertions based on the message in the captured mock response
    expect_true(S7_inherits(result, LLMMessage))
    expect_equal(
      result@message_history[[length(result@message_history)]]$content,
      "Hello, world. How can I assist you today?"
    )
    expect_equal(result@message_history[[length(result@message_history)]]$role, "assistant")
    
    # Now, check that the rate limit environment has been populated with correct values
    expect_true(exists("groq", envir = .tidyllm_rate_limit_env))
    
    # Retrieve rate limit info for chatgpt
    rl_info <- rate_limit_info("groq")
    
    # Assertions for rate limit values based on the mocked response
    expect_equal(rl_info$requests_remaining, 6999)
    expect_equal(rl_info$tokens_remaining, 6992)
  },simplify = FALSE)
})

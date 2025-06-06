testthat::skip_if_not_installed("httptest2")
library(httptest2)


test_that("claude function constructs a correct request and dry runs it", {
  # Call claude with .dry_run = TRUE and perform the dry run
  request <- llm_message("Make a poem about anteaters!") |>
    chat(claude,.dry_run = TRUE,.model="claude-3-5-sonnet-20240620") 
  
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
  expect_true("anthropic-version" %in% names(headers))
  expect_true("content-type" %in% names(headers))
  #expect_true("x-api-key" %in% names(headers))
  
  # Check that the content-type is JSON
  expect_equal(headers$`content-type`, "application/json; charset=utf-8")
  
  # Now check the body content to ensure the JSON is constructed as expected
  body_json <- request$body |> jsonlite::toJSON()  |> as.character()
  
  expected_json <- "{\"data\":{\"model\":[\"claude-3-5-sonnet-20240620\"],\"max_tokens\":[2048],\"messages\":[{\"role\":[\"user\"],\"content\":[{\"type\":[\"text\"],\"text\":[\"Make a poem about anteaters! \"]}]}],\"system\":[\"You are a helpful assistant\"],\"stream\":[false]},\"type\":[\"json\"],\"content_type\":[\"application/json\"],\"params\":{\"auto_unbox\":[true],\"digits\":[22],\"null\":[\"null\"]}}"

  # Check if the JSON  matches the expected JSON
  expect_equal(body_json, expected_json)
  
})


test_that("claude returns expected response", {
  with_mock_dir("claude",expr = {
    
    # Make sure the environment starts clean
    if (exists("claude", envir = .tidyllm_rate_limit_env)) {
      .tidyllm_rate_limit_env[["claude"]] <- NULL
    }
    
    messages <- llm_message("Hello, world")
    
    # Store the current API key and set a dummy key if none exists
    if (Sys.getenv("ANTHROPIC_API_KEY") == "") {
      Sys.setenv(ANTHROPIC_API_KEY = "DUMMY_KEY_FOR_TESTING")
    }
    
    result <- claude_chat(
      .llm = messages,
      .model = "claude-3-5-sonnet-20240620",
      .max_tokens = 1024,
      .temperature = 0,
      .stream = FALSE
    )
    
    # Store the current API key and set a dummy key if none exists
    if (Sys.getenv("ANTHROPIC_API_KEY") == "DUMMY_KEY_FOR_TESTING") {
      Sys.setenv(ANTHROPIC_API_KEY = "")
    }
    
    # Assertions based on the message in the captured mock response
    expect_true(S7_inherits(result, LLMMessage))
    expect_equal(
      result@message_history[[length(result@message_history)]]$content,
      "Hello! How can I assist you today? Is there anything specific you'd like to know or discuss?")
    expect_equal(result@message_history[[length(result@message_history)]]$role, "assistant")
    
    # Now, check that the rate limit environment has been populated with correct values
    expect_true(exists("claude", envir = .tidyllm_rate_limit_env))
    
    # Get the rate limit info for "claude"
    rl_info <- rate_limit_info("claude")
    
    # Assertions for rate limit values based on the mocked response
    expect_equal(rl_info$api, "claude")
    expect_equal(rl_info$requests_remaining, 999)
    expect_equal(rl_info$tokens_remaining, 96000)
  },simplify = FALSE)
})

test_that("claude_chat input validation works correctly", {
  llm <- llm_message("Test message")
  
  # Test temperature validation
  expect_error(
    claude_chat(.llm = llm, .temperature = 1.5),
    ".temperature must be numeric between 0 and 1 if provided"
  )
  expect_error(
    claude_chat(.llm = llm, .temperature = -0.1),
    ".temperature must be numeric between 0 and 1 if provided"
  )
  
  # Test top_p validation
  expect_error(
    claude_chat(.llm = llm, .top_p = 1.5),
    ".top_p must be numeric between 0 and 1 if provided"
  )
  
  # Test top_k validation
  expect_error(
    claude_chat(.llm = llm, .top_k = 0.5),
    ".top_k must be a positive integer if provided"
  )
  expect_error(
    claude_chat(.llm = llm, .top_k = -1),
    ".top_k must be a positive integer if provided"
  )
  
  # Test temperature and top_p mutual exclusivity
  expect_error(
    claude_chat(.llm = llm, .temperature = 0.7, .top_p = 0.9),
    "Only one of .temperature or .top_p should be specified"
  )
  
  # Test stop_sequences validation
  expect_error(
    claude_chat(.llm = llm, .stop_sequences = 123),
    ".stop_sequences must be a character vector"
  )
})

test_that("send_claude_batch creates correct request format", {
  # Generate batch of messages
  messages <- glue::glue("Write a haiku about {x}",
                         x = c("Mannheim", "Stuttgart", "Heidelberg")) |>
    purrr::map(llm_message)
  
  # Get request object with dry run
  request <- send_batch(messages, claude, 
                        .model = "claude-3-5-sonnet-20241022", 
                        .dry_run = TRUE)
  
  # Parse the request to check its structure
  dry_run <- request |>
    httr2::req_dry_run(redact_headers = TRUE, quiet = TRUE)
  
  # Check basic request structure
  expect_type(dry_run, "list")
  expect_named(dry_run, c("method", "path", "headers"))
  expect_equal(dry_run$method, "POST")
  expect_equal(dry_run$path, "/v1/messages/batches")
  

  request_body <- request$body
  # Verify requests structure
  expect_true("data" %in% names(request_body))
  expect_equal(length(request_body$data$requests), 3)
  
  # Check each request has the right structure
  purrr::walk(request_body$data$requests, function(req) {
    expect_true("custom_id" %in% names(req))
    expect_true("params" %in% names(req))
    expect_true("model" %in% names(req$params))
    expect_true("messages" %in% names(req$params))
    expect_equal(req$params$model, "claude-3-5-sonnet-20241022")
  })
  
  # Verify that the requests contain the correct prompts
  messages_content <- purrr::map(request_body$data$requests, function(req) {
    # Extract text content from the first message
    req$params$messages[[1]]$content[[1]]$text
  })
  
  expect_true(any(stringr::str_detect(unlist(messages_content), "Mannheim")))
  expect_true(any(stringr::str_detect(unlist(messages_content), "Stuttgart")))
  expect_true(any(stringr::str_detect(unlist(messages_content), "Heidelberg")))
})


test_that("fetch_claude_batch constructs correct request", {
  # Create test batch with necessary attributes
  test_batch <- glue::glue("Write a haiku about {x}", 
                           x = c("Mannheim", "Stuttgart")) |>
    purrr::map(llm_message) |>
    purrr::set_names(paste0("tidyllm_claude_req_", 1:2))
  
  attr(test_batch, "batch_id") <- "test-batch-id-67890"
  
  # Get request object with dry run
  request <- fetch_batch(test_batch, claude, .dry_run = TRUE)
  
  # Parse the request to check its structure
  dry_run <- request |>
    httr2::req_dry_run(redact_headers = TRUE, quiet = TRUE)
  
  # Check request structure
  expect_equal(dry_run$method, "GET")
  expect_equal(dry_run$path, "/v1/messages/batches/test-batch-id-67890")
  
})
testthat::skip_if_not_installed("httptest2")
library(httptest2)

test_that("ollama function constructs a correct request and dry runs it", {
  # Call groq with .dry_run = TRUE and perform the dry run
  request <- llm_message("Write a poem about porcupines") |> 
    ollama(.model="gemma2",.temperature = 0,.dry_run = TRUE)
  
  dry_run <- request |>
    httr2::req_dry_run(redact_headers = TRUE, quiet = TRUE)
  
  # Check the structure of the returned dry run object
  expect_type(dry_run, "list")
  expect_named(dry_run, c("method", "path", "headers"))
  
  # Check that the method is POST
  expect_equal(dry_run$method, "POST")
  
  #Dry run has a reasonable path 
  expect_equal(dry_run$path, "/api/chat")
  
  # Inspect headers
  headers <- dry_run$headers
  expect_type(headers, "list")
  
  # Check that the required headers are present
  expect_true("content-length" %in% names(headers))
  expect_true("content-type" %in% names(headers))
  
  #Content length is identical in header
  expect_equal(headers$`content-length`,"148")
  
  # Check that the content-type is JSON
  expect_equal(headers$`content-type`, "application/json")
  
  # Now check the body content to ensure the JSON is constructed as expected
  body_json <- request$body |> jsonlite::toJSON() |> as.character()
  
  expected_json <- "{\"data\":{\"model\":[\"gemma2\"],\"messages\":[{\"role\":[\"user\"],\"content\":[\"Write a poem about porcupines \"]}],\"options\":{\"temperature\":[0],\"num_ctx\":[2048]},\"stream\":[false]},\"type\":[\"json\"],\"content_type\":[\"application/json\"],\"params\":{\"auto_unbox\":[true],\"digits\":[22],\"null\":[\"null\"]}}"
  # Check if the JSON matches the expected JSON
  expect_equal(body_json, expected_json)
  
})

test_that("ollama_embedding function constructs a correct request and dry runs it", {
  
  #Character vector request
  request <- c("It is not that I am mad, it is only that my head is different from yours",
    "A man can do as he wills, but not will as he wills",
    "Whereof one cannot speak, thereof one must be silent",
    "The limits of my language mean the limits of my world") |>
    ollama_embedding(.dry_run = TRUE)
  
  dry_run <- request |>
    httr2::req_dry_run(redact_headers = TRUE, quiet = TRUE) 
  
  # Check the structure of the returned dry run object
  expect_type(dry_run, "list")
  expect_named(dry_run, c("method", "path", "headers"))
  
  # Check that the method is POST
  expect_equal(dry_run$method, "POST")
  
  #Dry run has a reasonable path 
  expect_equal(dry_run$path, "/api/embed")
  
  # Inspect headers
  headers <- dry_run$headers
  expect_type(headers, "list")
  
  # Check that the required headers are present
  expect_true("content-length" %in% names(headers))
  expect_true("content-type" %in% names(headers))
  
  # Check that the content-type is JSON
  expect_equal(headers$`content-type`, "application/json")
  
  # Now check the body content to ensure the JSON is constructed as expected
  body_json <- request$body |> jsonlite::toJSON() |> as.character()
  expected_json <- "{\"data\":{\"model\":[\"all-minilm\"],\"input\":[\"It is not that I am mad, it is only that my head is different from yours\",\"A man can do as he wills, but not will as he wills\",\"Whereof one cannot speak, thereof one must be silent\",\"The limits of my language mean the limits of my world\"],\"truncate\":[true]},\"type\":[\"json\"],\"content_type\":[\"application/json\"],\"params\":{\"auto_unbox\":[true],\"digits\":[22],\"null\":[\"null\"]}}"
  expect_equal(body_json, expected_json)
})

test_that("ollama returns expected response", {
  with_mock_dir("ollama",expr = {
    

    llm <- llm_message("user", "Hello, world")
    
    result <- ollama(
      .llm = llm,
      .temperature = 0,
      .model = "gemma2"
    )
    
    # Assertions based on the message in the captured mock response
    expect_true(inherits(result, "LLMMessage"))
    expect_equal(
      result$message_history[[length(result$message_history)]]$content,
      "Hello! 👋  How can I help you today?"
    )
    expect_equal(result$message_history[[length(result$message_history)]]$role, "assistant")
    
  },simplify = FALSE)
})


test_that("ollama_embedding returns expected response", {
  with_mock_dir("ollama_embedding",expr = {
    
    result <- c("It is not that I am mad, it is only that my head is different from yours",
                "A man can do as he wills, but not will as he wills",
                "Whereof one cannot speak, thereof one must be silent",
                "The limits of my language mean the limits of my world") |>
      ollama_embedding()
    
    expect_true(is.numeric(result))
    expect_equal(dim(result),c(384,4))
    
  },simplify = FALSE)
})

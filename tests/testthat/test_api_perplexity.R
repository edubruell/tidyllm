testthat::skip_if_not_installed("httptest2")
library(httptest2)

test_that("perplexity function constructs a correct request and dry runs it", {
  # Create a mock LLMMessage object
  llm <- llm_message("Write a haiku about search indices")
  
  # Call perplexity_chat with .dry_run = TRUE
  request <- llm |> chat(perplexity, .dry_run = TRUE) 
  
  dry_run <- request |> httr2::req_dry_run(redact_headers = TRUE, quiet = TRUE)
  
  # Check that the method is POST
  expect_equal(dry_run$method, "POST")
  
  # Check that the URL path is correct
  expect_true(grepl("/chat/completions", dry_run$path))
  
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
  
  expected_json <- "{\"data\":{\"model\":[\"sonar\"],\"messages\":[{\"role\":[\"user\"],\"content\":[\"Write a haiku about search indices \"]}],\"max_tokens\":[1024],\"return_images\":[false],\"search_mode\":[\"web\"],\"return_related_questions\":[false],\"stream\":[false]},\"type\":[\"json\"],\"content_type\":[\"application/json\"],\"params\":{\"auto_unbox\":[true],\"digits\":[22],\"null\":[\"null\"]}}"
  # Check if the JSON matches the expected JSON
  expect_equal(body_json, expected_json)
})

test_that("perplexity returns expected response", {
  with_mock_dir("perplexity", expr = {
    
    # Store the current API key and set a dummy key if none exists
    if (Sys.getenv("PERPLEXITY_API_KEY") == "") {
      Sys.setenv(PERPLEXITY_API_KEY = "DUMMY_KEY_FOR_TESTING")
    }
    
    llm <- llm_message("Write a haiku about Perplexity")
    
    result <- perplexity_chat(llm)
    
    reply <- result |>
      get_reply() 
    
    if (Sys.getenv("PERPLEXITY_API_KEY") == "DUMMY_KEY_FOR_TESTING") {
      Sys.setenv(PERPLEXITY_API_KEY = "")
    }
    
    ## Assertions based on the message in the captured mock response
    expect_true(S7::S7_inherits(result, LLMMessage))
    expect_equal(
      reply,
      "Silent web unfolds,  \nPerplexity finds the truth—  \nAnswers bloom like spring."
    )

  }, simplify = FALSE)
})

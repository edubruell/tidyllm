testthat::skip_if_not_installed("httptest2")
library(httptest2)

test_that("mistral function constructs a correct request and dry runs it", {
  # Call mistral with .dry_run = TRUE and perform the dry run
  request <- llm_message("Write a poem about the Gallic Rooster") |> 
    chat(mistral,.dry_run = TRUE)
  
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
  
  expected_json <- "{\"data\":{\"model\":[\"mistral-large-latest\"],\"messages\":[{\"role\":[\"user\"],\"content\":[\"Write a poem about the Gallic Rooster \"]}],\"temperature\":[0.7],\"max_tokens\":[1024],\"stream\":[false],\"top_p\":[1],\"safe_prompt\":[false]},\"type\":[\"json\"],\"content_type\":[\"application/json\"],\"params\":{\"auto_unbox\":[true],\"digits\":[22],\"null\":[\"null\"]}}"
  # Check if the JSON matches the expected JSON
  expect_equal(body_json, expected_json)
})

test_that("mistral returns expected response",{ 
  with_mock_dir("mistral",expr = {
    
    # Store the current API key and set a dummy key if none exists
    if (Sys.getenv("MISTRAL_API_KEY") == "") {
      Sys.setenv(MISTRAL_API_KEY = "DUMMY_KEY_FOR_TESTING")
    }
    
    # Make sure the environment starts clean
    if (exists("mistral", envir = .tidyllm_rate_limit_env)) {
      .tidyllm_rate_limit_env[["mistral"]] <- NULL
    }
    
    llm <- llm_message("Hello, world")
    
    result <- mistral_chat(
      .llm = llm,
      .max_tokens = 1024,
      .temperature = 0,
    )
    

    if (Sys.getenv("MISTRAL_API_KEY") == "DUMMY_KEY_FOR_TESTING") {
      Sys.setenv(MISTRAL_API_KEY = "")
    }
    
    # Assertions based on the message in the captured mock response
    expect_true(S7_inherits(result, LLMMessage))
    result_tbl <- as_tibble(result) 
    
    expect_equal(
      result_tbl$content[3],
      "Hello! How can I assist you today? Let's chat about anything you'd like. 😊"
    )
    expect_equal(result_tbl$role[3], "assistant")
    
    # Now, check that the rate limit environment has been populated with correct values
    expect_true(exists("mistral", envir = .tidyllm_rate_limit_env))
    

  },simplify = FALSE)
  
})
 

test_that("mistral_embedding returns expected response", {
  with_mock_dir("mistral_embedding",expr = {
    
    # Store the current API key and set a dummy key if none exists
    if (Sys.getenv("MISTRAL_API_KEY") == "") {
      Sys.setenv(MISTRAL_API_KEY = "DUMMY_KEY_FOR_TESTING")
    }
    
    
    result <- c("It is not that I am mad, it is only that my head is different from yours",
                "A man can do as he wills, but not will as he wills",
                "Whereof one cannot speak, thereof one must be silent",
                "The limits of my language mean the limits of my world") |>
      mistral_embedding() 
    
    if (Sys.getenv("MISTRAL_API_KEY") == "DUMMY_KEY_FOR_TESTING") {
      Sys.setenv(MISTRAL_API_KEY = "")
    }
    
    # Test that the result is a tibble
    expect_s3_class(result, "tbl_df")
    
    # Test that the tibble has two columns: input and embeddings
    expect_named(result, c("input", "embeddings"))
    
    # Test that the input column contains the original input texts
    expect_equal(result$input, c("It is not that I am mad, it is only that my head is different from yours",
                                 "A man can do as he wills, but not will as he wills",
                                 "Whereof one cannot speak, thereof one must be silent",
                                 "The limits of my language mean the limits of my world"))
    
    
    purrr::walk(result$embeddings, function(embedding) {
      expect_equal(length(embedding), 1024)
    })
    
  },simplify = FALSE)
})

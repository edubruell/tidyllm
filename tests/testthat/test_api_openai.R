testthat::skip_if_not_installed("httptest2")
library(httptest2)

test_that("openai function constructs a correct request and dry runs it", {
  # Create a mock LLMMessage object
  llm <- llm_message("Write a poem about a (stochastic) parrot")
  
  # Call chatgpt with .dry_run = TRUE and perform the dry run
  request <- llm |>
    chat(openai,.dry_run = TRUE) 
  
  dry_run <- request |>
    httr2::req_dry_run(redact_headers = TRUE, quiet = TRUE)
  
  # Check the structure of the returned dry run object
  expect_type(dry_run, "list")
  expect_named(dry_run, c("method", "path", "headers"))
  
  # Check that the method is POST
  expect_equal(dry_run$method, "POST")
  
  # Check that the URL path is correct
  expect_true(grepl("/v1/chat/completions", dry_run$path))
  
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
  
  expected_json <- "{\"data\":{\"model\":[\"gpt-4o\"],\"messages\":[{\"role\":[\"system\"],\"content\":[\"You are a helpful assistant \"]},{\"role\":[\"user\"],\"content\":[\"Write a poem about a (stochastic) parrot \"]}],\"stream\":[false]},\"type\":[\"json\"],\"content_type\":[\"application/json\"],\"params\":{\"auto_unbox\":[true],\"digits\":[22],\"null\":[\"null\"]}}"
  # Check if the JSON matches the expected JSON
  expect_equal(body_json, expected_json)
  
})


test_that("openai returns expected response", {
  with_mock_dir("openai",expr = {
    
    # Make sure the environment starts clean
    if (exists("openai", envir = .tidyllm_rate_limit_env)) {
      .tidyllm_rate_limit_env[["openai"]] <- NULL
    }
    
    # Store the current API key and set a dummy key if none exists
    if (Sys.getenv("OPENAI_API_KEY") == "") {
      Sys.setenv(OPENAI_API_KEY = "DUMMY_KEY_FOR_TESTING")
    }
    
    llm <- llm_message("Hello, world")
    
    result <- openai_chat(
      .llm = llm,
      .temperature = 0,
      .stream = FALSE
    )
    result_tbl <- as_tibble(result)

    if (Sys.getenv("OPENAI_API_KEY") == "DUMMY_KEY_FOR_TESTING") {
      Sys.setenv(OPENAI_API_KEY = "")
    }
    
    ## Assertions based on the message in the captured mock response
    expect_true(S7_inherits(result, LLMMessage))
    expect_equal(
      result_tbl$content[3],
      "Hello! How can I assist you today?"
    )
    expect_equal(result_tbl$role[3], "assistant")
    
    # Now, check that the rate limit environment has been populated with correct values
    expect_true(exists("openai", envir = .tidyllm_rate_limit_env))
    
    ## Retrieve rate limit info for chatgpt
    rl_info <- rate_limit_info("openai")
    #
    ## Assertions for rate limit values based on the mocked response
    expect_equal(rl_info$requests_remaining, 4999)
    expect_equal(rl_info$tokens_remaining, 799970)
  },simplify = FALSE)
})

test_that("openai_embedding returns expected response", {
  with_mock_dir("openai_embedding",expr = {
    
    # Set a dummy key if none exists
    if (Sys.getenv("OPENAI_API_KEY") == "") {
      Sys.setenv(OPENAI_API_KEY = "DUMMY_KEY_FOR_TESTING")
    }
    
    result <- c("It is not that I am mad, it is only that my head is different from yours",
                "A man can do as he wills, but not will as he wills",
                "Whereof one cannot speak, thereof one must be silent",
                "The limits of my language mean the limits of my world") |>
      openai_embedding() 
    
    if (Sys.getenv("OPENAI_API_KEY") == "DUMMY_KEY_FOR_TESTING") {
      Sys.setenv(OPENAI_API_KEY = "")
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
      expect_equal(length(embedding), 1536)
    })
    
  },simplify = FALSE)
})

test_that("tidyllm_schema() handles single element correctly", {
  with_mock_dir("openai_schema_single", {
    
    # Set a dummy key if none exists
    if (Sys.getenv("OPENAI_API_KEY") == "") {
      Sys.setenv(OPENAI_API_KEY = "DUMMY_KEY_FOR_TESTING")
    }
    
    if (exists("openai", envir = .tidyllm_rate_limit_env)) {
      .tidyllm_rate_limit_env[["openai"]] <- NULL
    }
    
    schema_single <- tidyllm_schema(
      name = "AreaSchema",
      area = "numeric"
    )
    message_single <- llm_message("Imagine an area in JSON format that matches the schema.") |>
      chat(openai(), .json_schema = schema_single) %>%
      get_metadata()
    
    expect_true("completion_tokens" %in% colnames(message_single))
    expect_gt(message_single$total_tokens, 0)
  }, simplify = FALSE)
})

test_that("tidyllm_schema() handles multiple elements correctly", {
  with_mock_dir("openai_schema_multiple", {
    
    # Set a dummy key if none exists
    if (Sys.getenv("OPENAI_API_KEY") == "") {
      Sys.setenv(OPENAI_API_KEY = "DUMMY_KEY_FOR_TESTING")
    }
    
    if (exists("openai", envir = .tidyllm_rate_limit_env)) {
      .tidyllm_rate_limit_env[["openai"]] <- NULL
    }
    
    schema_multiple <- tidyllm_schema(
      name = "AreaSchema",
      area = "numeric",
      population = "numeric"
    )
    message_multiple <- llm_message("Imagine an area and population in JSON format that matches the schema.") |>
      chat(openai(), .json_schema = schema_multiple) %>%
      get_metadata()
    
    expect_true("completion_tokens" %in% colnames(message_multiple))
    expect_gt(message_multiple$total_tokens, 0)
    
    if (Sys.getenv("OPENAI_API_KEY") == "DUMMY_KEY_FOR_TESTING") {
      Sys.setenv(OPENAI_API_KEY = "")
    }
  }, simplify = FALSE)
})

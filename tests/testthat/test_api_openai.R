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
  
  expected_json <- "{\"data\":{\"model\":[\"gpt-4.1\"],\"messages\":[{\"role\":[\"system\"],\"content\":[\"You are a helpful assistant \"]},{\"role\":[\"user\"],\"content\":[\"Write a poem about a (stochastic) parrot \"]}]},\"type\":[\"json\"],\"content_type\":[\"application/json\"],\"params\":{\"auto_unbox\":[true],\"digits\":[22],\"null\":[\"null\"]}}"
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
      "Hello, world! 👋 How can I help you today?"
    )
    expect_equal(result_tbl$role[3], "assistant")
    
    # Now, check that the rate limit environment has been populated with correct values
    expect_true(exists("openai", envir = .tidyllm_rate_limit_env))
    
    ## Retrieve rate limit info for chatgpt
    rl_info <- rate_limit_info("openai")
    #
    ## Assertions for rate limit values based on the mocked response
    expect_equal(rl_info$requests_remaining, 4999)
    expect_equal(rl_info$tokens_remaining, 799986.0)
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


test_that("send_batch creates correct JSONL for batch requests", {
  # Generate batch of messages
  messages <- glue::glue("Write a haiku about {x}",
                         x = c("Mannheim", "Stuttgart", "Heidelberg")) |>
    purrr::map(llm_message)
  
  jsonl_lines <- send_batch(messages, openai, .model = "gpt-4o-mini", .dry_run = TRUE)
  
  
  # Check that we have 3 lines (one for each request)
  expect_equal(length(jsonl_lines), 3)
  
  # Parse each line as JSON
  parsed_lines <- lapply(jsonl_lines, jsonlite::fromJSON)
  
  # Verify structure and content of each line
  purrr::iwalk(parsed_lines, function(x,y){
    expect_equal(x$custom_id, paste0("tidyllm_openai_req_", y))
    expect_equal(x$method, "POST")
    expect_equal(x$url, "/v1/chat/completions")
    expect_equal(x$body$model, "gpt-4o-mini")
    expect_equal(x$body$messages$role, c("system","user"))
    expect_equal(x$body$messages$content[1], "You are a helpful assistant ")
    expect_equal(x$body$messages$role[2], "user")
  })
  
  content_lines <- parsed_lines  |>
    purrr::map_chr(~.x$body$messages$content[2])
  
  expect_true(stringr::str_detect(content_lines[1],"Mannheim"))
  expect_true(stringr::str_detect(content_lines[2],"Stuttgart"))
  expect_true(stringr::str_detect(content_lines[3],"Heidelberg"))
})


test_that("list_openai_batches functions correctly", {
  with_mock_dir("list_openai_batches", {
    # Set a dummy key if none exists
    if (Sys.getenv("OPENAI_API_KEY") == "") {
      Sys.setenv(OPENAI_API_KEY = "DUMMY_KEY_FOR_TESTING")
    }
    
    # Test the function
    result <- list_openai_batches()
    
    # Clean up environment
    if (Sys.getenv("OPENAI_API_KEY") == "DUMMY_KEY_FOR_TESTING") {
      Sys.setenv(OPENAI_API_KEY = "")
    }
    
    # Check that the result is a tibble
    expect_s3_class(result, "tbl_df")
    
    # Check the structure of the returned tibble
    expected_columns <- c("batch_id", "status", "created_at", "expires_at", 
                          "request_total", "request_completed", "request_failed")
    expect_named(result, expected_columns)
    
    # Check column types
    expect_s3_class(result$created_at, "POSIXct")
    expect_s3_class(result$expires_at, "POSIXct")
    expect_type(result$batch_id, "character")
    expect_type(result$status, "character")
    expect_type(result$request_total, "integer")
    expect_type(result$request_completed, "integer")
    expect_type(result$request_failed, "integer")
  }, simplify = FALSE)
})


#test_that("fetch_batch retrieves and processes batch responses correctly", {
#  with_mock_dir("fetch_openai_batch", {
#    # Set a dummy key if none exists
#    if (Sys.getenv("OPENAI_API_KEY") == "") {
#      Sys.setenv(OPENAI_API_KEY = "DUMMY_KEY_FOR_TESTING")
#    }
#    
#    # Create test batch with necessary attributes
#    test_batch <- glue::glue("Write a haiku about {x}", 
#                             x = c("Mannheim", "Stuttgart", "Heidelberg", "Konstanz")) |>
#      purrr::map(llm_message) |>
#      purrr::set_names(paste0("tidyllm_openai_req_", 1:4))
#    
#    attr(test_batch, "batch_id") <- "batch_67daa551c13c8190ab7b855832f423ca"
#    attr(test_batch, "json") <- "FALSE"
#    
#    # Fetch the batch results
#    fetched_batch <- fetch_batch(test_batch, openai)
#    
#    # Clean up environment
#    if (Sys.getenv("OPENAI_API_KEY") == "DUMMY_KEY_FOR_TESTING") {
#      Sys.setenv(OPENAI_API_KEY = "")
#    }
#    
#    # Test that the result is a list of LLMMessage objects
#    expect_type(fetched_batch, "list")
#    expect_length(fetched_batch, 4)
#
#    # Test that each message has an assistant response
#    haikus <- purrr::map_chr(fetched_batch, get_reply)
#    expect_length(haikus, 4)
#    expect_true(all(nchar(haikus) > 0))
#    
#    # Check specific content for each city
#    expect_true(stringr::str_detect(haikus[1], "Mannheim"))
#    expect_true(stringr::str_detect(haikus[2], "Stuttgart"))
#    expect_true(stringr::str_detect(haikus[3], "Heidelberg"))
#    expect_true(stringr::str_detect(haikus[4], "Konstanz"))
#    
#    # Check that metadata was properly included in responses
#    expect_true(all(purrr::map_lgl(fetched_batch, function(msg) {
#      meta <- get_metadata(msg)
#      all(c("prompt_tokens", "completion_tokens", "total_tokens") %in% names(meta))
#    })))
#  }, simplify = FALSE)
#})

test_that("openai_list_models returns expected data", {
  with_mock_dir("openai_models", {
    # Set a dummy key if none exists
    if (Sys.getenv("OPENAI_API_KEY") == "") {
      Sys.setenv(OPENAI_API_KEY = "DUMMY_KEY_FOR_TESTING")
    }
    
    # Test the function
    result <- openai_list_models()
    
    # Clean up environment
    if (Sys.getenv("OPENAI_API_KEY") == "DUMMY_KEY_FOR_TESTING") {
      Sys.setenv(OPENAI_API_KEY = "")
    }
    
    # Check that the result is a tibble
    expect_s3_class(result, "tbl_df")
    
    # Check the structure of the returned tibble
    expect_named(result, c("id", "created", "owned_by"))
    
    # Check that columns have the right types
    expect_type(result$id, "character")
    expect_type(result$created, "character")
    expect_type(result$owned_by, "character")
    
    # Check that we have some models in the result
    expect_gt(nrow(result), 0)
    
    # Verify that common models are included
    common_models <- c("gpt-4o", "gpt-4o-mini")
    expect_true(any(stringr::str_detect(result$id, paste(common_models, collapse = "|"))))
  }, simplify = FALSE)
})

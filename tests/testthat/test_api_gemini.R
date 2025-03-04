testthat::skip_if_not_installed("httptest2")
library(httptest2)


test_that("gemini_chat function constructs a correct request and dry runs it", {
  # Create a mock LLMMessage object
  llm <- llm_message("Write a poem about an evil online search monopolist")
  
  # Call chatgpt with .dry_run = TRUE and perform the dry run
  request <- llm |>
    chat(gemini,.dry_run = TRUE) 
  
  dry_run <- request |>
    httr2::req_dry_run(redact_headers = TRUE, quiet = TRUE)
  
  # Check the structure of the returned dry run object
  expect_type(dry_run, "list")
  expect_named(dry_run, c("method", "path", "headers"))
  
  # Check that the method is POST
  expect_equal(dry_run$method, "POST")
  
  # Check that the URL path is correct
  expect_true(grepl("/v1beta/models/gemini-2.0-flash:generateContent", dry_run$path))
  
  # Inspect headers
  headers <- dry_run$headers
  expect_type(headers, "list")
  
  expect_true("content-type" %in% names(headers))
  
  # Check that the content-type is JSON
  expect_equal(headers$`content-type`, "application/json")
  
  # Now check the body content to ensure the JSON is constructed as expected
  body_json <- request$body |> jsonlite::toJSON() |> as.character()
  
  expected_json <- "{\"data\":{\"model\":[\"gemini-2.0-flash\"],\"contents\":[{\"role\":[\"user\"],\"parts\":{\"text\":[\"Write a poem about an evil online search monopolist \"]}}]},\"type\":[\"json\"],\"content_type\":[\"application/json\"],\"params\":{\"auto_unbox\":[true],\"digits\":[22],\"null\":[\"null\"]}}"
  # Check if the JSON matches the expected JSON
  expect_equal(body_json, expected_json)
  
})

#The mocking-based tests for gemini are uncommented because the google API-key is in the query parameter and not the headers
#Need to find a way to redact that.
if(FALSE){
test_that("gemini_chat returns expected response", {
  with_mock_dir("gemini",expr = {
    

    # Store the current API key and set a dummy key if none exists
    if (Sys.getenv("GOOGLE_API_KEY") == "") {
      Sys.setenv(GOOGLE_API_KEY = "DUMMY_KEY_FOR_TESTING")
    }
    
    llm <- llm_message("Hello, world")
    
    result <- gemini_chat(
      .llm = llm,
      .temperature = 0
    )
    result_tbl <- tibble::as_tibble(result)
    
    if (Sys.getenv("GOOGLE_API_KEY") == "DUMMY_KEY_FOR_TESTING") {
      Sys.setenv(GOOGLE_API_KEY = "")
    }
    
    ## Assertions based on the message in the captured mock response
    expect_true(S7::S7_inherits(result, LLMMessage))
    expect_equal(
      result_tbl$content[3],
     "Hello, world!\n"
    )
    expect_equal(result_tbl$role[3], "assistant")
    
    
  },simplify = FALSE)
})


test_that("gemini_embedding returns expected response", {
  with_mock_dir("gemini_embedding",expr = {
    
    # Store the current API key and set a dummy key if none exists
    if (Sys.getenv("GOOGLE_API_KEY") == "") {
      Sys.setenv(GOOGLE_API_KEY = "DUMMY_KEY_FOR_TESTING")
    }
    
    result <- c("It is not that I am mad, it is only that my head is different from yours",
                "A man can do as he wills, but not will as he wills",
                "Whereof one cannot speak, thereof one must be silent",
                "The limits of my language mean the limits of my world") |>
      embed(gemini()) 
    
    if (Sys.getenv("GOOGLE_API_KEY") == "DUMMY_KEY_FOR_TESTING") {
      Sys.setenv(GOOGLE_API_KEY = "")
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
     expect_equal(length(embedding), 768)
   })
    
  },simplify = FALSE)
})
}
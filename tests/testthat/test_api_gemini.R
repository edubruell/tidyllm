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
  
  # Check that the method is POST
  expect_equal(dry_run$method, "POST")
  
  # Check that the URL path is correct
  expect_true(grepl("/v1beta/models/gemini-2.5-flash:generateContent", dry_run$path))
  
  # Inspect headers
  headers <- dry_run$headers
  expect_type(headers, "list")
  
  expect_true("content-type" %in% names(headers))
  
  # Check that the content-type is JSON
  expect_equal(headers$`content-type`, "application/json")
  
  # Now check the body content to ensure the JSON is constructed as expected
  body_json <- request$body |> jsonlite::toJSON() |> as.character()
  
  expected_json <- "{\"data\":{\"model\":[\"gemini-2.5-flash\"],\"system_instruction\":{\"parts\":{\"text\":[\"You are a helpful assistant\"]}},\"contents\":[{\"role\":[\"user\"],\"parts\":{\"text\":[\"Write a poem about an evil online search monopolist \"]}}]},\"type\":[\"json\"],\"content_type\":[\"application/json\"],\"params\":{\"auto_unbox\":[true],\"digits\":[22],\"null\":[\"null\"]}}"
  # Check if the JSON matches the expected JSON
  expect_equal(body_json, expected_json)
  
})


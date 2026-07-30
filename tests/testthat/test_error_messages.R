test_that("api_error_message unwraps the OpenRouter metadata$raw payload", {
  err <- list(
    message = "Provider returned error",
    code = 400,
    metadata = list(
      raw = paste0(
        '{"error":{"message":"Invalid schema for response_format ',
        "'tidyllm_schema': In context=(\\\"properties\\\"), 'additionalProperties' ",
        'is required to be supplied and to be false.","code":"invalid_json_schema"}}'
      ),
      provider_name = "Azure"
    )
  )

  msg <- tidyllm:::api_error_message(err)
  expect_true(grepl("additionalProperties", msg, fixed = TRUE))
  expect_true(grepl("Provider returned error", msg, fixed = TRUE))
  expect_true(grepl("Azure", msg, fixed = TRUE))
})

test_that("api_error_message keeps a specific upstream message untouched", {
  msg <- tidyllm:::api_error_message(list(message = "Rate limit exceeded"))
  expect_equal(msg, "Rate limit exceeded")
})

test_that("api_error_message falls back when nothing is provided", {
  expect_equal(tidyllm:::api_error_message(list()),
               "No error message returned by the API")
})

test_that("api_error_message surfaces a non-JSON raw payload verbatim", {
  msg <- tidyllm:::api_error_message(list(
    message = "Provider returned error",
    metadata = list(raw = "upstream exploded")
  ))
  expect_true(grepl("upstream exploded", msg, fixed = TRUE))
})

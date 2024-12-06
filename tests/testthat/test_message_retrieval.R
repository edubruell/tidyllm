test_that("get_reply works with raw assistant message", {
  llm <- llm_message("Hello assistant") |>
    llm_message("Hello user",.role="assistant") 
  
  reply <- get_reply(llm)
  expect_equal(reply, "Hello user")
})

test_that("get_reply works with assistant JSON message", {
  llm <-llm_message("Provide data in JSON format")
  json_content <- '{"name": "Alice", "age": 30}'
  llm <-add_message(llm = llm,
                    role = "assistant", 
                    content = json_content, 
                    json = TRUE)
  
  reply_string <- get_reply(llm)
  expect_equal(reply_string, json_content)
})

test_that("get_reply_data works with assistant JSON message", {
  llm <-llm_message("Provide data in JSON format")
  json_content <- '{"name": "Alice", "age": 30}'
  llm <-add_message(llm = llm,
                    role = "assistant", 
                    content = json_content, 
                    json = TRUE)
  parsed_data <- get_reply_data(llm)
  expect_equal(parsed_data, jsonlite::fromJSON(json_content))
})

test_that("get_reply_data warns about corrupt JSON in assistant message marked as JSON", {
  llm <-llm_message("Provide data in JSON format")
  corrupt_json_content <- 'Garbage before JSON {"name": "Alice", "age": 30}'
  llm <-add_message(llm = llm,
                    role = "assistant", 
                    content = corrupt_json_content, 
                    json = TRUE)
  
  expect_warning(parsed_data <- get_reply_data(llm), "Failed to parse JSON content")
  expect_null(parsed_data)
})


test_that("get_reply_data extracts  JSON in assistant message not marked as JSON with warning", {
  llm <-llm_message("Provide data in JSON format")
  corrupt_json_content <- 'Garbage before JSON {"name": "Alice", "age": 30}'
  llm <-add_message(llm = llm,
                    role = "assistant", 
                    content = corrupt_json_content, 
                    json = FALSE)
  
  expect_warning(parsed_data <- get_reply_data(llm), "The reply is not explicitly marked as JSON. Trying to extract JSON.")
  expect_equal(parsed_data, jsonlite::fromJSON('{"name": "Alice", "age": 30}'))
})


test_that("last_reply returns NA_character_ when there are no assistant replies", {
  llm <- llm_message("Hello assistant")
  expect_warning(reply <- last_reply(llm),"No assistant replies available in the message history.")
  expect_equal(reply, NA_character_)
})

test_that("last_reply returns the most recent assistant reply", {
  llm <- llm_message("Hello") 
  llm <- add_message(llm = llm,
                    role = "assistant", 
                    content = "First reply", 
                    json = FALSE)
  llm <- add_message(llm = llm,
                     role = "assistant", 
                     content = '{"data": "Second reply"}', 
                     json = TRUE)
  
  reply <- last_reply(llm)
  expect_equal(reply, '{"data": "Second reply"}')
})

test_that("last_reply ignores non-assistant messages", {
  llm <- llm_message("User message")|>
    llm_message(.role="assistant",.prompt = "Assistant message")
  reply <- last_reply(llm)
  expect_equal(reply, "Assistant message")
})


test_that("get_reply retrieves assistant reply by index", {
 llm <- llm_message("User message")|>
    llm_message(.role="assistant",.prompt = "Assistant reply 1") |>
    llm_message(.role="assistant",.prompt = "Assistant reply 2")
  

  reply1 <- get_reply(llm, .index = 1)
  expect_equal(reply1, "Assistant reply 1")
  reply2 <- get_reply(llm, .index = 2)
  expect_equal(reply2, "Assistant reply 2")
})

test_that("get_reply returns an error for out-of-bounds index", {
  llm <- llm_message("User message")|>
    llm_message(.role="assistant",.prompt = "Assistant reply 1") 
  expect_error(reply <- get_reply(llm, .index = 2), "Index must be a positive integer within bounds")
})

test_that("get_user_message retrieves user message by index", {
  llm <- llm_message("User message 1")|>
    llm_message(.role="assistant",.prompt = "Assistant reply 1") |>
    llm_message(.role="user",.prompt = "User message 2")
  
  msg1 <- get_user_message(llm, .index = 1)
  expect_equal(msg1, "User message 1")
  msg2 <- get_user_message(llm, .index = 2)
  expect_equal(msg2, "User message 2")
})

test_that("get_user_message returns error for out-of-bounds index", {
  llm <- llm_message("User message 1")|>
    llm_message(.role="assistant",.prompt = "Assistant reply 1")
  expect_error(reply <- get_reply(llm, .index = 2), "Index must be a positive integer within bounds")
})

test_that("last_user_message retrieves the most recent user message", {
  llm <- llm_message("User message 1")|>
    llm_message(.role="assistant",.prompt = "Assistant reply 1") |>
    llm_message(.role="user",.prompt = "User message 2")
  last_msg <- last_user_message(llm)
  expect_equal(last_msg, "User message 2")
})

test_that("get_metadata works for multiple assistant replies", {
  llm <- llm_message("User message")
  llm <- add_message(llm=llm,
                     role = "assistant", 
                     content = "First assistant message", 
                     meta = list(model = "gpt-3", 
                                timestamp = "2024-11-08T10:00:00Z", 
                                prompt_tokens = 5, 
                                completion_tokens = 15, 
                                total_tokens = 20)
                     )
  llm <- add_message(llm=llm,
                     role = "assistant", 
                     content = "Second assistant message", 
                     meta = list(model = "gpt-4", 
                                 timestamp = "2024-11-08T10:00:00Z", 
                                 prompt_tokens = 10, 
                                 completion_tokens = 20, 
                                 total_tokens = 30)
  )
  

  metadata <- get_metadata(llm)
  expect_equal(nrow(metadata), 2)
  expect_equal(metadata$model, c("gpt-3", "gpt-4"))
  expect_equal(metadata$total_tokens, c(20, 30))
})

test_that("get_metadata handles missing with an NA row", {
  llm <- llm_message("User message") |>
    llm_message("Assistant message without meta",.role="assistant")
  metadata <- get_metadata(llm)
  expect_equal(nrow(metadata), 1)
  #All standard metadata fields are NA. The one with the api_specific metadata is a list with NULL
  expect_true(all(is.na(metadata)|is.list(metadata)))
})

test_that("get_metadata retrieves metadata for a specific index", {
  llm <- llm_message("User message")
  llm <- add_message(llm=llm,
                     role = "assistant", 
                     content = "First assistant message", 
                     meta = list(model = "gpt-3", 
                                 timestamp = "2024-11-08T10:00:00Z", 
                                 prompt_tokens = 5, 
                                 completion_tokens = 15, 
                                 total_tokens = 20)
  )
  llm <- add_message(llm=llm,
                     role = "assistant", 
                     content = "Second assistant message", 
                     meta = list(model = "gpt-4", 
                                 timestamp = "2024-11-08T10:00:00Z", 
                                 prompt_tokens = 10, 
                                 completion_tokens = 20, 
                                 total_tokens = 30)
  )
  
  metadata <- get_metadata(llm, .index = 2)
  expect_equal(nrow(metadata), 1)
  expect_equal(metadata$model, "gpt-4")
  expect_equal(metadata$total_tokens, 30)
})

test_that("get_metadata returns error for out-of-bounds index", {
  llm <- llm_message("User message")
  llm <- add_message(llm=llm,
                     role = "assistant", 
                     content = "First assistant message", 
                     meta = list(model = "gpt-3", 
                                 timestamp = "2024-11-08T10:00:00Z", 
                                 prompt_tokens = 5, 
                                 completion_tokens = 15, 
                                 total_tokens = 20)
  )
  
  expect_error(get_metadata(llm, .index = 2), "Index must be a positive integer within bounds")
})

test_that("get_metadata returns empty tibble and warns when no assistant messages", {
  llm <- llm_message("User message")
  expect_warning(metadata <- get_metadata(llm), "No assistant replies available in the message history.")
  expect_equal(nrow(metadata), 0)
})

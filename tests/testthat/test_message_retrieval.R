test_that("last_reply works with raw assistant message", {
  llm <- LLMMessage$new()
  llm$add_message(role = "user", content = "Hello assistant")
  llm$add_message(role = "assistant", content = "Hello user", json = FALSE)
  reply <- last_reply(llm)
  expect_equal(reply, "Hello user")
})

test_that("get_reply works with assistant JSON message", {
  llm <- LLMMessage$new()
  llm$add_message(role = "user", content = "Provide data in JSON format")
  json_content <- '{"name": "Alice", "age": 30}'
  llm$add_message(role = "assistant", content = json_content, json = TRUE)
  reply_string <- get_reply(llm)
  expect_equal(reply_string, json_content)
})

test_that("get_reply_data works with assistant JSON message", {
  llm <- LLMMessage$new()
  llm$add_message(role = "user", content = "Provide data in JSON format")
  json_content <- '{"name": "Alice", "age": 30}'
  llm$add_message(role = "assistant", content = json_content, json = TRUE)
  parsed_data <- get_reply_data(llm)
  expect_equal(parsed_data, jsonlite::fromJSON(json_content))
})

test_that("get_reply_data handles corrupt JSON in assistant message", {
  llm <- LLMMessage$new()
  llm$add_message(role = "user", content = "Provide data in JSON format")
  corrupt_json_content <- 'Garbage before JSON {"name": "Alice", "age": 30}'
  llm$add_message(role = "assistant", content = corrupt_json_content, json = TRUE)
  expect_warning(parsed_data <- get_reply_data(llm), "Failed to parse JSON content")
  expect_null(parsed_data)
})


test_that("last_reply returns NA_character_ when there are no assistant replies", {
  llm <- LLMMessage$new()
  llm$add_message(role = "user", content = "Hello assistant")
  expect_warning(reply <- last_reply(llm),"No assistant replies available in the message history.")
  expect_equal(reply, NA_character_)
})

test_that("last_reply returns the most recent assistant reply", {
  llm <- LLMMessage$new()
  llm$add_message(role = "assistant", content = "First reply", json = FALSE)
  llm$add_message(role = "assistant", content = '{"data": "Second reply"}', json = TRUE)
  reply <- last_reply(llm)
  expect_equal(reply, '{"data": "Second reply"}')
})

test_that("last_reply ignores non-assistant messages", {
  llm <- LLMMessage$new()
  llm$add_message(role = "user", content = "User message", json = FALSE)
  llm$add_message(role = "system", content = "System message", json = FALSE)
  llm$add_message(role = "assistant", content = "Assistant message", json = FALSE)
  reply <- last_reply(llm)
  expect_equal(reply, "Assistant message")
})


test_that("get_reply retrieves assistant reply by index", {
  llm <- LLMMessage$new()
  llm$add_message(role = "assistant", content = "Assistant reply 1", json = FALSE)
  llm$add_message(role = "assistant", content = '{"data": "Assistant reply 2"}', json = TRUE)
  reply1 <- get_reply(llm, .index = 1)
  expect_equal(reply1, "Assistant reply 1")
  reply2 <- get_reply(llm, .index = 2)
  expect_equal(reply2, '{"data": "Assistant reply 2"}')
})

test_that("get_reply returns an error for out-of-bounds index", {
  llm <- LLMMessage$new()
  llm$add_message(role = "assistant", content = "Assistant reply", json = FALSE)
  expect_error(reply <- get_reply(llm, .index = 2), "Index must be a positive integer within bounds")
})

test_that("get_user_message retrieves user message by index", {
  llm <- LLMMessage$new()
  llm$add_message(role = "user", content = "User message 1")
  llm$add_message(role = "user", content = "User message 2")
  msg1 <- get_user_message(llm, .index = 1)
  expect_equal(msg1, "User message 1")
  msg2 <- get_user_message(llm, .index = 2)
  expect_equal(msg2, "User message 2")
})

test_that("get_user_message returns error for out-of-bounds index", {
  llm <- LLMMessage$new()
  llm$add_message(role = "user", content = "User message")
  expect_error(reply <- get_reply(llm, .index = 2), "Index must be a positive integer within bounds")
})

test_that("last_user_message retrieves the most recent user message", {
  llm <- LLMMessage$new()
  llm$add_message(role = "user", content = "First user message")
  llm$add_message(role = "assistant", content = "Assistant reply")
  llm$add_message(role = "user", content = "Second user message")
  last_msg <- last_user_message(llm)
  expect_equal(last_msg, "Second user message")
})





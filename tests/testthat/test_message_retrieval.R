test_that("last_reply works with raw assistant message", {
  # Create a new LLMMessage object
  llm <- LLMMessage$new()
  # Add a user message
  llm$add_message(role = "user", content = "Hello assistant")
  # Add an assistant message
  llm$add_message(role = "assistant", content = "Hello user", json = FALSE)
  # Call last_reply
  reply <- last_reply(llm)
  # Check that the reply is the assistant's content
  expect_equal(reply, "Hello user")
})

test_that("last_reply works with assistant JSON message", {
  # Create a new LLMMessage object
  llm <- LLMMessage$new()
  # Add a user message
  llm$add_message(role = "user", content = "Provide data in JSON format")
  # Add an assistant message with valid JSON
  json_content <- '{"name": "Alice", "age": 30}'
  llm$add_message(role = "assistant", content = json_content, json = TRUE)
  # Call last_reply
  reply <- last_reply(llm)
  # Check that parsed_content is correct
  expect_equal(reply$parsed_content, jsonlite::fromJSON(json_content))
  expect_equal(reply$raw_response, json_content)
  expect_true(reply$is_parsed)
})

test_that("last_reply handles corrupt JSON in assistant message", {
  # Create a new LLMMessage object
  llm <- LLMMessage$new()
  # Add a user message
  llm$add_message(role = "user", content = "Provide data in JSON format")
  # Add an assistant message with corrupt JSON
  corrupt_json_content <- 'Garbage before JSON {"name": "Alice", "age": 30}'
  llm$add_message(role = "assistant", content = corrupt_json_content, json = TRUE)
  # Call last_reply and expect a warning
  expect_warning(reply <- last_reply(llm), "Failed to parse JSON content")
  # Check that parsed_content is NULL
  expect_null(reply$parsed_content)
  expect_equal(reply$raw_response, corrupt_json_content)
  expect_false(reply$is_parsed)
})


test_that("last_reply returns raw content when .raw is TRUE", {
  # Create a new LLMMessage object
  llm <- LLMMessage$new()
  # Add a user message
  llm$add_message(role = "user", content = "Provide data in JSON format")
  # Add an assistant message with valid JSON
  json_content <- '{"name": "Alice", "age": 30}'
  llm$add_message(role = "assistant", content = json_content, json = TRUE)
  # Call last_reply with .raw = TRUE
  reply <- last_reply(llm, .raw = TRUE)
  # Check that the reply is the raw content
  expect_equal(reply, json_content)
})

test_that("last_reply returns NULL when there are no assistant replies", {
  # Create a new LLMMessage object
  llm <- LLMMessage$new()
  # Add only user messages
  llm$add_message(role = "user", content = "Hello assistant")
  llm$add_message(role = "user", content = "Are you there?")
  # Call last_reply
  reply <- last_reply(llm)
  # Check that reply is NULL
  expect_null(reply)
})

test_that("last_reply returns the most recent assistant reply", {
  # Create a new LLMMessage object
  llm <- LLMMessage$new()
  # Add multiple assistant replies
  llm$add_message(role = "assistant", content = "First reply", json = FALSE)
  llm$add_message(role = "assistant", content = '{"data": "Second reply"}', json = TRUE)
  # Call last_reply
  reply <- last_reply(llm)
  # Check that the last reply is the second one
  expect_equal(reply$parsed_content, jsonlite::fromJSON('{"data": "Second reply"}'))
})

test_that("last_reply ignores non-assistant messages", {
  # Create a new LLMMessage object
  llm <- LLMMessage$new()
  # Add various roles
  llm$add_message(role = "user", content = "User message", json = FALSE)
  llm$add_message(role = "system", content = "System message", json = FALSE)
  llm$add_message(role = "assistant", content = "Assistant message", json = FALSE)
  llm$add_message(role = "user", content = "User message 2", json = FALSE)
  # Call last_reply
  reply <- last_reply(llm)
  # Check that the reply is the assistant's message
  expect_equal(reply, "Assistant message")
})

test_that("last_reply returns NULL for empty message history", {
  # Create an empty LLMMessage object
  llm <- LLMMessage$new()
  llm$message_history <- list()  # Clear the message history
  # Call last_reply
  reply <- last_reply(llm)
  # Check that the reply is NULL
  expect_null(reply)
})

test_that("get_reply retrieves assistant reply by index", {
  # Create a new LLMMessage object
  llm <- LLMMessage$new()
  # Add assistant messages
  llm$add_message(role = "assistant", content = "Assistant reply 1", json = FALSE)
  llm$add_message(role = "assistant", content = '{"data": "Assistant reply 2"}', json = TRUE)
  # Retrieve first assistant reply
  reply1 <- get_reply(llm, .index = 1)
  expect_equal(reply1, "Assistant reply 1")
  # Retrieve second assistant reply
  reply2 <- get_reply(llm, .index = 2)
  expect_equal(reply2$parsed_content, jsonlite::fromJSON('{"data": "Assistant reply 2"}'))
})

test_that("get_reply returns error for invalid index", {
  llm <- LLMMessage$new()
  llm$add_message(role = "assistant", content = "Assistant reply", json = FALSE)
  expect_error(get_reply(llm, .index = 0), "Index must be a positive integer")
  expect_error(get_reply(llm, .index = 2), "Index is out of bounds")
})

test_that("get_user_message retrieves user message by index", {
  llm <- LLMMessage$new()
  llm$add_message(role = "user", content = "User message 1")
  llm$add_message(role = "user", content = "User message 2")
  # Retrieve first user message
  msg1 <- get_user_message(llm, .index = 1)
  expect_equal(msg1, "User message 1")
  # Retrieve second user message
  msg2 <- get_user_message(llm, .index = 2)
  expect_equal(msg2, "User message 2")
})

test_that("get_user_message returns error for invalid index", {
  llm <- LLMMessage$new()
  llm$add_message(role = "user", content = "User message")
  expect_error(get_user_message(llm, .index = 0), "Index must be a positive integer")
  expect_error(get_user_message(llm, .index = 2), "Index is out of bounds")
})

test_that("last_user_message retrieves the most recent user message", {
  llm <- LLMMessage$new()
  llm$add_message(role = "user", content = "First user message")
  llm$add_message(role = "assistant", content = "Assistant reply")
  llm$add_message(role = "user", content = "Second user message")
  # Retrieve last user message
  last_msg <- last_user_message(llm)
  expect_equal(last_msg, "Second user message")
})

test_that("get_reply returns NULL when there are no assistant replies", {
  llm <- LLMMessage$new()
  llm$add_message(role = "user", content = "User message")
  reply <- get_reply(llm, .index = 1)
  expect_null(reply)
})

test_that("get_user_message returns NULL when there are no user messages", {
  llm <- LLMMessage$new()
  llm$add_message(role = "assistant", content = "Assistant reply")
  msg <- get_user_message(llm, .index = 1)
  expect_null(msg)
})
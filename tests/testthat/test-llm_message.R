test_that("LLMMessage initializes with default system prompt", {
  llm <- LLMMessage$new()
  expect_equal(llm$system_prompt, "You are a helpful assistant")
  expect_equal(length(llm$message_history), 1)
  expect_equal(llm$message_history[[1]]$content, "You are a helpful assistant")
})

test_that("LLMMessage initializes with custom system prompt", {
  custom_prompt <- "Custom system message"
  llm <- LLMMessage$new(system_prompt = custom_prompt)
  expect_equal(llm$system_prompt, custom_prompt)
  expect_equal(llm$message_history[[1]]$content, custom_prompt)
})

test_that("LLMMessage adds a user message", {
  llm <- LLMMessage$new()
  llm$add_message("user", "Hello, assistant!")
  expect_equal(length(llm$message_history), 2)
  expect_equal(llm$message_history[[2]]$role, "user")
  expect_equal(llm$message_history[[2]]$content, "Hello, assistant!")
})

test_that("LLMMessage deep cloning works", {
  llm <- LLMMessage$new()
  llm$add_message("user", "Original message")
  llm_clone <- llm$clone_deep()
  llm_clone$add_message("user", "New message in clone")
  expect_equal(length(llm$message_history), 2)
  expect_equal(length(llm_clone$message_history), 3)
})

test_that("LLMMessage converts to Claude API format", {
  llm <- LLMMessage$new()
  llm$add_message("user", "Hello, assistant!")
  llm$add_message("assistant", "Hello, user!")
  api_format <- llm$to_api_format("claude")
  expect_equal(length(api_format), 2)
  expect_equal(api_format[[1]]$role, "user")
  expect_equal(api_format[[2]]$role, "assistant")
})


test_that("LLMMessage saves media attachments in the right place", {
  llm <- LLMMessage$new()
  llm$add_message("user", "Here is an image", media = list(list(type = "Image", content = "base64string", filename = "image.png")))
  expect_equal(llm$message_history[[2]]$media[[1]]$filename, "image.png")
})

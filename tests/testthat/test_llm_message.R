#Tests for our workhorse class
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

#Tests for the message input function
library(testthat)
library(pdftools)
library(stringr)
library(rlang)

test_that("llm_message creates a new LLMMessage with initial prompt", {
  llm <- llm_message("Hello, assistant!")
  expect_s3_class(llm, "LLMMessage")
  expect_equal(length(llm$message_history), 2)
  expect_equal(llm$message_history[[2]]$role, "user")
  expect_equal(llm$message_history[[2]]$content, "Hello, assistant!")
})

test_that("llm_message adds a prompt to an existing LLMMessage", {
  llm <- LLMMessage$new()
  llm <- llm_message(.llm = llm, .prompt = "Another message")
  expect_equal(length(llm$message_history), 2)
  expect_equal(llm$message_history[[2]]$content, "Another message")
})

test_that("llm_message handles image files", {
  # Create a temporary image file
  temp_img <- tempfile(fileext = ".png")
  png(temp_img)
  plot(1:10)
  dev.off()
  
  llm <- llm_message("Please analyze the attached image.", .imagefile = temp_img)
  expect_equal(length(llm$message_history), 2)
  media <- llm$message_history[[2]]$media
  expect_equal(length(media), 1)
  expect_equal(media[[1]]$type, "Image")
  expect_equal(media[[1]]$filename, basename(temp_img))
  
  # Clean up
  unlink(temp_img)
})

test_that("llm_message handles PDFs with file path", {
  pdf_file <- test_path("pdf-sample_150kB.pdf")
  
  llm <- llm_message("Please summarize the attached PDF.", .pdf = pdf_file)
  expect_equal(length(llm$message_history), 2)
  media <- llm$message_history[[2]]$media
  expect_equal(length(media), 1)
  expect_equal(media[[1]]$type, "PDF")
  expect_equal(media[[1]]$filename, basename(pdf_file))
})

test_that("llm_message handles PDFs with specified pages", {
  pdf_file <- test_path("pdf-sample_150kB.pdf")
  
  llm <- llm_message("Please summarize pages 2 to 3.",
    .pdf = list(filename = pdf_file, start_page = 2, end_page = 3)
  )
  expect_equal(length(llm$message_history), 2)
  media <- llm$message_history[[2]]$media
  expect_equal(length(media), 1)
  expect_equal(media[[1]]$type, "PDF")
  expect_equal(media[[1]]$filename, basename(pdf_file))
  
  # Check that the content corresponds to pages 2 and 3
  pdf_text <- pdftools::pdf_text(pdf_file)[2:3] |> stringr::str_c(collapse = "\n")
  expect_equal(media[[1]]$content, pdf_text)
})

test_that("llm_message adjusts end_page if it exceeds total pages", {
  # Create a sample PDF with 2 pages
  pdf_file <- test_path("pdf-sample_150kB.pdf")
  
  # Expect a warning about end_page adjustment
  expect_warning(
    llm_message("Please summarize pages 1 to 5.",
      .pdf = list(filename = pdf_file, start_page = 1, end_page = 100)
    ),
    "end_page is greater than total pages. Setting end_page to total pages."
  )
  
})

test_that("llm_message handles text files", {
  # Create a temporary text file
  temp_txt <- tempfile(fileext = ".txt")
  writeLines(c("Line 1", "Line 2"), temp_txt)
  
  llm <- llm_message("Please analyze the attached text file.", .textfile = temp_txt)
  expect_equal(length(llm$message_history), 2)
  media <- llm$message_history[[2]]$media
  expect_equal(length(media), 1)
  expect_equal(media[[1]]$type, "TextFile")
  expect_equal(media[[1]]$filename, basename(temp_txt))
  expect_equal(media[[1]]$content, "Line 1\nLine 2")
  
  # Clean up
  unlink(temp_txt)
})

test_that("llm_message handles function outputs with .f", {
  llm <- llm_message("Please analyze the function output.",
    .f = function() {
      print("Test output")
    }
  )
  expect_equal(length(llm$message_history), 2)
  media <- llm$message_history[[2]]$media
  expect_equal(length(media), 1)
  expect_equal(media[[1]]$type, "RConsole")
  expect_equal(media[[1]]$filename, "RConsole.txt")
  expect_equal(media[[1]]$content, "[1] \"Test output\"")
})

test_that("llm_message validates inputs and throws errors for invalid inputs", {
  expect_error(llm_message(.llm = 123), "must be NULL, a character vector, or an LLMMessage object")
  expect_error(llm_message(.prompt = ""), "must be a non-empty string if provided")
  expect_error(llm_message(.role = ""), "must be a non-empty string")
  expect_error(llm_message(.imagefile = "nonexistent.png"), "must be NULL or a valid file path")
  expect_error(llm_message(.pdf = "nonexistent.pdf"), ".pdf must be NULL, a file path, or a list with filename, start_page, and end_page")
  expect_error(
    llm_message(.pdf = list(filename = "nonexistent.pdf", start_page = 1, end_page = 2)),
    ".pdf must be NULL, a file path, or a list with filename, start_page, and end_page"
  )
  expect_error(llm_message(.textfile = "nonexistent.txt"), ".textfile must be NULL or a valid file path")
  expect_error(llm_message(.capture_plot = "yes"), "must be logical")
  expect_error(llm_message(.f = 123), "must be NULL or coercible to a function via rlang::as_function")
})

#strange discrepancy between devtools::check() and devtools::test()
#test_that("llm_message handles .capture_plot correctly", {
#  # Create a plot
#  plot(1:10)
#  
#  llm <- llm_message("Please analyze the attached plot.", .capture_plot = TRUE)
#  expect_equal(length(llm$message_history), 2)
#  media <- llm$message_history[[2]]$media
#  expect_equal(length(media), 1)
#  expect_equal(media[[1]]$type, "Image")
#  expect_true(grepl("\\.png$", media[[1]]$filename))
#})

test_that("llm_message handles initial prompt when .llm is character", {
  llm <- llm_message(.llm = "Initial prompt")
  expect_s3_class(llm, "LLMMessage")
  expect_equal(length(llm$message_history), 2)
  expect_equal(llm$message_history[[2]]$role, "user")
  expect_equal(llm$message_history[[2]]$content, "Initial prompt")
})

test_that("llm_message maintains immutability of original LLMMessage object", {
  llm_original <- LLMMessage$new()
  llm_original$add_message("user", "Original message")
  
  llm_new <- llm_message(.llm = llm_original, .prompt = "New message")
  
  expect_equal(length(llm_original$message_history), 2)
  expect_equal(llm_original$message_history[[2]]$content, "Original message")
  
  expect_equal(length(llm_new$message_history), 3)
  expect_equal(llm_new$message_history[[3]]$content, "New message")
})

test_that("llm_message handles anonymous functions with .f", {
  llm <- llm_message("Please analyze the function output.",
    .f = ~ print("Anonymous function output")
  )
  expect_equal(length(llm$message_history), 2)
  media <- llm$message_history[[2]]$media
  expect_equal(media[[1]]$content, "[1] \"Anonymous function output\"")
})


test_that("pdf_page_batch processes the PDF correctly", {
  # Path to the sample PDF in the tests folder
  pdf_file <- test_path("pdf-sample_150kB.pdf")
  
  # Call the pdf_page_batch function
  general_prompt <- "Extract everything you see on this page as markdown"
  messages <- pdf_page_batch(.pdf = pdf_file, .general_prompt = general_prompt)
  
  # Check that the function returns a list
  expect_type(messages, "list")
  
  # Check that there is one message per page (assuming the PDF has 4 pages)
  expect_length(messages, 4)
  
  # Check that each message is an LLMMessage object
  lapply(messages, function(msg) {
    expect_true(S7_inherits(msg, LLMMessage))
  })
  
  # Check that the first message contains the correct prompt and PDF text
  first_message <- messages[[1]]@message_history[[2]]
  expect_match(first_message$content, "Extract everything you see on this page as markdown")
  expect_match(first_message$content, "Lorem ipsum dolor sit amet")
  
  # Check that the message has an attached image
  expect_true("media" %in% names(first_message))
  expect_match(first_message$media[[1]]$filename, ".png")
})

test_that("pdf_page_batch applies the page range correctly", {
  # Path to the sample PDF in the tests folder
  pdf_file <- test_path("pdf-sample_150kB.pdf")
  
  # Call the pdf_page_batch function with a page range (first 2 pages)
  general_prompt <- "Extract everything you see on this page as markdown"
  messages <- pdf_page_batch(.pdf = pdf_file, .general_prompt = general_prompt, .page_range = c(1, 2))
  
  # Check that only the specified range is processed
  expect_length(messages, 2)
  
  # Check that each message is an LLMMessage object
  lapply(messages, function(msg) {
    expect_true(S7_inherits(msg, LLMMessage))
  })
})

test_that("pdf_page_batch handles custom prompt function", {
  # Path to the sample PDF in the tests folder
  pdf_file <- test_path("pdf-sample_150kB.pdf")
  
  # Custom prompt function that takes page text and returns a custom prompt
  custom_prompt_fn <- function(text) {
    return(paste("Please summarize the following content:", text))
  }
  
  # Call the pdf_page_batch function with a custom prompt function
  messages <- pdf_page_batch(.pdf = pdf_file, .general_prompt = "Extract", .prompt_fn = custom_prompt_fn)
  
  # Check that the custom prompt is applied
  first_message <- messages[[1]]@message_history[[2]]
  expect_match(first_message$content, "Please summarize the following content")
  expect_match(first_message$content, "Lorem ipsum dolor sit amet")
})

test_that("pdf_page_batch handles empty pages", {
  # Path to the sample PDF in the tests folder
  pdf_file <- test_path("pdf-sample_150kB.pdf")
  
  # Call the pdf_page_batch function
  general_prompt <- "Extract everything you see on this page as markdown"
  messages <- pdf_page_batch(.pdf = pdf_file, .general_prompt = general_prompt)
  
  # Check that empty pages are handled correctly
  # (Assuming that page 4 is an empty page in the PDF)
  fourth_message <- messages[[4]]@message_history[[2]]
  expect_match(fourth_message$content, "<pdf></pdf>")
  
  # Check that even empty pages have attached media (images)
  expect_true("media" %in% names(fourth_message))
  expect_match(fourth_message$media[[1]]$filename, ".png")
})

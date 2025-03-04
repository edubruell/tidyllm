testthat::skip_if_not_installed("httptest2")
library(httptest2)

test_that("perplexity function constructs a correct request and dry runs it", {
  # Create a mock LLMMessage object
  llm <- llm_message("Write a haiku about search indices")
  
  # Call perplexity_chat with .dry_run = TRUE
  request <- llm |> chat(perplexity, .dry_run = TRUE) 
  
  dry_run <- request |> httr2::req_dry_run(redact_headers = TRUE, quiet = TRUE)
  
  # Check the structure of the returned dry run object
  expect_type(dry_run, "list")
  expect_named(dry_run, c("method", "path", "headers"))
  
  # Check that the method is POST
  expect_equal(dry_run$method, "POST")
  
  # Check that the URL path is correct
  expect_true(grepl("/chat/completions", dry_run$path))
  
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
  
  expected_json <- "{\"data\":{\"model\":[\"sonar\"],\"max_tokens\":[1024],\"messages\":[{\"role\":[\"user\"],\"content\":[\"Write a haiku about search indices \"]}],\"return_images\":[false],\"stream\":[false]},\"type\":[\"json\"],\"content_type\":[\"application/json\"],\"params\":{\"auto_unbox\":[true],\"digits\":[22],\"null\":[\"null\"]}}"
  # Check if the JSON matches the expected JSON
  expect_equal(body_json, expected_json)
})

test_that("perplexity returns expected response", {
  with_mock_dir("perplexity", expr = {
    
    # Store the current API key and set a dummy key if none exists
    if (Sys.getenv("PERPLEXITY_API_KEY") == "") {
      Sys.setenv(PERPLEXITY_API_KEY = "DUMMY_KEY_FOR_TESTING")
    }
    
    llm <- llm_message("Summarize the latest AI trends")
    
    result <- perplexity_chat(llm)
    
    reply <- result |>
      get_reply() 
    
    if (Sys.getenv("PERPLEXITY_API_KEY") == "DUMMY_KEY_FOR_TESTING") {
      Sys.setenv(PERPLEXITY_API_KEY = "")
    }
    
    ## Assertions based on the message in the captured mock response
    expect_true(S7::S7_inherits(result, LLMMessage))
    expect_equal(
      reply,
      "Here are some of the latest AI trends shaping the technology landscape:\n\n## 1. **Generative AI**\nGenerative AI continues to gain momentum, enabling the creation of original content such as text, images, audio, and video. This technology has seen significant investments and is transforming industries by automating content generation tasks[1][3].\n\n## 2. **Multimodal AI Models**\nMultimodal AI models are becoming increasingly important as they can process multiple types of data simultaneously, such as text, images, and audio. This capability enhances contextual understanding and improves applications in fields like cybersecurity and healthcare[1][2].\n\n## 3. **AI Democratization**\nAI democratization is making AI tools more accessible to a broader audience, including those without extensive technical knowledge. This trend is exemplified by the rapid adoption of tools like ChatGPT[1][3].\n\n## 4. **Workplace AI**\nAI is being integrated into workplaces to enhance productivity by automating repetitive tasks and improving efficiency in various sectors, including manufacturing and customer service[3].\n\n## 5. **Edge AI**\nEdge AI focuses on processing data locally on devices, reducing latency and improving real-time decision-making across industries like manufacturing, healthcare, and retail[2].\n\n## 6. **Quantum Computing and AI Convergence**\nThe integration of quantum computing with AI promises to significantly boost computing power, enabling faster processing of complex data sets and enhancing capabilities in areas such as cybersecurity and scientific research[2].\n\n## 7. **AI Regulation and Ethics**\nAs AI becomes more pervasive, there is a growing need for regulations and ethical frameworks to ensure responsible AI development and deployment[1].\n\n## 8. **Personalization at Scale**\nGenerative AI is also being used to create personalized experiences for customers, including tailored marketing messages and custom products[4]."
    )

  }, simplify = FALSE)
})
testthat::skip_if_not_installed("httptest2")
library(httptest2)

test_that("voyage_embedding returns expected response for text input", {
  with_mock_dir("voyage_embedding_text", {
    
    # Store the current API key and set a dummy key if none exists
    original_key <- Sys.getenv("VOYAGE_API_KEY")
    if (original_key == "") {
      Sys.setenv(VOYAGE_API_KEY = "DUMMY_KEY_FOR_TESTING")
    }
    
    input_texts <- c(
      "How does photosynthesis work?",
      "The quick brown fox jumps over the lazy dog"
    )
    
    result <- input_texts |>
      embed(voyage)
    
    # Reset the API key if we used a dummy
    if (original_key == "") {
      Sys.setenv(VOYAGE_API_KEY = "")
    }
    
    # Check that the result is a tibble with the correct columns and values
    expect_s3_class(result, "tbl_df")
    expect_named(result, c("input", "embeddings"))
    expect_equal(result$input, input_texts)
    
    # Assuming embeddings are 1024-dimensional vectors
    purrr::walk(result$embeddings, function(embedding) {
      expect_equal(length(embedding), 1024)
    })
  })
})



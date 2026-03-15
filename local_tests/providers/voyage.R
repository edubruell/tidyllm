devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("voyage")

cosine_sim <- function(a, b) sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))

# ── Text embeddings ───────────────────────────────────────────────────────────

llt_test("text embed returns tibble with embeddings", {
  result <- c("The dog ran in the park", "A cat sat on the mat") |> embed(voyage())
  llt_expect_tibble(result, min_rows = 2)
  llt_expect_true("embeddings" %in% names(result), "Should have embeddings column")
  llt_expect_true(is.numeric(result$embeddings[[1]]), "Embedding should be numeric vector")
})

llt_test("similar texts have higher cosine similarity than dissimilar ones", {
  result <- c("dog", "puppy", "automobile") |> embed(voyage())
  sim_similar    <- cosine_sim(result$embeddings[[1]], result$embeddings[[2]])  # dog ~ puppy
  sim_dissimilar <- cosine_sim(result$embeddings[[1]], result$embeddings[[3]])  # dog ~ automobile
  llt_expect_true(sim_similar > sim_dissimilar,
                  sprintf("dog~puppy (%.3f) should exceed dog~automobile (%.3f)",
                          sim_similar, sim_dissimilar))
})

# ── Multimodal embeddings ─────────────────────────────────────────────────────

llt_test("multimodal embed returns tibble with image labels", {
  inputs <- list("A dog", "A hotdog", img("local_tests/media/hotdog.jpg"))
  result <- inputs |> embed(voyage())
  llt_expect_tibble(result, min_rows = 3)
  llt_expect_true(any(grepl("\\[IMG\\]", result$input)),
                  "Image entry should have [IMG] label")
})

llt_test("hotdog image embedding is closer to hotdog text than to dog text", {
  inputs <- list("A dog", "A hotdog", img("local_tests/media/hotdog.jpg"))
  result <- inputs |> embed(voyage())

  emb_dog_text    <- result$embeddings[[1]]  # "A dog"
  emb_hotdog_text <- result$embeddings[[2]]  # "A hotdog"
  emb_hotdog_img  <- result$embeddings[[3]]  # hotdog image

  sim_img_to_hotdog <- cosine_sim(emb_hotdog_img, emb_hotdog_text)
  sim_img_to_dog    <- cosine_sim(emb_hotdog_img, emb_dog_text)

  llt_expect_true(
    sim_img_to_hotdog > sim_img_to_dog,
    sprintf("hotdog image should be closer to 'A hotdog' (%.3f) than to 'A dog' (%.3f)",
            sim_img_to_hotdog, sim_img_to_dog)
  )
})

llt_test("dog image embedding is closer to dog text than to hotdog text", {
  inputs <- list("A dog", "A hotdog", img("local_tests/media/dog.jpg"))
  result <- inputs |> embed(voyage())

  emb_dog_text    <- result$embeddings[[1]]  # "A dog"
  emb_hotdog_text <- result$embeddings[[2]]  # "A hotdog"
  emb_dog_img     <- result$embeddings[[3]]  # dog image

  sim_img_to_dog    <- cosine_sim(emb_dog_img, emb_dog_text)
  sim_img_to_hotdog <- cosine_sim(emb_dog_img, emb_hotdog_text)

  llt_expect_true(
    sim_img_to_dog > sim_img_to_hotdog,
    sprintf("dog image should be closer to 'A dog' (%.3f) than to 'A hotdog' (%.3f)",
            sim_img_to_dog, sim_img_to_hotdog)
  )
})

llt_report()

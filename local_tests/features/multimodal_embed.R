devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("multimodal_embed")

MEDIA_DIR <- "local_context/test_data/multimodal"
sample <- function(.file) file.path(MEDIA_DIR, .file)

cosine <- function(.a, .b) sum(.a * .b) / sqrt(sum(.a^2) * sum(.b^2))

embedding_ok <- function(.v, .dim) {
  is.numeric(.v) && length(.v) == .dim && all(is.finite(.v)) && any(.v != 0)
}

rows_distinct <- function(.emb) {
  nrow(.emb) < 2 || length(unique(purrr::map_chr(.emb$embeddings, \(v) paste(round(v, 6), collapse = ",")))) > 1
}

http_up <- function(.url) {
  tryCatch(
    httr2::request(.url) |> httr2::req_timeout(3) |> httr2::req_perform() |> httr2::resp_status() < 500,
    error = function(e) FALSE
  )
}

ollama_has_model <- function(.model) {
  tryCatch({
    tags <- httr2::request("http://localhost:11434/api/tags") |>
      httr2::req_timeout(3) |> httr2::req_perform() |> httr2::resp_body_json()
    any(purrr::map_lgl(tags$models, \(m) startsWith(m$name, .model)))
  }, error = function(e) FALSE)
}

provider_spec <- function(.name, .make, .dim, .small, .dim_arg, .media, .pause = 0) {
  list(name = .name, make = .make, dim = .dim, small = .small,
       dim_arg = .dim_arg, media = .media, pause = .pause)
}

embed_with <- function(.spec, .input, .task = NULL, .dimensions = NULL, .provider = .spec$make()) {
  Sys.sleep(.spec$pause)
  embed(.input, .provider, .task = .task, .dimensions = .dimensions)
}

media_example <- function(.type) {
  switch(.type,
    image = img(sample("cat.jpg")),
    audio = audio_file(sample("cat.wav")),
    video = video_file(sample("cat.mp4"))
  )
}

media_label <- function(.type) {
  switch(.type, image = "[IMG] cat.jpg", audio = "[AUDIO] cat.wav", video = "[VIDEO] cat.mp4")
}

test_text_rows <- function(.spec) {
  llt_test(paste(.spec$name, "text list: rows and dimension"), {
    texts <- c("a photo of a cat", "a photo of a car", "the weather is nice")
    emb <- embed_with(.spec, as.list(texts))
    llt_expect_tibble(emb, 3)
    llt_expect_true(identical(names(emb), c("input", "embeddings")), "columns should be input, embeddings")
    llt_expect_true(identical(emb$input, texts), "input column should hold the texts")
    llt_expect_true(all(purrr::map_lgl(emb$embeddings, embedding_ok, .dim = .spec$dim)),
                    paste("every text vector should be finite with length", .spec$dim))
    llt_expect_true(rows_distinct(emb), "text rows should not all be identical")
  })
}

test_media_type <- function(.spec, .type) {
  llt_test(paste(.spec$name, .type, "returns a vector of the right dimension"), {
    emb <- embed_with(.spec, list(media_example(.type)))
    llt_expect_tibble(emb, 1)
    llt_expect_true(nrow(emb) == 1, "one media item should give one row")
    llt_expect_true(identical(emb$input, media_label(.type)),
                    paste("label was", emb$input))
    llt_expect_true(embedding_ok(emb$embeddings[[1]], .spec$dim),
                    paste("expected a finite vector of length", .spec$dim))
  })
}

test_mixed_and_retrieval <- function(.spec) {
  llt_test(paste(.spec$name, "mixed list: one row per element, in order"), {
    items <- list("a photo of a cat", img(sample("cat.jpg")), img(sample("car.jpg")), "a photo of a car")
    emb <- embed_with(.spec, items)
    llt_expect_tibble(emb, 4)
    llt_expect_true(nrow(emb) == 4, paste("expected 4 rows, got", nrow(emb)))
    llt_expect_true(identical(emb$input, c("a photo of a cat", "[IMG] cat.jpg", "[IMG] car.jpg", "a photo of a car")),
                    paste("labels were:", paste(emb$input, collapse = " | ")))
    llt_expect_true(all(purrr::map_lgl(emb$embeddings, embedding_ok, .dim = .spec$dim)),
                    "all rows should be finite vectors of the provider dimension")
    llt_expect_true(rows_distinct(emb), "rows should not all be identical")
  })

  llt_test(paste(.spec$name, "retrieval: 'a photo of a cat' is closer to cat.jpg than car.jpg"), {
    emb <- embed_with(.spec, list("a photo of a cat", img(sample("cat.jpg")), img(sample("car.jpg"))),
                      .task = "query")
    v <- emb$embeddings
    to_cat <- cosine(v[[1]], v[[2]])
    to_car <- cosine(v[[1]], v[[3]])
    llt_expect_true(to_cat > to_car,
                    sprintf("cosine to cat %.4f should exceed cosine to car %.4f", to_cat, to_car))
  })
}

test_nested <- function(.spec) {
  llt_test(paste(.spec$name, "nested list gives one fused row"), {
    emb <- embed_with(.spec, list(list("a cat", img(sample("cat.jpg")))))
    llt_expect_true(nrow(emb) == 1, paste("expected 1 row, got", nrow(emb)))
    llt_expect_true(identical(emb$input, "a cat + [IMG] cat.jpg"), paste("label was", emb$input))
    llt_expect_true(embedding_ok(emb$embeddings[[1]], .spec$dim), "fused vector should be finite with the provider dimension")
  })
}

test_dimensions <- function(.spec) {
  if (is.null(.spec$dim_arg)) return(invisible(NULL))
  llt_test(paste(.spec$name, .spec$dim_arg, "changes the vector length"), {
    items <- list("a photo of a cat", img(sample("cat.jpg")))
    emb <- if (.spec$dim_arg == ".dimensions") {
      embed_with(.spec, items, .dimensions = .spec$small)
    } else {
      embed_with(.spec, items, .provider = .spec$make(.output_dimension = .spec$small))
    }
    lens <- purrr::map_int(emb$embeddings, length)
    llt_expect_true(all(lens == .spec$small), paste("lengths were", paste(lens, collapse = ", "), "expected", .spec$small))
    llt_expect_true(.spec$small != .spec$dim, "test needs a small size different from the default")
  })
}

run_provider <- function(.spec) {
  cat(sprintf("\n  -- %s --\n", .spec$name))
  test_text_rows(.spec)
  purrr::walk(.spec$media, \(t) test_media_type(.spec, t))
  test_mixed_and_retrieval(.spec)
  test_nested(.spec)
  test_dimensions(.spec)
}

llt_test("provider_capabilities(.what = 'embed_media') has the expected shape", {
  caps <- provider_capabilities(.what = "embed_media")
  llt_expect_tibble(caps)
  llt_expect_true(identical(names(caps), c("provider", "media", "supported")), "columns should be provider, media, supported")
  llt_expect_true(setequal(caps$media, c("image", "audio", "video")), "media should be image, audio, video")
  llt_expect_true(is.logical(caps$supported), "supported should be logical")
  supported <- function(p, m) caps$supported[caps$provider == p & caps$media == m]
  llt_expect_true(all(purrr::map_lgl(c("image", "audio", "video"), \(m) supported("gemini", m))), "gemini should embed all three")
  llt_expect_true(supported("voyage", "image") && !supported("voyage", "audio") && !supported("voyage", "video"), "voyage embeds image but not audio")
  llt_expect_true(!supported("ollama", "video"), "ollama does not embed video")
  llt_expect_true(!any(caps$supported[caps$provider == "openai"]), "openai embeds no media")
})

llt_test("embed() of an image with openai errors before any request", {
  err <- tryCatch(embed(list(img(sample("cat.jpg"))), openai(), .dry_run = TRUE), error = function(e) conditionMessage(e))
  llt_expect_true(is.character(err) && grepl("cannot embed image", err), paste("got:", paste(err, collapse = " ")))
})

llt_test("embed() of audio with voyage errors before any request", {
  err <- tryCatch(embed(list(audio_file(sample("cat.wav"))), voyage(), .dry_run = TRUE), error = function(e) conditionMessage(e))
  llt_expect_true(is.character(err) && grepl("cannot embed audio", err), paste("got:", paste(err, collapse = " ")))
})

llt_test("embed() of video with voyage errors before any request", {
  err <- tryCatch(embed(list(video_file(sample("cat.mp4"))), voyage(), .dry_run = TRUE), error = function(e) conditionMessage(e))
  llt_expect_true(is.character(err) && grepl("cannot embed video", err), paste("got:", paste(err, collapse = " ")))
})

llt_test("embed() rejects a malformed list element", {
  err <- tryCatch(embed(list(42), gemini(), .dry_run = TRUE), error = function(e) conditionMessage(e))
  llt_expect_true(is.character(err) && grepl("single string", err), paste("got:", paste(err, collapse = " ")))
})

if (!nzchar(Sys.getenv("LLAMACPP_SERVER"))) Sys.setenv(LLAMACPP_SERVER = "http://localhost:8091")
if (!http_up(paste0(Sys.getenv("LLAMACPP_SERVER"), "/health"))) {
  cat("  [skip] llamacpp - no llama-server answers on", Sys.getenv("LLAMACPP_SERVER"), "\n")
} else {
  run_provider(provider_spec("llamacpp", \(...) llamacpp(.model = "local-model", ...), 768, NULL, NULL,
                             c("image", "audio", "video")))
}

if (!ollama_has_model("embeddinggemma-2")) {
  cat("  [skip] ollama - embeddinggemma-2 is not available on the local Ollama server\n")
} else {
  run_provider(provider_spec("ollama", \(...) ollama(.model = "embeddinggemma-2:740m", ...), 768, 256,
                             ".dimensions", c("image", "audio")))
}

if (!nzchar(Sys.getenv("GOOGLE_API_KEY"))) {
  cat("  [skip] gemini - GOOGLE_API_KEY is not set\n")
} else {
  run_provider(provider_spec("gemini", \(...) gemini(.model = "gemini-embedding-2", ...), 3072, 256,
                             ".dimensions", c("image", "audio", "video")))
  llt_test("gemini .media_columns adds modality and path in row order", {
    out <- embed(list("a cat", list("a dog", img(sample("dog.jpg"))), audio_file(sample("speech.wav"))),
                 gemini(.model = "gemini-embedding-2"), .media_columns = TRUE)
    llt_expect_true(identical(names(out), c("input", "modality", "path", "embeddings")), paste(names(out), collapse = ","))
    llt_expect_true(identical(out$modality, c("text", "text + image", "audio")), paste(out$modality, collapse = ","))
    llt_expect_true(identical(out$path, c(NA, sample("dog.jpg"), sample("speech.wav"))), paste(out$path, collapse = ","))
  })
}

if (!nzchar(Sys.getenv("OPENROUTER_API_KEY"))) {
  cat("  [skip] openrouter - OPENROUTER_API_KEY is not set\n")
} else {
  run_provider(provider_spec("openrouter", \(...) openrouter(.model = "google/gemini-embedding-2", ...), 3072, 256,
                             ".dimensions", c("image", "audio", "video")))
}

if (!nzchar(Sys.getenv("VOYAGE_API_KEY"))) {
  cat("  [skip] voyage - VOYAGE_API_KEY is not set\n")
} else {
  run_provider(provider_spec("voyage", \(...) voyage(...), 1024, 256,
                             ".output_dimension", c("image"), .pause = 25))
}

llt_report("multimodal_embed")

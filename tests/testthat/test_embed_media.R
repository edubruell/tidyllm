tiny_media <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  paths <- file.path(dir, c("cat.png", "cat.wav", "cat.mp4"))
  purrr::walk(paths, \(p) writeBin(as.raw(1:32), p))
  list(
    image = img(paths[1]),
    audio = audio_file(paths[2]),
    video = video_file(paths[3]),
    paths = paths
  )
}

body_of <- function(.request) .request$body$data

with_fake_keys <- function(code) {
  withr::with_envvar(
    c(GOOGLE_API_KEY = "x", OPENROUTER_API_KEY = "x", VOYAGE_API_KEY = "x"),
    code
  )
}

test_that("embed_items parses text, media and fused input into labelled items", {
  m <- tiny_media()
  cases <- list(
    list(input = c("a", "b"),                      labels = c("a", "b"),                       parts = c(1, 1)),
    list(input = list("a", m$image),               labels = c("a", "[IMG] cat.png"),           parts = c(1, 1)),
    list(input = list(m$audio, m$video),           labels = c("[AUDIO] cat.wav", "[VIDEO] cat.mp4"), parts = c(1, 1)),
    list(input = list(list("a", m$image)),         labels = "a + [IMG] cat.png",               parts = 2)
  )
  purrr::walk(cases, \(case) {
    items <- embed_items(case$input)
    expect_equal(embed_labels(items), case$labels)
    expect_equal(vapply(items, \(i) length(i$parts), numeric(1)), case$parts)
  })
})

test_that("embed_items rejects malformed input", {
  m <- tiny_media()
  bad <- list(
    list(1),
    list(NULL),
    list(c("a", "b")),
    list(NA_character_),
    list(list()),
    list(list("a", 2)),
    list(m$image, list(m$audio, TRUE)),
    list(),
    character(0),
    42,
    tibble::tibble(a = "x"),
    c("x", NA)
  )
  purrr::walk(bad, \(x) expect_error(embed_items(x), "must"))
})

test_that("embed_media_used and embed_is_text_only read the items", {
  m <- tiny_media()
  expect_true(embed_is_text_only(embed_items(c("a", "b"))))
  expect_false(embed_is_text_only(embed_items(list("a", m$image))))
  expect_false(embed_is_text_only(embed_items(list(list("a", "b")))))
  expect_setequal(embed_media_used(embed_items(list("a", m$image, m$audio))), c("image", "audio"))
  expect_length(embed_media_used(embed_items("a")), 0)
})

test_that("the embed media registry matches what each provider documents", {
  caps <- provider_capabilities(.what = "embed_media") |> dplyr::filter(supported)
  registry <- split(caps$media, caps$provider)
  expect_setequal(registry$llamacpp, c("image", "audio", "video"))
  expect_setequal(registry$gemini, c("image", "audio", "video"))
  expect_setequal(registry$openrouter, c("image", "audio", "video"))
  expect_setequal(registry$ollama, c("image", "audio"))
  expect_setequal(registry$voyage, "image")
  expect_null(registry$openai)
})

test_that("embed refuses media a provider cannot embed before any request", {
  m <- tiny_media()
  cases <- list(
    list(input = list(m$image),               provider = quote(openai()),   pattern = "cannot embed image"),
    list(input = list("a", m$audio),          provider = quote(voyage()),   pattern = "cannot embed audio"),
    list(input = list(m$video),               provider = quote(voyage()),   pattern = "cannot embed video"),
    list(input = list(m$video),               provider = quote(ollama()),   pattern = "cannot embed video"),
    list(input = list(list("a", "b")),        provider = quote(mistral()),  pattern = "fused")
  )
  purrr::walk(cases, \(case) {
    expect_error(
      rlang::eval_tidy(rlang::call2("embed", case$input, case$provider, .dry_run = TRUE)),
      case$pattern
    )
  })
})

test_that("embed passes a text-only list to text providers as a character vector", {
  request <- embed(list("a", "b"), ollama(), .dry_run = TRUE)
  expect_equal(unlist(body_of(request)$input), c("a", "b"))
})

test_that("embed rejects bad .task and .dimensions", {
  expect_error(embed("a", llamacpp(), .task = "similarity", .dry_run = TRUE), ".task")
  expect_error(embed("a", ollama(), .dimensions = 0, .dry_run = TRUE), ".dimensions")
  expect_error(embed("a", ollama(), .dimensions = 2.5, .dry_run = TRUE), ".dimensions")
  expect_error(embed("a", openai(), .task = "query"), "not supported")
})

test_that("llamacpp builds typed content parts and leaves text-only input alone", {
  m <- tiny_media()
  text_only <- embed(c("a", "b"), llamacpp(), .dry_run = TRUE)
  expect_equal(unlist(body_of(text_only)$input), c("a", "b"))

  mixed <- body_of(embed(list("a", m$image, m$audio, m$video, list("t", m$image)),
                         llamacpp(), .dry_run = TRUE, .task = "query"))$input
  types <- purrr::map(mixed, \(i) purrr::map_chr(i$content, "type"))
  expect_equal(types, list("text", "image_url", "input_audio", "input_video", c("text", "image_url")))
  expect_equal(mixed[[1]]$content[[1]]$text, "task: search result | query: a")
  expect_equal(mixed[[3]]$content[[1]]$input_audio$format, "wav")
  expect_match(mixed[[4]]$content[[1]]$input_video$url, "^data:video/mp4;base64,")
})

test_that("openrouter sends data URLs for audio and video and the dimensions field", {
  m <- tiny_media()
  body <- with_fake_keys(body_of(
    embed(list(m$audio, m$video), openrouter(.model = "google/gemini-embedding-2"),
          .dry_run = TRUE, .dimensions = 768)
  ))
  expect_equal(body$dimensions, 768)
  expect_match(body$input[[1]]$content[[1]]$input_audio$data, "^data:audio/wav;base64,")
  expect_match(body$input[[2]]$content[[1]]$input_video$data, "^data:video/mp4;base64,")
})

test_that("ollama sends plain base64 objects and fuses text with one image", {
  m <- tiny_media()
  body <- body_of(embed(list("a", m$image, m$audio, list("t", m$image)),
                        ollama(), .dry_run = TRUE, .dimensions = 256))
  expect_equal(body$dimensions, 256)
  expect_equal(body$input[[1]], "a")
  expect_named(body$input[[2]], "image")
  expect_named(body$input[[3]], "audio")
  expect_named(body$input[[4]], c("text", "image"))
  expect_false(grepl("^data:", body$input[[2]]$image))
  expect_error(
    embed(list(list(m$image, m$image)), ollama(), .dry_run = TRUE),
    "at most one image"
  )
})

test_that("gemini builds one request per item with inlineData parts", {
  m <- tiny_media()
  body <- with_fake_keys(body_of(
    embed(list("a", list("t", m$image), m$video), gemini(), .dry_run = TRUE, .dimensions = 768)
  ))
  expect_length(body$requests, 3)
  expect_equal(body$requests[[1]]$content$parts[[1]]$text, "a")
  expect_equal(body$requests[[2]]$content$parts[[2]]$inlineData$mimeType, "image/png")
  expect_equal(body$requests[[3]]$content$parts[[1]]$inlineData$mimeType, "video/mp4")
  expect_true(all(purrr::map_dbl(body$requests, "outputDimensionality") == 768))
})

test_that("gemini refuses a media file over the size limit", {
  big <- withr::local_tempfile(fileext = ".wav")
  con <- file(big, "wb")
  writeBin(raw(21 * 1024^2), con)
  close(con)
  expect_error(
    with_fake_keys(embed(list(audio_file(big)), gemini(), .dry_run = TRUE)),
    "20 MB limit"
  )
})

test_that("voyage switches to the multimodal endpoint for images", {
  m <- tiny_media()
  with_fake_keys({
    text <- embed(c("a", "b"), voyage(), .dry_run = TRUE)
    expect_match(text$url, "/v1/embeddings$")

    media <- embed(list("a", list("t", m$image), m$image), voyage(),
                   .dry_run = TRUE, .task = "document")
    expect_match(media$url, "/v1/multimodalembeddings$")
    body <- body_of(media)
    expect_equal(body$input_type, "document")
    expect_equal(purrr::map_chr(body$inputs[[3]]$content, "type"), "image_base64")
    expect_equal(purrr::map_chr(body$inputs[[2]]$content, "type"), c("text", "image_base64"))
  })
})

test_that("img() labels common image formats with their MIME type", {
  dir <- withr::local_tempdir()
  cases <- c(png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg", webp = "image/webp", gif = "image/gif")
  purrr::iwalk(cases, \(mime, ext) {
    path <- file.path(dir, paste0("x.", ext))
    writeBin(as.raw(1:8), path)
    expect_match(img(path)@imagebase64, paste0("^data:", mime, ";base64,"))
  })
})

test_that("the task prefix goes once before the first text part of an item", {
  m <- tiny_media()
  ollama_body <- body_of(embed(list(list("a", "b", m$image)), ollama(), .task = "query", .dry_run = TRUE))
  expect_equal(ollama_body$input[[1]]$text, "task: search result | query: a b")

  llamacpp_body <- body_of(embed(list(list("a", "b")), llamacpp(), .task = "document", .dry_run = TRUE))
  texts <- purrr::map_chr(llamacpp_body$input[[1]]$content, "text")
  expect_equal(texts, c("title: none | text: a", "b"))

  gemini_body <- with_fake_keys(body_of(embed(c("a", "b"), gemini(), .task = "query", .dry_run = TRUE)))
  expect_equal(purrr::map_chr(gemini_body$requests, \(r) r$content$parts[[1]]$text),
               paste0("task: search result | query: ", c("a", "b")))
})

test_that("ollama refuses MP3 audio before any request", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "x.mp3")
  writeBin(as.raw(1:8), path)
  expect_error(embed(list(audio_file(path)), ollama(), .dry_run = TRUE), "WAV or OGG")
})

test_that("embed keeps LLMMessage and named character input working for text providers", {
  named <- c(first = "a", second = "b")
  expect_equal(unname(unlist(body_of(embed(named, ollama(), .dry_run = TRUE))$input)), c("a", "b"))

  llm <- llm_message("Hello there")
  expect_equal(unlist(body_of(embed(llm, ollama(), .dry_run = TRUE))$input), "Hello there ")
})

test_that("embed_media_columns adds modality and path for plain, media and fused items", {
  m <- tiny_media()
  items <- embed_items(list("a", m$image, m$audio, list("t", m$image), list(m$image, m$audio)))
  result <- tibble::tibble(input = embed_labels(items), embeddings = list(1, 2, 3, 4, 5))
  out <- embed_media_columns(result, items)
  expect_named(out, c("input", "modality", "path", "embeddings"))
  expect_equal(out$modality, c("text", "image", "audio", "text + image", "image + audio"))
  expect_equal(out$path, c(NA, m$paths[1], m$paths[2], m$paths[1], paste(m$paths[1:2], collapse = " + ")))
  expect_error(embed_media_columns(result[1:2, ], items), "different number of rows")
})

test_that("embed adds media columns only on request and rejects a non-logical flag", {
  m <- tiny_media()
  expect_error(embed("a", ollama(), .media_columns = "yes", .dry_run = TRUE), ".media_columns")
  expect_s3_class(embed(list("a", m$image), ollama(), .media_columns = TRUE, .dry_run = TRUE), "httr2_request")
})

test_that("embed_part_label and embed_part_type name every part kind", {
  m <- tiny_media()
  cases <- list(
    list(part = "hi",    type = "text",  label = "hi"),
    list(part = m$image, type = "image", label = "[IMG] cat.png"),
    list(part = m$audio, type = "audio", label = "[AUDIO] cat.wav"),
    list(part = m$video, type = "video", label = "[VIDEO] cat.mp4")
  )
  purrr::walk(cases, \(case) {
    expect_equal(embed_part_type(case$part), case$type)
    expect_equal(embed_part_label(case$part), case$label)
  })
})

test_that("openai and mistral send .dimensions under their own field names", {
  withr::local_envvar(OPENAI_API_KEY = "x", MISTRAL_API_KEY = "x")
  openai_body <- body_of(embed("a", openai(), .dimensions = 256, .dry_run = TRUE))
  expect_equal(openai_body$dimensions, 256)
  mistral_body <- body_of(embed("a", mistral(.model = "codestral-embed"), .dimensions = 256, .dry_run = TRUE))
  expect_equal(mistral_body$output_dimension, 256)
  expect_null(body_of(embed("a", openai(), .dry_run = TRUE))$dimensions)
  expect_null(body_of(embed("a", mistral(), .dry_run = TRUE))$output_dimension)
  expect_error(embed("a", openai(), .dimensions = 0, .dry_run = TRUE), ".dimensions")
  expect_error(embed("a", llamacpp(), .dimensions = 256, .dry_run = TRUE))
})

# Offline tests for multimodal decide() input: parts, checks, registry and request bodies.
#' Write a header-only PNG with the given pixel size and return its path.
#' @noRd
fake_png <- function(.w, .h, .dir = withr::local_tempdir(.local_envir = parent.frame()), .name = "a.png") {
  be32 <- \(n) as.raw(c(n %/% 16777216, (n %/% 65536) %% 256, (n %/% 256) %% 256, n %% 256))
  path <- file.path(.dir, .name)
  writeBin(c(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)), be32(13L), charToRaw("IHDR"),
             be32(.w), be32(.h), as.raw(c(8, 2, 0, 0, 0))), path)
  path
}

#' Write a header-only JPEG with the given pixel size and return its path.
#' @noRd
fake_jpeg <- function(.w, .h, .dir = withr::local_tempdir(.local_envir = parent.frame()), .name = "a.jpg") {
  path <- file.path(.dir, .name)
  writeBin(as.raw(c(0xff, 0xd8, 0xff, 0xe0, 0x00, 0x04, 0x00, 0x00,
                    0xff, 0xc0, 0x00, 0x11, 0x08, .h %/% 256, .h %% 256, .w %/% 256, .w %% 256,
                    3, 1, 0x22, 0, 2, 0x11, 1, 3, 0x11, 1, 0xff, 0xd9)), path)
  path
}

#' Write the given bytes to a temporary file and return its path.
#' @noRd
fake_file <- function(.name, .bytes = as.raw(1:32), .dir = withr::local_tempdir(.local_envir = parent.frame())) {
  path <- file.path(.dir, .name)
  writeBin(.bytes, path)
  path
}

#' Write a one-page PDF with a line of text and return its path.
#' @noRd
fake_pdf <- function(.dir = withr::local_tempdir(.local_envir = parent.frame())) {
  path <- file.path(.dir, "p.pdf")
  grDevices::pdf(path)
  graphics::plot.new()
  graphics::text(0.5, 0.5, "hello pdf")
  grDevices::dev.off()
  path
}

#' Provider metadata for a provider call.
#' @noRd
meta_of <- function(.provider) provider_metadata(.provider)

#' The dry-run httr2 request of decide() with fake keys.
#' @noRd
dry_body <- function(.provider, .state, .schema = tidyllm_schema(q = field_lgl("Is it a cat?"))) {
  withr::with_envvar(
    c(TYPESAFE_API_KEY = "x", OPENAI_API_KEY = "x", GOOGLE_API_KEY = "x"),
    decide(.state, .schema, .provider, .dry_run = TRUE)
  )
}

test_that("image_dims reads PNG and JPEG headers and rejects damaged files", {
  expect_equal(image_dims(fake_png(300, 200)), c(300L, 200L))
  expect_equal(image_dims(fake_jpeg(1500, 700)), c(1500L, 700L))
  expect_null(image_dims(fake_file("t.png", charToRaw("this is text, not an image"))))
  expect_null(image_dims(fake_file("t.png", as.raw(c(0x89, 0x50, 0x4e, 0x47, 1, 2)))))
  expect_null(image_dims(fake_file("t.jpg", as.raw(c(0xff, 0xd8, 0xff, 0xe0, 0, 4, 0, 0)))))
  expect_equal(image_kind(fake_file("t.jpg", charToRaw("plain text file"))), "unknown")
})

test_that("a JPEG with fill bytes before a marker keeps its size", {
  bytes <- as.raw(c(0xff, 0xd8, 0xff, 0xe0, 0x00, 0x04, 0x00, 0x00,
                    0xff, 0xff, 0xff, 0xc0, 0x00, 0x11, 0x08, 0x02, 0xbc, 0x05, 0xdc,
                    3, 1, 0x22, 0, 2, 0x11, 1, 3, 0x11, 1, 0xff, 0xd9))
  expect_equal(image_dims(fake_file("fill.jpg", bytes)), c(1500L, 700L))
  expect_null(image_dims(fake_file("fillonly.jpg", as.raw(c(0xff, 0xd8, rep(0xff, 20))))))
})

test_that("decide() on media with a provider that has no decide method names the verb", {
  png <- fake_png(300, 300)
  expect_error(decide(img(png), tidyllm_schema(q = field_lgl("x")), claude(), .dry_run = TRUE),
               "not available for the `claude\\(\\)` provider")
})

test_that("the empty-PDF message only names remedies that apply", {
  cases <- list(
    list(native = TRUE, forced = TRUE, has = "Drop `.text_extract = TRUE`", lacks = "gemini"),
    list(native = TRUE, forced = FALSE, has = "OCR", lacks = "gemini"),
    list(native = FALSE, forced = FALSE, has = "gemini\\(\\)", lacks = "Drop")
  )
  skip_if_not_installed("pdftools")
  for (case in cases) {
    parts <- list(pdf_file(fake_pdf(), .text_extract = case$forced))
    msg <- decide_pdf_empty_message(parts, case$native)
    expect_match(msg, case$has)
    expect_no_match(msg, case$lacks)
  }
})

test_that("decide_parts parses every accepted shape", {
  png_path <- fake_png(300, 300)
  wav_path <- fake_file("c.wav")
  cases <- list(
    string = list(input = "hello", n = 1, names = "", label = "hello"),
    image = list(input = img(png_path), n = 1, names = "", label = "[IMG] a.png"),
    unnamed = list(input = list("note", img(png_path)), n = 2, names = c("", ""), label = "note + [IMG] a.png"),
    named = list(input = list(first = img(png_path), second = "text"), n = 2,
                 names = c("first", "second"), label = "[IMG] a.png + text"),
    audio = list(input = audio_file(wav_path), n = 1, names = "", label = "[AUDIO] c.wav")
  )
  for (nm in names(cases)) {
    parsed <- decide_parts(cases[[nm]]$input)
    expect_length(parsed$parts, cases[[nm]]$n)
    expect_equal(parsed$names, cases[[nm]]$names, info = nm)
    expect_equal(parsed$label, cases[[nm]]$label, info = nm)
  }
  long <- decide_parts(strrep("x", 200))
  expect_equal(nchar(long$label), 60)
})

test_that("an LLMMessage gives its last user text and media", {
  png_path <- fake_png(300, 300)
  msg <- llm_message("Is it a cat?", .media = img(png_path))
  parsed <- decide_parts(msg)
  expect_equal(parsed$label, "Is it a cat? + [IMG] a.png")
  expect_equal(decide_types(parsed), c("text", "image"))
  expect_error(decide_parts(LLMMessage()), "user message")
})

test_that("decide_parts rejects malformed state before any request", {
  png_path <- fake_png(300, 300)
  gone <- img(png_path)
  unlink(png_path)
  bad <- list(
    null = NULL, na = NA_character_, empty = "", vector = c("a", "b"), number = 1,
    empty_list = list(), nested = list("a", list("b")), na_in_list = list("a", NA_character_),
    function_in_list = list("a", mean), file_object = tidyllm_file(id = "x", provider = "openai"),
    empty_text_part = list("a", ""), duplicate_names = list(a = "x", a = "y"),
    deleted_file = gone
  )
  for (nm in names(bad)) {
    expect_error(decide_parts(bad[[nm]]), "must be|names", info = nm)
  }
  expect_error(img(png_path), "does not exist")
})

test_that("check_decide_media names providers that can", {
  m <- list(png = img(fake_png(300, 300)), wav = audio_file(fake_file("c.wav")),
            mp4 = video_file(fake_file("c.mp4")))
  cases <- list(
    list(provider = openai(), state = m$wav, msg = "cannot decide on audio.*gemini"),
    list(provider = jev(), state = m$mp4, msg = "cannot decide on video.*gemini"),
    list(provider = ollama(), state = list("x", m$wav), msg = "cannot decide on audio")
  )
  for (case in cases) {
    expect_error(check_decide_media(decide_parts(case$state), meta_of(case$provider)), case$msg)
  }
  for (p in list(jev(), openai(), gemini(), ollama())) {
    expect_silent(check_decide_media(decide_parts(m$png), meta_of(p)))
  }
  expect_silent(check_decide_media(decide_parts(list(m$wav, m$mp4)), meta_of(gemini())))
})

test_that("a PDF becomes text where the backend cannot read it, and stays native on gemini", {
  skip_if_not_installed("pdftools")
  pdf <- pdf_file(fake_pdf())
  to_text <- decide_pdf_fallback(decide_parts(pdf), meta_of(openai()))
  expect_true(is.character(to_text$parts[[1]]))
  expect_match(to_text$parts[[1]], "hello pdf")
  native <- decide_pdf_fallback(decide_parts(pdf), meta_of(gemini()))
  expect_true(S7::S7_inherits(native$parts[[1]], tidyllm_pdf))
  forced <- decide_pdf_fallback(decide_parts(pdf_file(fake_pdf(), .text_extract = TRUE)), meta_of(gemini()))
  expect_true(is.character(forced$parts[[1]]))
})

test_that("limit checks stop before any request", {
  q <- decide_questions(tidyllm_schema(q = field_lgl("Is it a cat?")))
  many <- function(n) decide_questions(do.call(tidyllm_schema, stats::setNames(rep(list(field_lgl("x")), n), paste0("q", seq_len(n)))))
  cases <- list(
    jev_gif = list(provider = jev(), state = img(fake_file("c.gif")), msg = "GIF file; jev\\(\\) reads png"),
    jev_small = list(provider = jev(), state = img(fake_png(64, 64)), msg = "64 x 64 pixels"),
    jev_big = list(provider = jev(), state = img(fake_jpeg(2000, 1500)), msg = "2000 x 1500 pixels"),
    jev_max = list(provider = jev(), state = img(fake_png(1568, 1568)), msg = NA),
    jev_ok = list(provider = jev(), state = img(fake_png(300, 300)), msg = NA),
    jev_fake_png = list(provider = jev(), state = img(fake_file("t.png", charToRaw("text, not png"))), msg = "not a valid PNG"),
    jev_truncated = list(provider = jev(), state = img(fake_file("t.png", as.raw(c(0x89, 0x50, 0x4e, 0x47, 1)))), msg = "cannot be read"),
    jev_mismatch = list(provider = jev(), state = img(fake_png(300, 300, .name = "m.jpg")), msg = "not a valid JPG"),
    ollama_five = list(provider = ollama(),
                       state = rep(list(img(fake_png(300, 300))), 5), msg = "holds 5 images; ollama\\(\\) reads at most 4"),
    ollama_webp = list(provider = ollama(), state = img(fake_file("w.webp")), msg = NA),
    openai_any_size = list(provider = openai(), state = img(fake_png(64, 64)), msg = NA),
    gemini_bad_mime = list(provider = gemini(), state = audio_file(fake_file("x.xyz")), msg = "does not read as audio"),
    gemini_wrong_family = list(provider = gemini(), state = audio_file(fake_file("x.png")), msg = "does not read as audio"),
    gemini_ok = list(provider = gemini(), state = list(audio_file(fake_file("c.wav")), video_file(fake_file("c.mp4"))), msg = NA)
  )
  for (nm in names(cases)) {
    case <- cases[[nm]]
    run <- \() check_decide_state(decide_parts(case$state), q, meta_of(case$provider))
    if (is.na(case$msg)) expect_silent(run()) else expect_error(run(), case$msg, info = nm)
  }
  expect_error(check_decide_state(decide_parts("s"), many(401), meta_of(openai())), "at most 400 questions")
  expect_error(check_decide_state(decide_parts("s"), many(1001), meta_of(gemini())), "at most 1000 questions")
  expect_error(check_decide_state(decide_parts("s"), many(65), meta_of(ollama())), "at most 64 questions")
  bad_name <- decide_questions(tidyllm_schema(`bad key!` = field_lgl("x")))
  expect_error(check_decide_state(decide_parts("s"), bad_name, meta_of(gemini())), "bad key")
  expect_silent(check_decide_state(decide_parts("s"), bad_name, meta_of(openai())))
  too_many_levels <- decide_questions(tidyllm_schema(
    q = field_fct("x", .levels = letters[1:11], .ordered = TRUE)))
  expect_error(check_decide_state(decide_parts("s"), too_many_levels, meta_of(openai())), "score levels")
})

test_that("the decide media registry is validated and listed", {
  expect_error(create_provider_function("x", .decide_media = "files"), "decide_media")
  caps <- provider_capabilities(.what = "decide_media")
  expect_named(caps, c("provider", "media", "supported"))
  expect_setequal(caps$provider, c("jev", "openai", "gemini", "ollama"))
  supported <- \(p) caps$media[caps$provider == p & caps$supported]
  expect_equal(supported("jev"), "image")
  expect_equal(supported("openai"), "image")
  expect_equal(supported("ollama"), "image")
  expect_setequal(supported("gemini"), c("image", "audio", "video", "pdf"))
  expect_error(provider_capabilities(.what = "decide_media", .verb = "chat"), "arguments")
  expect_equal(unique(provider_capabilities(openai(), .what = "decide_media")$provider), "openai")
})

test_that("embed() still refuses PDF objects", {
  skip_if_not_installed("pdftools")
  expect_error(embed_items(list(pdf_file(fake_pdf()))), "single string")
})

test_that("text-only state keeps the plain body on every backend", {
  for (p in list(openai(), gemini())) {
    expect_equal(dry_body(p, "hello")$body$data$input, "hello")
  }
  for (p in list(jev(), ollama())) {
    body <- dry_body(p, "hello")$body$data
    expect_equal(body$state, "hello")
    expect_null(body$assets)
    expect_null(body$images)
  }
})

test_that("OpenAI builds a message of parts, with detail only when set", {
  a <- fake_jpeg(300, 300, .name = "a.jpg")
  state <- list(list(photo_a = img(a), "and some text"))
  input <- dry_body(openai(), state)$body$data$input
  expect_length(input, 1)
  expect_equal(input[[1]]$role, "user")
  content <- input[[1]]$content
  expect_equal(purrr::map_chr(content, "type"), c("input_text", "input_image", "input_text"))
  expect_equal(content[[1]]$text, "photo_a:")
  expect_match(content[[2]]$image_url, "^data:image/jpeg;base64,")
  expect_equal(sub("^data:image/jpeg;base64,", "", content[[2]]$image_url),
               base64enc::base64encode(a))
  expect_null(content[[2]]$detail)
  low <- dry_body(openai(.detail = "low"), state)$body$data$input[[1]]$content
  expect_equal(low[[2]]$detail, "low")
  expect_error(dry_body(openai(.detail = "tiny"), state), ".detail must be")
})

test_that("Gemini builds typed blocks with newlines, resolution only when set", {
  state <- list(list(photo_a = img(fake_png(300, 300)), clip = audio_file(fake_file("c.wav")),
                video_file(fake_file("c.mp4")), "closing note"))
  blocks <- dry_body(gemini(), state)$body$data$input
  types <- purrr::map_chr(blocks, "type")
  expect_equal(types, c("text", "image", "text", "audio", "video", "text"))
  expect_equal(blocks[[1]]$text, "photo_a:\n")
  expect_equal(blocks[[6]]$text, "closing note\n")
  expect_equal(blocks[[2]]$mime_type, "image/png")
  expect_equal(blocks[[4]]$mime_type, "audio/wav")
  expect_null(blocks[[2]]$resolution)
  low <- dry_body(gemini(.resolution = "low"), state)$body$data$input
  expect_equal(low[[2]]$resolution, "low")
  expect_equal(low[[5]]$resolution, "low")
  expect_null(low[[4]]$resolution)
  expect_error(dry_body(gemini(.resolution = "huge"), state), ".resolution must be")
})

test_that("Gemini sends a PDF as a document block", {
  skip_if_not_installed("pdftools")
  blocks <- dry_body(gemini(), pdf_file(fake_pdf()))$body$data$input
  expect_equal(blocks[[1]]$type, "document")
  expect_equal(blocks[[1]]$mime_type, "application/pdf")
})

test_that("Jev sends references in the state, assets beside it, and the images header", {
  a <- fake_png(300, 300, .name = "a.png")
  b <- fake_jpeg(400, 400, .name = "b.jpg")
  single <- dry_body(jev(), img(a))
  expect_equal(single$headers[["TypeSafe-Danger-Zone-Images"]], "true")
  body <- single$body$data
  expect_length(body$assets, 1)
  expect_equal(body$state, names(body$assets))
  expect_equal(body$assets[[1]]$format, "png")
  expect_equal(body$assets[[1]]$data_base64, base64enc::base64encode(a))

  multi <- dry_body(jev(), list(list(note = "Pet show.", first = img(a), img(b))))$body$data
  expect_equal(names(multi$state), c("note", "first", "part_3"))
  expect_equal(multi$state$note, "Pet show.")
  expect_setequal(unlist(multi$state[c("first", "part_3")]), names(multi$assets))
  expect_equal(multi$assets[[multi$state$part_3]]$format, "jpg")
  expect_false(is.null(dry_body(jev(), list(list(img(a), "text")))$headers[["TypeSafe-Danger-Zone-Images"]]))

  text_only <- dry_body(jev(), list(list(a = "x", b = "y")))
  expect_null(text_only$headers[["TypeSafe-Danger-Zone-Images"]])
  expect_null(text_only$body$data$assets)
  expect_equal(text_only$body$data$state, list(a = "x", b = "y"))
})

test_that("Ollama sends raw base64 in an images array and marks images in the text", {
  a <- fake_png(300, 300, .name = "a.png")
  body <- dry_body(ollama(), list(list(photo = img(a), "second text", img(a))))$body$data
  expect_equal(body$state, "photo: [image 1]\nsecond text\n[image 2]")
  expect_length(body$images, 2)
  expect_equal(body$images[[1]], base64enc::base64encode(a))
  expect_false(grepl("^data:", body$images[[1]]))
})

test_that("an image in a pipe reaches decide() as the first argument", {
  req <- withr::with_envvar(c(OPENAI_API_KEY = "x"),
    img(fake_png(300, 300)) |>
      decide(tidyllm_schema(q = field_lgl("Is it a cat?")), openai(), .dry_run = TRUE))
  expect_equal(req$body$data$input[[1]]$content[[1]]$type, "input_image")
})

test_that("yes/no definitions reach each wire format", {
  schema <- tidyllm_schema(q = field_lgl("Is it a cat?", .true = "A cat is shown", .false = "No cat"))
  for (p in list(jev(), ollama())) {
    expect_equal(dry_body(p, "s", schema)$body$data$questions$q$criteria, list(true = "A cat is shown", false = "No cat"))
  }
  expect_equal(dry_body(gemini(), "s", schema)$body$data$questions$q$criteria,
               list(true_description = "A cat is shown", false_description = "No cat"))
  text <- dry_body(openai(), "s", schema)$body$data$questions[[1]]$instructions
  expect_match(text, "Is it a cat\\? Yes means: A cat is shown No means: No cat")
  plain <- dry_body(jev(), "s", tidyllm_schema(q = field_lgl("Is it a cat?")))$body$data$questions$q
  expect_null(plain$criteria)
})

test_that("field_lgl validates its definitions", {
  bad <- list(only_true = list(.true = "yes"), only_false = list(.false = "no"),
              number = list(.true = 1, .false = "no"), empty = list(.true = "", .false = "no"),
              vector = list(.true = c("a", "b"), .false = "no"))
  for (nm in names(bad)) {
    expect_error(do.call(field_lgl, c(list("q"), bad[[nm]])), "\\.true|both", info = nm)
  }
  expect_silent(field_lgl("q", .true = "a", .false = "b"))
  plain <- tidyllm_schema(q = field_lgl("q"))
  expect_null(attr(plain, "decide_meta"))
  json <- jsonlite::toJSON(unclass(tidyllm_schema(q = field_lgl("q", .true = "a", .false = "b"))), auto_unbox = TRUE)
  expect_false(grepl("description\":\"a|true_description", json))
})

test_that("provider_capabilities() returns the documented columns for every provider", {
  caps <- provider_capabilities()
  expect_s3_class(caps, "tbl_df")
  expect_named(caps, c("provider", "verb", "argument", "default", "fn"))
  expect_setequal(unique(caps$provider), tidyllm:::all_provider_names())
  expect_false("build" %in% caps$verb)
  expect_true("build" %in% provider_capabilities(.internal = TRUE)$verb)
})

test_that("a provider call, a name and the function give the same rows", {
  from_call <- provider_capabilities(claude(.model = "x"), .verb = "chat")
  expect_identical(from_call, provider_capabilities("claude", .verb = "chat"))
  expect_identical(from_call, provider_capabilities(claude, .verb = "chat"))
  expect_error(provider_capabilities("nonsense"), "not a tidyllm provider")
})

test_that("rows come from the provider's own function formals", {
  rows <- provider_capabilities("claude", .verb = "chat")
  expect_identical(rows$argument, names(formals(claude_chat)))
  expect_identical(rows$fn, rep("claude_chat", nrow(rows)))
  expect_identical(rows$default[rows$argument == ".thinking"], "FALSE")
  expect_true(is.na(rows$default[rows$argument == ".llm"]))
})

test_that("every registered verb function exists and every registry entry is consistent", {
  for (p in tidyllm:::all_provider_names()) {
    meta <- tidyllm:::provider_metadata(p)
    expect_identical(names(meta$supported_args), names(meta$functions))
    expect_identical(names(meta$supported_defaults), names(meta$supported_args))
    for (v in names(meta$functions)) {
      fn <- get(meta$functions[[v]], envir = asNamespace("tidyllm"))
      expect_identical(names(formals(fn)), meta$supported_args[[v]])
    }
  }
})

test_that("filters on verb and argument narrow the table", {
  thinking <- provider_capabilities(.argument = ".thinking")
  expect_true(all(thinking$argument == ".thinking"))
  expect_true("claude" %in% thinking$provider)
  expect_equal(unique(provider_capabilities(.verb = "embed")$verb), "embed")
})

test_that("media rows list every media type per provider", {
  media <- provider_capabilities(.what = "media")
  expect_named(media, c("provider", "media", "supported"))
  expect_equal(nrow(media), length(tidyllm:::all_provider_names()) * length(tidyllm:::MEDIA_TYPES))
  supported <- function(p, m) media$supported[media$provider == p & media$media == m]
  expect_true(supported("gemini", "video"))
  expect_false(supported("claude", "audio"))
  expect_true(supported("claude", "files"))
  expect_true(supported("chat_completions", "audio"))
  expect_error(provider_capabilities(.what = "media", .verb = "chat"), "apply to")
})

test_that("the attachment check reads the media registry", {
  wav <- withr::local_tempfile(fileext = ".wav")
  writeBin(as.raw(0:10), wav)
  with_audio <- function() llm_message("hi", .media = list(audio_file(wav)))
  expect_error(tidyllm:::validate_message_attachments(with_audio(), "claude"), "inline audio")
  expect_error(tidyllm:::validate_message_attachments(with_audio(), claude()), "inline audio")
  expect_no_error(tidyllm:::validate_message_attachments(with_audio(), "gemini"))
  expect_no_error(tidyllm:::validate_message_attachments(with_audio(), gemini()))
})

test_that("the attachment check rejects images for providers without image support", {
  png <- withr::local_tempfile(fileext = ".png")
  writeBin(as.raw(0:10), png)
  with_image <- llm_message("hi", .media = list(img(png)))
  for (provider in c("claude_cli", "codex_cli", "vibe_cli", "deepseek")) {
    expect_error(tidyllm:::validate_message_attachments(with_image, provider), "does not support images")
  }
  for (provider in c("claude", "gemini", "openai", "ellmer")) {
    expect_no_error(tidyllm:::validate_message_attachments(with_image, provider))
  }
})

test_that("create_provider_function rejects unknown media types", {
  expect_error(
    tidyllm:::create_provider_function(.name = "x", chat = claude_chat, .media = "hologram"),
    "must contain only"
  )
})

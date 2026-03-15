# local_tests/features/gemini_file_api.R
# Tests for Gemini Files API: upload, inject into chat, metadata, list, delete.
#
# What this tests:
#   - gemini_upload_file() returns a tibble with uri, name, state
#   - Uploaded file can be used in a chat via .fileid
#   - Model can describe/answer questions about file content
#   - gemini_file_metadata() retrieves correct info
#   - gemini_list_files() includes the uploaded file
#   - gemini_delete_file() cleans up (always runs via on.exit)
#
# Uses local_tests/media/ test assets (PDF and image).

devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("gemini_file_api")

# ── PDF upload + chat ─────────────────────────────────────────────────────────

llt_test("upload PDF returns tibble with required fields", {
  info <- gemini_upload_file("local_tests/media/ssrn-4983498.pdf")
  llt_expect_true(tibble::is_tibble(info), "Should return a tibble")
  llt_expect_true("name" %in% names(info), "Should have name column")
  llt_expect_true("uri" %in% names(info), "Should have uri column")
  llt_expect_true("state" %in% names(info), "Should have state column")
  llt_expect_true(nzchar(info$name), "name should be non-empty")
  on.exit(tryCatch(gemini_delete_file(info$name), error = function(e) NULL))
})

llt_test("uploaded PDF can be used in chat via .fileid", {
  info <- gemini_upload_file("local_tests/media/ssrn-4983498.pdf")
  on.exit(tryCatch(gemini_delete_file(info$name), error = function(e) NULL))

  result <- llm_message("What is the title of this document? Give the title only.") |>
    chat(gemini(.fileid = info$name))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

# ── Image upload + chat ───────────────────────────────────────────────────────

llt_test("upload image returns tibble with uri", {
  info <- gemini_upload_file("local_tests/media/hotdog.jpg")
  on.exit(tryCatch(gemini_delete_file(info$name), error = function(e) NULL))
  llt_expect_true("uri" %in% names(info), "Should have uri")
  llt_expect_true(grepl("^https://", info$uri), "URI should be an https URL")
})

llt_test("uploaded image chat: model identifies hotdog content", {
  info <- gemini_upload_file("local_tests/media/hotdog.jpg")
  on.exit(tryCatch(gemini_delete_file(info$name), error = function(e) NULL))

  result <- llm_message("Describe what food is in this image in one word.") |>
    chat(gemini(.fileid = info$name))
  reply <- tolower(get_reply(result))
  llt_expect_true(
    grepl("hotdog|hot dog|sausage|frankfurter|bun", reply),
    paste("Expected hotdog-related word, got:", reply)
  )
})

# ── File management ───────────────────────────────────────────────────────────

llt_test("gemini_file_metadata returns correct info for uploaded file", {
  info <- gemini_upload_file("local_tests/media/dog.jpg")
  on.exit(tryCatch(gemini_delete_file(info$name), error = function(e) NULL))

  meta <- gemini_file_metadata(info$name)
  llt_expect_true(tibble::is_tibble(meta), "Should return tibble")
  llt_expect_true(meta$name == info$name, "name should match uploaded file")
  llt_expect_true(grepl("image", meta$mime_type), "mime_type should be image/*")
})

llt_test("gemini_list_files returns tibble including uploaded file", {
  info <- gemini_upload_file("local_tests/media/dog.jpg")
  on.exit(tryCatch(gemini_delete_file(info$name), error = function(e) NULL))

  files <- gemini_list_files(.page_size = 50)
  llt_expect_true(tibble::is_tibble(files), "Should return tibble")
  llt_expect_true(info$name %in% files$name, "Uploaded file should appear in list")
})

llt_test("gemini_delete_file removes file from listing", {
  info <- gemini_upload_file("local_tests/media/dog.jpg")

  gemini_delete_file(info$name)
  files <- gemini_list_files(.page_size = 100)
  llt_expect_true(
    !info$name %in% files$name,
    "Deleted file should not appear in listing"
  )
})

llt_report()

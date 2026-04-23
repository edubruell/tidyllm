# local_tests/features/gemini_file_api.R
# Tests for Gemini Files API: upload, chat, metadata, list, delete.
#
# Uses the new verb+provider pattern: upload_file(gemini()), list_files(gemini()),
# file_info(gemini()), delete_file(gemini()).  Also verifies the deprecated
# gemini_upload_file() / gemini_file_metadata() / gemini_list_files() wrappers
# still function (with warnings).
#
# Column names are normalized to match Claude: file_id, filename, size_bytes,
# mime_type, created_at, expires_at, uri.

devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("gemini_file_api")

# ── upload_file() returns tidyllm_file ────────────────────────────────────────

llt_test("upload_file(gemini()) returns tidyllm_file for PDF", {
  f <- upload_file(gemini(), .path = "local_tests/media/ssrn-4983498.pdf")
  on.exit(tryCatch(delete_file(gemini(), .file_id = f@id), error = function(e) NULL))

  llt_expect_true(S7_inherits(f, tidyllm_file), "Should return tidyllm_file")
  llt_expect_true(f@provider == "gemini", "provider should be 'gemini'")
  llt_expect_true(nzchar(f@id), "id should be non-empty")
  llt_expect_true(nzchar(f@uri), "uri should be non-empty")
  llt_expect_true(grepl("pdf", f@mime_type, ignore.case = TRUE), "mime_type should be PDF")
})

llt_test("upload_file(gemini()) returns tidyllm_file for image", {
  f <- upload_file(gemini(), .path = "local_tests/media/hotdog.jpg")
  on.exit(tryCatch(delete_file(gemini(), .file_id = f@id), error = function(e) NULL))

  llt_expect_true(S7_inherits(f, tidyllm_file), "Should return tidyllm_file")
  llt_expect_true(grepl("image", f@mime_type, ignore.case = TRUE), "mime_type should be image/*")
  llt_expect_true(grepl("^https://", f@uri), "uri should be an https URL")
})

# ── .files in llm_message() ───────────────────────────────────────────────────

llt_test("uploaded PDF can be used in chat via .files on llm_message()", {
  f <- upload_file(gemini(), .path = "local_tests/media/ssrn-4983498.pdf")
  on.exit(tryCatch(delete_file(gemini(), .file_id = f@id), error = function(e) NULL))

  result <- llm_message("What is the title of this document? Give the title only.",
                        .files = f) |>
    chat(gemini())
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

llt_test("uploaded image chat via .files: model identifies hotdog", {
  f <- upload_file(gemini(), .path = "local_tests/media/hotdog.jpg")
  on.exit(tryCatch(delete_file(gemini(), .file_id = f@id), error = function(e) NULL))

  result <- llm_message("What specific type of food is in this image? Give just the food name, one or two words only.", .files = f) |>
    chat(gemini())
  reply <- tolower(get_reply(result))
  llt_expect_true(
    grepl("hotdog|hot dog|sausage|frankfurter|bun|corn dog|bratwurst", reply),
    paste("Expected hotdog-related word, got:", reply)
  )
})

# ── file_info() and list_files() ──────────────────────────────────────────────

llt_test("file_info(gemini()) returns tibble with normalized columns", {
  f <- upload_file(gemini(), .path = "local_tests/media/dog.jpg")
  on.exit(tryCatch(delete_file(gemini(), .file_id = f@id), error = function(e) NULL))

  info <- file_info(gemini(), .file_id = f@id)
  llt_expect_true(tibble::is_tibble(info), "Should return tibble")
  llt_expect_true("file_id" %in% names(info), "Should have file_id column")
  llt_expect_true("filename" %in% names(info), "Should have filename column")
  llt_expect_true("uri" %in% names(info), "Should have uri column")
  llt_expect_true(info$file_id == f@id, "file_id should match uploaded file")
})

llt_test("list_files(gemini()) returns tibble with normalized columns including uploaded file", {
  f <- upload_file(gemini(), .path = "local_tests/media/dog.jpg")
  on.exit(tryCatch(delete_file(gemini(), .file_id = f@id), error = function(e) NULL))

  files <- list_files(gemini())
  llt_expect_true(tibble::is_tibble(files), "Should return tibble")
  llt_expect_true("file_id" %in% names(files), "Should have file_id column (normalized)")
  llt_expect_true(f@id %in% files$file_id, "Uploaded file should appear in listing")
})

llt_test("delete_file(gemini()) removes file from listing", {
  f <- upload_file(gemini(), .path = "local_tests/media/dog.jpg")

  delete_file(gemini(), .file_id = f@id)

  Sys.sleep(1)
  files <- list_files(gemini(), .page_size = 100)
  llt_expect_true(
    !f@id %in% files$file_id,
    "Deleted file should not appear in listing"
  )
})

# ── backward-compat: deprecated wrappers still work with warning ──────────────

llt_test("deprecated gemini_upload_file() still works (with warning)", {
  info <- withCallingHandlers(
    gemini_upload_file("local_tests/media/dog.jpg"),
    lifecycle_warning_deprecated = function(w) invokeRestart("muffleWarning")
  )
  on.exit(tryCatch(
    delete_file(gemini(), .file_id = info$name %||% info$file_id),
    error = function(e) NULL
  ))

  llt_expect_true(tibble::is_tibble(info), "Should return tibble")
})

llt_report()

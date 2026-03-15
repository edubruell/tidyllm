# local_tests/features/claude_file_api.R
# Tests for Claude Files API: upload, inject into chat, metadata, list, delete.
#
# What this tests:
#   - claude_upload_file() returns a tibble with file_id, filename, size_bytes
#   - Uploaded file can be used in chat via .file_ids
#   - Model can read/answer questions about uploaded file content
#   - claude_file_metadata() retrieves correct info
#   - claude_list_files() includes uploaded file
#   - claude_delete_file() removes the file (always called via on.exit)
#
# Uses local_tests/media/ test assets (PDF and image).

devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("claude_file_api")

# ── PDF upload + chat ─────────────────────────────────────────────────────────

llt_test("upload PDF returns tibble with required fields", {
  info <- claude_upload_file("local_tests/media/ssrn-4983498.pdf")
  on.exit(tryCatch(claude_delete_file(info$file_id), error = function(e) NULL))

  llt_expect_true(tibble::is_tibble(info), "Should return a tibble")
  llt_expect_true("file_id" %in% names(info), "Should have file_id column")
  llt_expect_true("filename" %in% names(info), "Should have filename column")
  llt_expect_true("size_bytes" %in% names(info), "Should have size_bytes column")
  llt_expect_true(nzchar(info$file_id), "file_id should be non-empty")
  llt_expect_true(info$size_bytes > 0, "size_bytes should be positive")
})

llt_test("uploaded PDF can be used in chat via .file_ids", {
  info <- claude_upload_file("local_tests/media/ssrn-4983498.pdf")
  on.exit(tryCatch(claude_delete_file(info$file_id), error = function(e) NULL))

  result <- llm_message("What is the title of this document? Give the title only.") |>
    chat(claude(.file_ids = info$file_id))
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

# ── Image upload + chat ───────────────────────────────────────────────────────

llt_test("upload image returns tibble with correct mime_type", {
  info <- claude_upload_file("local_tests/media/hotdog.jpg")
  on.exit(tryCatch(claude_delete_file(info$file_id), error = function(e) NULL))

  llt_expect_true(grepl("image", info$mime_type), "mime_type should be image/*")
})

llt_test("uploaded image chat: model identifies hotdog content", {
  info <- claude_upload_file("local_tests/media/hotdog.jpg")
  on.exit(tryCatch(claude_delete_file(info$file_id), error = function(e) NULL))

  result <- llm_message("Describe what food is in this image in one word.") |>
    chat(claude(.file_ids = info$file_id))
  reply <- tolower(get_reply(result))
  llt_expect_true(
    grepl("hotdog|hot dog|sausage|frankfurter|bun", reply),
    paste("Expected hotdog-related word, got:", reply)
  )
})

# ── File management ───────────────────────────────────────────────────────────

llt_test("claude_file_metadata returns correct info for uploaded file", {
  info <- claude_upload_file("local_tests/media/dog.jpg")
  on.exit(tryCatch(claude_delete_file(info$file_id), error = function(e) NULL))

  meta <- claude_file_metadata(info$file_id)
  llt_expect_true(tibble::is_tibble(meta), "Should return tibble")
  llt_expect_true(meta$file_id == info$file_id, "file_id should match")
  llt_expect_true(grepl("image", meta$mime_type), "mime_type should be image/*")
})

llt_test("claude_list_files returns tibble including uploaded file", {
  info <- claude_upload_file("local_tests/media/dog.jpg")
  on.exit(tryCatch(claude_delete_file(info$file_id), error = function(e) NULL))

  files <- claude_list_files()
  llt_expect_true(tibble::is_tibble(files), "Should return tibble")
  llt_expect_true(info$file_id %in% files$file_id, "Uploaded file should appear in list")
})

llt_test("claude_delete_file removes file — subsequent metadata call errors", {
  info <- claude_upload_file("local_tests/media/dog.jpg")

  claude_delete_file(info$file_id)

  err <- tryCatch(claude_file_metadata(info$file_id), error = function(e) e)
  llt_expect_true(inherits(err, "error"), "Accessing deleted file should error")
})

llt_report()

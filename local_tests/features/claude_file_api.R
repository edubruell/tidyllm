# local_tests/features/claude_file_api.R
# Tests for Claude Files API: upload, chat, metadata, list, delete.
#
# Uses the new verb+provider pattern: upload_file(claude()), list_files(claude()),
# file_info(claude()), delete_file(claude()).  Also verifies the deprecated
# claude_upload_file() / claude_file_metadata() / claude_list_files() wrappers
# still function (with warnings).

devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("claude_file_api")

# ── upload_file() returns tidyllm_file ────────────────────────────────────────

llt_test("upload_file(claude()) returns tidyllm_file for PDF", {
  f <- upload_file(claude(), .path = "local_tests/media/ssrn-4983498.pdf")
  on.exit(tryCatch(delete_file(claude(), .file_id = f@id), error = function(e) NULL))

  llt_expect_true(S7_inherits(f, tidyllm_file), "Should return tidyllm_file")
  llt_expect_true(f@provider == "claude", "provider should be 'claude'")
  llt_expect_true(nzchar(f@id), "id should be non-empty")
  llt_expect_true(nzchar(f@filename), "filename should be non-empty")
  llt_expect_true(grepl("pdf", f@mime_type, ignore.case = TRUE), "mime_type should be PDF")
})

llt_test("upload_file(claude()) returns tidyllm_file for image", {
  f <- upload_file(claude(), .path = "local_tests/media/hotdog.jpg")
  on.exit(tryCatch(delete_file(claude(), .file_id = f@id), error = function(e) NULL))

  llt_expect_true(S7_inherits(f, tidyllm_file), "Should return tidyllm_file")
  llt_expect_true(grepl("image", f@mime_type, ignore.case = TRUE), "mime_type should be image/*")
})

# ── .files in llm_message() ───────────────────────────────────────────────────

llt_test("uploaded PDF can be used in chat via .files on llm_message()", {
  f <- upload_file(claude(), .path = "local_tests/media/ssrn-4983498.pdf")
  on.exit(tryCatch(delete_file(claude(), .file_id = f@id), error = function(e) NULL))

  result <- llm_message("What is the title of this document? Give the title only.",
                        .files = f) |>
    chat(claude())
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

llt_test("uploaded image chat via .files: model identifies hotdog", {
  f <- upload_file(claude(), .path = "local_tests/media/hotdog.jpg")
  on.exit(tryCatch(delete_file(claude(), .file_id = f@id), error = function(e) NULL))

  result <- llm_message("Describe the food in this image in one word.", .files = f) |>
    chat(claude())
  reply <- tolower(get_reply(result))
  llt_expect_true(
    grepl("hotdog|hot dog|sausage|frankfurter|bun", reply),
    paste("Expected hotdog-related word, got:", reply)
  )
})

# ── file_info() and list_files() ──────────────────────────────────────────────

llt_test("file_info(claude()) returns tibble with matching file_id", {
  f <- upload_file(claude(), .path = "local_tests/media/dog.jpg")
  on.exit(tryCatch(delete_file(claude(), .file_id = f@id), error = function(e) NULL))

  info <- file_info(claude(), .file_id = f@id)
  llt_expect_true(tibble::is_tibble(info), "Should return tibble")
  llt_expect_true(info$file_id == f@id, "file_id should match uploaded file")
  llt_expect_true(grepl("image", info$mime_type), "mime_type should be image/*")
})

llt_test("list_files(claude()) returns tibble including uploaded file", {
  f <- upload_file(claude(), .path = "local_tests/media/dog.jpg")
  on.exit(tryCatch(delete_file(claude(), .file_id = f@id), error = function(e) NULL))

  files <- list_files(claude())
  llt_expect_true(tibble::is_tibble(files), "Should return tibble")
  llt_expect_true(f@id %in% files$file_id, "Uploaded file should appear in listing")
})

llt_test("delete_file(claude()) removes file — subsequent file_info() errors", {
  f <- upload_file(claude(), .path = "local_tests/media/dog.jpg")

  delete_file(claude(), .file_id = f@id)

  err <- tryCatch(file_info(claude(), .file_id = f@id), error = function(e) e)
  llt_expect_true(inherits(err, "error"), "Accessing deleted file should error")
})

# ── backward-compat: deprecated wrappers still work with warning ──────────────

llt_test("deprecated claude_upload_file() still works (with warning)", {
  info <- withCallingHandlers(
    claude_upload_file("local_tests/media/dog.jpg"),
    lifecycle_warning_deprecated = function(w) invokeRestart("muffleWarning")
  )
  on.exit(tryCatch(delete_file(claude(), .file_id = info$file_id), error = function(e) NULL))

  llt_expect_true(tibble::is_tibble(info), "Should return tibble")
  llt_expect_true("file_id" %in% names(info), "Should have file_id column")
})

llt_test("deprecated .file_ids= still works in chat (with warning)", {
  info <- withCallingHandlers(
    claude_upload_file("local_tests/media/ssrn-4983498.pdf"),
    lifecycle_warning_deprecated = function(w) invokeRestart("muffleWarning")
  )
  on.exit(tryCatch(delete_file(claude(), .file_id = info$file_id), error = function(e) NULL))

  result <- withCallingHandlers(
    llm_message("What is the title of this document? One sentence.") |>
      chat(claude(.file_ids = info$file_id)),
    lifecycle_warning_deprecated = function(w) invokeRestart("muffleWarning")
  )
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

llt_report()

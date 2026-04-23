# local_tests/features/openai_file_api.R
# Tests for OpenAI Files API: upload, chat via .files, metadata, list, delete.
#
# Uses the new verb+provider pattern: upload_file(openai()), list_files(openai()),
# file_info(openai()), delete_file(openai()).
#
# Note: OpenAI Responses API supports input_file blocks for PDFs and images
# when the file is uploaded with purpose = "assistants".

devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("openai_file_api")

# ── upload_file() returns tidyllm_file ────────────────────────────────────────

llt_test("upload_file(openai()) returns tidyllm_file for PDF", {
  f <- upload_file(openai(), .path = "local_tests/media/ssrn-4983498.pdf")
  on.exit(tryCatch(delete_file(openai(), .file_id = f@id), error = function(e) NULL))

  llt_expect_true(S7_inherits(f, tidyllm_file), "Should return tidyllm_file")
  llt_expect_true(f@provider == "openai", "provider should be 'openai'")
  llt_expect_true(nzchar(f@id), "id should be non-empty")
  llt_expect_true(nzchar(f@filename), "filename should be non-empty")
})

# ── .files in llm_message() ───────────────────────────────────────────────────

llt_test("uploaded PDF can be used in chat via .files on llm_message()", {
  f <- upload_file(openai(), .path = "local_tests/media/ssrn-4983498.pdf")
  on.exit(tryCatch(delete_file(openai(), .file_id = f@id), error = function(e) NULL))

  result <- llm_message("What is the title of this document? Give the title only.",
                        .files = f) |>
    chat(openai())
  llt_expect_s7(result, LLMMessage)
  llt_expect_reply(result)
})

# ── file_info() and list_files() ──────────────────────────────────────────────

llt_test("file_info(openai()) returns tibble with expected columns", {
  f <- upload_file(openai(), .path = "local_tests/media/dog.jpg")
  on.exit(tryCatch(delete_file(openai(), .file_id = f@id), error = function(e) NULL))

  info <- file_info(openai(), .file_id = f@id)
  llt_expect_true(tibble::is_tibble(info), "Should return tibble")
  llt_expect_true("file_id" %in% names(info), "Should have file_id column")
  llt_expect_true("filename" %in% names(info), "Should have filename column")
  llt_expect_true("size_bytes" %in% names(info), "Should have size_bytes column")
  llt_expect_true(info$file_id == f@id, "file_id should match uploaded file")
})

llt_test("list_files(openai()) returns tibble including uploaded file", {
  f <- upload_file(openai(), .path = "local_tests/media/dog.jpg")
  on.exit(tryCatch(delete_file(openai(), .file_id = f@id), error = function(e) NULL))

  files <- list_files(openai())
  llt_expect_true(tibble::is_tibble(files), "Should return tibble")
  llt_expect_true("file_id" %in% names(files), "Should have file_id column")
  llt_expect_true(f@id %in% files$file_id, "Uploaded file should appear in listing")
})

llt_test("delete_file(openai()) removes file — subsequent file_info() errors", {
  f <- upload_file(openai(), .path = "local_tests/media/dog.jpg")

  delete_file(openai(), .file_id = f@id)

  err <- tryCatch(file_info(openai(), .file_id = f@id), error = function(e) e)
  llt_expect_true(inherits(err, "error"), "Accessing deleted file should error")
})

llt_report()

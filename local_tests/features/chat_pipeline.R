# Source-level checks on the build / perform / finish split.
#
# The bulk of this suite moved to tests/testthat/test_chat_pipeline.R, where
# R CMD check runs it: it is pure introspection and needs no key, no network and
# no mock, so there was no reason for the invariants that make the split safe to
# depend on someone remembering to run a local file.
#
# What stays here is the one check that cannot: it greps the package sources in
# R/, which are not present when tests run against an installed package.
#
#   Rscript -e 'devtools::load_all("."); source("local_tests/features/chat_pipeline.R")'

source("local_tests/test_harness.R")

llt_suite("chat_pipeline")

# Providers allowed to call perform_chat_request() directly, with the reason.
# Matching on filename keeps the check simple; when a second provider needs a
# custom perform step, add it here rather than loosening the pattern.
PERFORM_FN_PROVIDERS <- c(
  api_openai.R = "stateful mode retries against a rebuilt body in a .perform_fn"
)

llt_test("no provider performs its own request outside a .perform_fn", {
  for (f in list.files("R", pattern = "^api_.*\\.R$", full.names = TRUE)) {
    src  <- readLines(f, warn = FALSE)
    hits <- grep("perform_chat_request\\(", src, value = TRUE)
    hits <- hits[!grepl("^\\s*#", hits)]
    if (length(hits) == 0) next

    reason <- PERFORM_FN_PROVIDERS[[basename(f)]]
    llt_expect_true(!is.null(reason),
                    sprintf("%s calls perform_chat_request() directly:\n  %s\nIf that is deliberate, add it to PERFORM_FN_PROVIDERS with a reason.",
                            basename(f), paste(hits, collapse = "\n  ")))
  }
})

llt_test("the shared pipeline is what providers actually return", {
  # A builder that forgot to end in new_chat_request() would return whatever its
  # last expression produced, and only fail later inside finish_chat_response().
  for (f in list.files("R", pattern = "^api_.*\\.R$", full.names = TRUE)) {
    src <- readLines(f, warn = FALSE)
    if (!any(grepl("_build_chat_request <- function", src))) next
    llt_expect_true(any(grepl("new_chat_request\\(", src)),
                    sprintf("%s defines a builder but never calls new_chat_request()",
                            basename(f)))
  }
})

llt_report("chat_pipeline")

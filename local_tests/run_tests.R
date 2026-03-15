# local_tests/run_tests.R
# Main entry point for local live-API test suites.
#
# Sync provider suites (fast, no batch API calls):
#   source("local_tests/providers/claude.R")       # single provider
#   run_all_local_tests()                           # all providers
#   run_all_local_tests(c("claude", "openai"))      # subset
#
# Provider feature tests (opt-in, may upload/delete files or use paid APIs):
#   source("local_tests/features/gemini_grounding.R")
#   source("local_tests/features/gemini_file_api.R")
#   source("local_tests/features/claude_file_api.R")
#   run_feature_tests()                             # all feature suites
#
# Async batch workflow (manual, opt-in):
#   source("local_tests/batch_submit.R")            # submit — skips if already pending
#   source("local_tests/async_tracker.R")
#   check_all_async_jobs()                          # poll until all show "completed"
#   source("local_tests/batch_fetch.R")             # fetch and assert

run_all_local_tests <- function(providers = NULL) {
  all_providers <- c(
    "claude", "openai", "gemini", "ollama",
    "mistral", "groq", "perplexity", "deepseek",
    "voyage", "azure_openai"
  )
  targets <- providers %||% all_providers

  for (p in targets) {
    path <- file.path("local_tests/providers", paste0(p, ".R"))
    if (!file.exists(path)) {
      cat(sprintf("  [skip] %s — no suite at %s\n", p, path))
      next
    }
    source(path)
  }
}

run_feature_tests <- function(features = NULL) {
  all_features <- c(
    "gemini_grounding",
    "gemini_file_api",
    "claude_file_api"
  )
  targets <- features %||% all_features

  for (f in targets) {
    path <- file.path("local_tests/features", paste0(f, ".R"))
    if (!file.exists(path)) {
      cat(sprintf("  [skip] %s — no suite at %s\n", f, path))
      next
    }
    source(path)
  }
}

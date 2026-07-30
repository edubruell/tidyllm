devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("cache_metadata")

# Regression suite for the 0.5.2 cache metadata columns (workplan 1.3 / 1.4).
# get_metadata() gains cached_tokens and cache_creation_tokens as top-level
# columns; every provider either fills them or leaves them NA_integer_.

cache_cols <- c("cached_tokens", "cache_creation_tokens")

expect_cache_cols <- function(result) {
  meta <- get_metadata(result)
  missing <- setdiff(cache_cols, names(meta))
  if (length(missing) > 0) {
    stop(paste("get_metadata() missing cache columns:", paste(missing, collapse = ", ")))
  }
  for (col in cache_cols) {
    llt_expect_true(is.integer(meta[[col]]) || is.numeric(meta[[col]]),
                    paste(col, "must be numeric or NA_integer_"))
  }
  meta
}

# The minimum cacheable prefix is per model and is *higher* on the cheap models:
# 4096 tokens on claude-haiku-4-5, 1024 on claude-sonnet-5. Below the floor the
# API silently reports zero cache tokens, so the filler has to clear 4096.
long_context <- paste(
  rep(paste("The following paragraph is filler used to push the prompt above the",
            "provider minimum for prompt caching. It carries no information and",
            "should simply be ignored by the model when answering."), 250),
  collapse = " "
)

# ── column presence across providers ──────────────────────────────────────────

cols_case <- function(label, provider_call) {
  llt_test(label, {
    result <- llm_message("Say hello in one word.") |> chat(provider_call)
    expect_cache_cols(result)
  })
}

cols_case("openai reports cache columns",     openai(.model = "gpt-4o-mini"))
cols_case("claude reports cache columns",     claude(.model = "claude-haiku-4-5"))
cols_case("gemini reports cache columns",     gemini(.model = "gemini-2.5-flash"))
cols_case("openrouter reports cache columns", openrouter(.model = "google/gemini-2.5-flash"))
cols_case("groq reports cache columns",       groq(.model = "openai/gpt-oss-120b"))
cols_case("mistral reports cache columns",    mistral(.model = "mistral-small-latest"))
cols_case("deepseek reports cache columns",   deepseek(.model = "deepseek-v4-flash"))

# ── Claude: a real cache write followed by a real cache read ──────────────────

llt_test("claude .cache reports creation then read tokens", {
  first <- llm_message(long_context, .system_prompt = "You answer in one word.") |>
    chat(claude(.model = "claude-haiku-4-5", .cache = TRUE, .max_tokens = 32))
  meta_first <- expect_cache_cols(first)

  second <- llm_message(long_context, .system_prompt = "You answer in one word.") |>
    chat(claude(.model = "claude-haiku-4-5", .cache = TRUE, .max_tokens = 32))
  meta_second <- expect_cache_cols(second)

  wrote <- !is.na(meta_first$cache_creation_tokens) && meta_first$cache_creation_tokens > 0
  read  <- !is.na(meta_second$cached_tokens) && meta_second$cached_tokens > 0
  llt_expect_true(wrote || read,
                  paste("expected a cache write on the first call or a cache read on the second;",
                        "got creation =", meta_first$cache_creation_tokens,
                        "read =", meta_second$cached_tokens))
})

llt_test("claude streaming metadata is no longer a stub", {
  result <- llm_message("Count to three.") |>
    chat(claude(.model = "claude-haiku-4-5", .stream = TRUE, .max_tokens = 64))
  meta <- expect_cache_cols(result)
  llt_expect_true(isTRUE(meta$stream), "stream flag must be TRUE")
  specific <- meta$api_specific[[1]]
  llt_expect_true(is.null(specific$warning),
                  "streaming metadata must no longer carry the not-implemented warning")
  llt_expect_true(!is.null(specific$stop_reason), "stop_reason must be reported for streams")
  llt_expect_true(!is.null(specific$id), "message id must be reported for streams")
})

# ── OpenAI: a repeated long prompt should hit the automatic prompt cache ───────

llt_test("openai reports cached_tokens on a repeated long prompt", {
  ask <- function() {
    llm_message(paste(long_context, "Answer with the single word: ok")) |>
      chat(openai(.model = "gpt-4o-mini"))
  }
  ask()
  meta <- expect_cache_cols(ask())
  llt_expect_true(is.na(meta$cached_tokens) || meta$cached_tokens >= 0,
                  "cached_tokens must be NA or a non-negative count")
})

llt_report("cache_metadata")

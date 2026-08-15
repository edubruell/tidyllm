# Structural tests for the build / perform / finish pipeline.
#
# Pure introspection: no network, no API key, no mocks, no extra dependency.
# These live in the CRAN suite rather than local_tests/ because they guard
# invariants that are cheap to break and impossible to notice at runtime.

# Every provider whose chat function is split. `chat_ellmer()` is deliberately
# absent: it delegates to an ellmer Chat object and builds no httr2 request, so
# there is nothing for a driver to perform.
SPLIT_PROVIDERS <- c("claude", "cc", "groq", "mistral", "deepseek", "openrouter",
                     "llamacpp", "azure_openai", "ollama", "gemini", "perplexity",
                     "openai")

test_that("every split provider has a builder", {
  for (p in SPLIT_PROVIDERS) {
    expect_true(
      exists(paste0(p, "_build_chat_request"), asNamespace("tidyllm")),
      label = paste0(p, "_build_chat_request exists")
    )
  }
})

test_that("wrapper and builder formals are identical", {
  # The wrapper forwards with `do.call(builder, mget(names(formals())))`, so a
  # default that drifts in one function silently sends the wrong value from the
  # other. Comparing the whole pairlist covers names, order and defaults.
  for (p in SPLIT_PROVIDERS) {
    f <- get(paste0(p, "_chat"), asNamespace("tidyllm"))
    b <- get(paste0(p, "_build_chat_request"), asNamespace("tidyllm"))
    expect_identical(formals(f), formals(b), label = paste0(p, " formals"))
  }
})

test_that("no split chat function takes dots", {
  # `mget(names(formals()))` on a function with `...` returns a list element
  # literally named "...", which do.call then passes as one bogus argument.
  # Sibling functions in these same files already take `...`, so adding one here
  # would be a natural change with a non-obvious failure.
  for (p in SPLIT_PROVIDERS) {
    f <- get(paste0(p, "_chat"), asNamespace("tidyllm"))
    expect_false("..." %in% names(formals(f)), label = paste0(p, "_chat has no dots"))
  }
})

test_that("builders return a tidyllm_chat_request with the fields finish needs", {
  built <- tidyllm:::claude_build_chat_request(llm_message("hello"), .dry_run = TRUE)

  expect_s3_class(built, "tidyllm_chat_request")
  expect_s3_class(built$request, "httr2_request")
  for (field in c("request", "api", "llm", "body", "tools_def", "json", "mode",
                  "timeout", "max_tries", "max_tool_rounds")) {
    expect_true(field %in% names(built), label = paste0("built$", field))
  }
})

test_that("every builder sets a mode the pipeline understands", {
  # An unrecognised mode silently reads as non-streaming, which would perform a
  # blocking request against a body that asked to stream and then run the tool
  # loop over a streamed response.
  llm <- llm_message("hello")
  extra <- list(azure_openai = list(.deployment = "x",
                                    .endpoint_url = "https://x.openai.azure.com"))

  for (p in SPLIT_PROVIDERS) {
    b <- get(paste0(p, "_build_chat_request"), asNamespace("tidyllm"))
    args <- c(list(llm, .dry_run = TRUE), extra[[p]])

    plain <- do.call(b, args)
    expect_identical(plain$mode, "value", label = paste0(p, " non-streaming mode"))
    expect_false(tidyllm:::chat_request_streams(plain))

    # Perplexity is the only provider that streams without a tools argument to
    # conflict with, but all of them accept .stream on its own.
    streamed <- do.call(b, c(args, list(.stream = TRUE)))
    expect_identical(streamed$mode, "stream", label = paste0(p, " streaming mode"))
    expect_true(tidyllm:::chat_request_streams(streamed))
  }
})

test_that("dry run returns a bare httr2 request, not the built object", {
  llm <- llm_message("hello")
  for (f in list(claude_chat, openai_chat, gemini_chat, ollama_chat, groq_chat)) {
    expect_s3_class(f(llm, .dry_run = TRUE), "httr2_request")
  }
})

test_that("rate limit and logprob support is decided by the provider class", {
  # These were per-request boolean flags until the generics gained an
  # APIProvider default. The expectations below are the pre-refactor call sites
  # verbatim: exactly these providers called track_rate_limit(), and exactly
  # these called parse_logprobs().
  mk <- function(ctor) ctor(short_name = "x", long_name = "X", api_key_env_var = "K")

  apis <- list(
    claude = api_claude, openai = api_openai, cc = tidyllm:::api_chat_completions,
    groq = tidyllm:::api_groq, mistral = tidyllm:::api_mistral,
    azure = tidyllm:::api_azure_openai, gemini = tidyllm:::api_gemini,
    ollama = tidyllm:::api_ollama, deepseek = tidyllm:::api_deepseek,
    openrouter = tidyllm:::api_openrouter, llamacpp = tidyllm:::api_llamacpp,
    perplexity = tidyllm:::api_perplexity, compatible = tidyllm:::api_compatible
  )
  expect_rl <- c(claude = TRUE, openai = TRUE, cc = TRUE, groq = TRUE,
                 mistral = TRUE, azure = TRUE, gemini = FALSE, ollama = FALSE,
                 deepseek = FALSE, openrouter = FALSE, llamacpp = FALSE,
                 perplexity = FALSE, compatible = FALSE)
  expect_lp <- c(cc = TRUE, azure = TRUE, deepseek = TRUE, llamacpp = TRUE,
                 compatible = TRUE, claude = FALSE, openai = FALSE, groq = FALSE,
                 mistral = FALSE, gemini = FALSE, ollama = FALSE,
                 openrouter = FALSE, perplexity = FALSE)

  # Every provider's header dialect at once, so a method that legitimately
  # parses finds what it needs and only a real NULL override returns NULL.
  h <- list(date = "Fri, 15 Aug 2026 09:00:00 GMT")
  for (pre in c("x-ratelimit", "anthropic-ratelimit", "ratelimit")) {
    for (k in c("limit-requests", "remaining-requests", "reset-requests",
                "limit-tokens", "remaining-tokens", "reset-tokens",
                "requests-limit", "requests-remaining", "requests-reset",
                "tokens-limit", "tokens-remaining", "tokens-reset")) {
      h[[paste(pre, k, sep = "-")]] <- "1s"
    }
  }
  for (k in c("ratelimitbysize-remaining", "ratelimitbysize-limit",
              "ratelimitbysize-reset")) h[[k]] <- "1"
  headers <- structure(h, class = "httr2_headers")

  body <- list(content = list(choices = list(list(logprobs = list(content = list(
    list(token = "a", logprob = -0.1, bytes = list(97), top_logprobs = list())
  ))))))

  for (n in names(apis)) {
    api <- mk(apis[[n]])
    expect_identical(
      !is.null(suppressWarnings(ratelimit_from_header(api, headers))),
      unname(expect_rl[[n]]),
      label = paste0(n, " rate limit support")
    )
    expect_identical(
      !is.null(parse_logprobs(api, body)),
      unname(expect_lp[[n]]),
      label = paste0(n, " logprob support")
    )
  }
})

# `finish_chat_response()` is pure given a (built, response) pair, so every hook
# it dispatches on can be exercised offline with a hand-built response. Without
# this, `.meta_fn` has no exercised consumer anywhere: its only user is
# perplexity, whose live suite is skipped for lack of account quota.

fake_response <- function(reply = "hello", meta = list(model = "m"),
                          raw = NULL, headers = NULL, ...) {
  c(list(assistant_reply = reply, meta = meta, raw = raw, headers = headers), list(...))
}

test_that("finish_chat_response appends the reply and metadata", {
  built <- tidyllm:::claude_build_chat_request(llm_message("hi"), .dry_run = TRUE)
  out <- tidyllm:::finish_chat_response(
    built, fake_response("the answer", meta = list(model = "claude-sonnet-5"))
  )

  expect_s3_class(out, "tidyllm::LLMMessage")
  expect_identical(get_reply(out), "the answer")
  expect_identical(get_metadata(out)$model, "claude-sonnet-5")
})

test_that("finish_chat_response folds perplexity search results into metadata", {
  built <- tidyllm:::perplexity_build_chat_request(llm_message("hi"), .dry_run = TRUE)
  expect_false(is.null(built$meta_fn))

  results <- list(list(title = "A source", url = "https://example.org"))
  out <- tidyllm:::finish_chat_response(
    built,
    fake_response("answer", meta = list(model = "sonar"), search_results = results)
  )

  # api_specific is a list-column on the metadata tibble.
  expect_identical(get_metadata(out)$api_specific[[1]]$search_results, results)
})

test_that("finish_chat_response leaves metadata alone without a meta_fn", {
  built <- tidyllm:::claude_build_chat_request(llm_message("hi"), .dry_run = TRUE)
  expect_null(built$meta_fn)

  meta <- list(model = "m", prompt_tokens = 7L)
  out <- tidyllm:::finish_chat_response(built, fake_response(meta = meta))
  expect_identical(get_metadata(out)$prompt_tokens, 7L)
})

test_that("finish_chat_response attaches logprobs only where the provider parses them", {
  raw <- list(content = list(choices = list(list(logprobs = list(content = list(
    list(token = "hel", logprob = -0.25, bytes = list(104L), top_logprobs = list())
  ))))))

  # ChatCompletions parses logprobs.
  with_lp <- tidyllm:::finish_chat_response(
    tidyllm:::cc_build_chat_request(llm_message("hi"), .dry_run = TRUE),
    fake_response(raw = raw)
  )
  lp <- get_logprobs(with_lp)
  expect_gt(nrow(lp), 0)
  expect_identical(lp$token[[1]], "hel")

  # Claude does not, and must not choke on a body shaped for another provider.
  without_lp <- tidyllm:::finish_chat_response(
    tidyllm:::claude_build_chat_request(llm_message("hi"), .dry_run = TRUE),
    fake_response(raw = raw)
  )
  expect_equal(nrow(get_logprobs(without_lp)), 0)
})

test_that("finish_chat_response records rate limits for providers that report them", {
  headers <- structure(list(
    date = "Fri, 15 Aug 2026 09:00:00 GMT",
    `x-ratelimit-limit-requests`     = "100",
    `x-ratelimit-remaining-requests` = "42",
    `x-ratelimit-reset-requests`     = "1s",
    `x-ratelimit-limit-tokens`       = "1000",
    `x-ratelimit-remaining-tokens`   = "900",
    `x-ratelimit-reset-tokens`       = "1s"
  ), class = "httr2_headers")

  built <- tidyllm:::cc_build_chat_request(llm_message("hi"), .dry_run = TRUE)
  tidyllm:::finish_chat_response(built, fake_response(headers = headers))

  info <- rate_limit_info()
  row <- info[info$requests_remaining == 42, ]
  expect_gt(nrow(row), 0)
})

test_that("finish_chat_response skips the tool loop when streaming", {
  # The tool loop performs blocking HTTP round trips, so a streamed request must
  # never enter it. If it did, this would attempt a real request and fail.
  tool <- tidyllm_tool(function(city) "x", "Get weather", city = field_chr("City"))
  built <- tidyllm:::claude_build_chat_request(llm_message("hi"), .dry_run = TRUE)
  built$tools_def <- list(tool)
  built$mode <- "stream"

  out <- tidyllm:::finish_chat_response(built, fake_response("streamed"))
  expect_identical(get_reply(out), "streamed")
})

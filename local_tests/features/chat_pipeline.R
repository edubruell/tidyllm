# Structural regression suite for the 0.6.0 build / perform / finish split.
#
# Offline and introspection-only: no API key, no network, no mocks. It asserts
# the shape of the seam rather than any provider's behaviour, which is what the
# async and parallel entry points depend on.
#
#   Rscript -e 'devtools::load_all("."); source("local_tests/features/chat_pipeline.R")'

source("local_tests/test_harness.R")

llt_suite("chat_pipeline")

# Every provider whose chat function was split. `chat_ellmer()` is deliberately
# absent: it delegates to an ellmer Chat object and builds no httr2 request, so
# it has no request to hand to an async driver.
SPLIT_PROVIDERS <- c("claude", "cc", "groq", "mistral", "deepseek", "openrouter",
                     "llamacpp", "azure_openai", "ollama", "gemini", "perplexity",
                     "openai")

llt_test("every split provider has a builder", {
  for (p in SPLIT_PROVIDERS) {
    llt_expect_true(exists(paste0(p, "_build_chat_request"), asNamespace("tidyllm")),
                    sprintf("%s has no _build_chat_request()", p))
  }
})

# The wrapper forwards with `mget(names(formals()))`, so a drifted default in
# either function would silently send the wrong value. Comparing the whole
# pairlist covers names, order and default expressions.
llt_test("wrapper and builder formals are identical", {
  for (p in SPLIT_PROVIDERS) {
    f <- get(paste0(p, "_chat"), asNamespace("tidyllm"))
    b <- get(paste0(p, "_build_chat_request"), asNamespace("tidyllm"))
    llt_expect_true(identical(formals(f), formals(b)),
                    sprintf("%s_chat() and %s_build_chat_request() formals differ:\n  chat:    %s\n  builder: %s",
                            p, p,
                            paste(names(formals(f)), collapse = ", "),
                            paste(names(formals(b)), collapse = ", ")))
  }
})

llt_test("no provider performs its own request any more", {
  # The tool loop is the one legitimate caller of perform_chat_request(); the
  # OpenAI stateful fallback is the other, and it lives in a perform_fn.
  for (f in list.files("R", pattern = "^api_.*\\.R$", full.names = TRUE)) {
    src <- readLines(f, warn = FALSE)
    hits <- grep("perform_chat_request\\(", src, value = TRUE)
    hits <- hits[!grepl("^\\s*#", hits)]
    allowed <- basename(f) == "api_openai.R"
    llt_expect_true(length(hits) == 0 || allowed,
                    sprintf("%s still calls perform_chat_request() directly:\n  %s",
                            basename(f), paste(hits, collapse = "\n  ")))
  }
})

llt_test("builders return a tidyllm_chat_request carrying what finish needs", {
  llm <- llm_message("hello")
  built <- claude_build_chat_request(llm, .dry_run = TRUE)

  llt_expect_true(inherits(built, "tidyllm_chat_request"), "not a tidyllm_chat_request")
  for (field in c("request", "api", "llm", "body", "tools_def", "json", "mode",
                  "timeout", "max_tries", "max_tool_rounds")) {
    llt_expect_true(field %in% names(built), sprintf("built object lacks '%s'", field))
  }
  llt_expect_true(inherits(built$request, "httr2_request"), "request is not an httr2 request")
  llt_expect_true(S7::S7_inherits(built$llm, LLMMessage), "llm did not survive the build")
})

# `.dry_run` is a user-facing contract: it returns the bare httr2 request, not
# the new built object.
llt_test("dry run still returns a bare httr2 request", {
  llm <- llm_message("hello")
  for (call in list(
    function() claude_chat(llm, .dry_run = TRUE),
    function() openai_chat(llm, .dry_run = TRUE),
    function() gemini_chat(llm, .dry_run = TRUE),
    function() ollama_chat(llm, .dry_run = TRUE),
    function() groq_chat(llm, .dry_run = TRUE)
  )) {
    req <- call()
    llt_expect_true(inherits(req, "httr2_request"),
                    sprintf("dry run returned a %s", paste(class(req), collapse = "/")))
  }
})

# Streaming has to be baked into the request at build time, because a request
# built for "value" will not stream: Gemini selects the URL path, the others put
# the flag in the body.
llt_test("the stream flag reaches the built request", {
  llm <- llm_message("hello")

  g_plain  <- gemini_build_chat_request(llm, .dry_run = TRUE)
  g_stream <- gemini_build_chat_request(llm, .stream = TRUE, .dry_run = TRUE)
  llt_expect_true(grepl(":generateContent", g_plain$request$url, fixed = TRUE),
                  "non-streaming gemini request is not on :generateContent")
  llt_expect_true(grepl(":streamGenerateContent", g_stream$request$url, fixed = TRUE),
                  "streaming gemini request is not on :streamGenerateContent")
  llt_expect_true(grepl("alt=sse", g_stream$request$url, fixed = TRUE),
                  "streaming gemini request is missing alt=sse")

  c_stream <- claude_build_chat_request(llm, .stream = TRUE, .dry_run = TRUE)
  llt_expect_true(isTRUE(c_stream$body$stream), "claude body lacks stream = TRUE")
  llt_expect_true(identical(c_stream$mode, "stream"), "claude built mode is not 'stream'")

  c_plain <- claude_build_chat_request(llm, .dry_run = TRUE)
  llt_expect_true(identical(c_plain$mode, "value"), "non-streaming mode is not 'value'")
})

# Only six providers return headers worth tracking. Getting this wrong is
# silent: an unnecessary call warns about missing headers, a missing one drops
# rate limit tracking without any signal.
llt_test("rate limit tracking matches the pre-split set", {
  llm <- llm_message("hello")
  expected <- c(claude = TRUE, cc = TRUE, groq = TRUE, mistral = TRUE,
                azure_openai = TRUE, openai = TRUE,
                gemini = FALSE, ollama = FALSE, perplexity = FALSE,
                openrouter = FALSE, llamacpp = FALSE, deepseek = FALSE)

  for (p in names(expected)) {
    b <- get(paste0(p, "_build_chat_request"), asNamespace("tidyllm"))
    built <- if (p == "azure_openai") {
      b(llm, .deployment = "x", .endpoint_url = "https://x.openai.azure.com", .dry_run = TRUE)
    } else {
      b(llm, .dry_run = TRUE)
    }
    llt_expect_true(identical(isTRUE(built$track_rate_limit), unname(expected[[p]])),
                    sprintf("%s: track_rate_limit is %s, expected %s",
                            p, built$track_rate_limit, expected[[p]]))
  }
})

llt_report("chat_pipeline")

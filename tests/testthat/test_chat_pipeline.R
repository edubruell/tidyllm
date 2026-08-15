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
  # The shape matters and used to be wrong: the parsed body lives under
  # `raw$content`, so a hook reading `response$search_results` found nothing and
  # perplexity's search results never reached the metadata at all.
  out <- tidyllm:::finish_chat_response(
    built,
    fake_response("answer", meta = list(model = "sonar"),
                  raw = list(content = list(search_results = results)))
  )

  # api_specific is a list-column on the metadata tibble.
  expect_identical(get_metadata(out)$api_specific[[1]]$search_results, results)
})

test_that("perplexity search results survive the streaming assembler", {
  # Streamed chunks repeat the response-level fields, so the assembled body has
  # to carry `search_results` through for the hook above to see anything.
  results <- list(list(title = "A source", url = "https://example.org"))
  events <- list(
    list(id = "1", model = "sonar",
         choices = list(list(delta = list(content = "an")))),
    list(search_results = results, citations = list("https://example.org"),
         choices = list(list(delta = list(content = "swer"),
                             finish_reason = "stop")))
  )
  assembled <- assemble_stream_response(tidyllm:::api_perplexity(
    short_name = "perplexity", long_name = "Perplexity",
    api_key_env_var = "PERPLEXITY_API_KEY"
  ), events)

  expect_identical(assembled$search_results, results)
  expect_identical(assembled$choices[[1]]$message$content, "answer")
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

test_that("finish_chat_response runs the tool loop for streams too", {
  # A streamed response reaches the loop in the same shape a blocking one does,
  # so `has_tool_calls()` decides whether to loop; it is not short-circuited by
  # the mode. With no tool calls in the assembled body there is nothing to
  # perform, so this stays offline while still going through the branch.
  tool  <- tidyllm_tool(function(city) "x", "Get weather", city = field_chr("City"))
  built <- tidyllm:::claude_build_chat_request(llm_message("hi"), .dry_run = TRUE)
  built$tools_def <- list(tool)
  built$mode <- "stream"

  assembled <- list(content = list(role = "assistant", stop_reason = "end_turn",
                                   content = list(list(type = "text", text = "streamed"))))
  out <- tidyllm:::finish_chat_response(built, fake_response("streamed", raw = assembled))
  expect_identical(get_reply(out), "streamed")
})

test_that("a stream that assembled to nothing refuses to run tools", {
  # The failure this replaces is silent: an absent body reads as "no tool calls",
  # so the loop never runs and the model's preamble becomes the final answer.
  tool  <- tidyllm_tool(function(city) "x", "Get weather", city = field_chr("City"))
  built <- tidyllm:::claude_build_chat_request(llm_message("hi"), .dry_run = TRUE)
  built$tools_def <- list(tool)
  built$mode <- "stream"

  expect_error(
    tidyllm:::finish_chat_response(built, fake_response("preamble", raw = NULL)),
    "assemble_stream_response"
  )
})

# `assemble_stream_response()` is what lets a streamed response reach the tool
# loop. The fixtures that exercise it against real recorded wire bytes live in
# local_tests/, but the accumulation rules are worth pinning here too: they are
# pure functions of a list, and the failure they guard against (a silently
# lossy assembly) does not raise anywhere.

test_that("an absent body reads as no tool calls on every provider", {
  # The guard in finish_chat_response() exists because this is FALSE rather than
  # an error: without it a provider lacking an assembler silently skips its
  # tools. If any provider ever raised here instead, that guard would be
  # unreachable and the diagnostic would come from the wrong place.
  apis <- list(
    claude = api_claude, openai = api_openai, cc = tidyllm:::api_chat_completions,
    groq = tidyllm:::api_groq, mistral = tidyllm:::api_mistral,
    azure = tidyllm:::api_azure_openai, gemini = tidyllm:::api_gemini,
    ollama = tidyllm:::api_ollama, deepseek = tidyllm:::api_deepseek,
    openrouter = tidyllm:::api_openrouter, llamacpp = tidyllm:::api_llamacpp,
    perplexity = tidyllm:::api_perplexity, compatible = tidyllm:::api_compatible
  )
  for (n in names(apis)) {
    api <- apis[[n]](short_name = "x", long_name = "X", api_key_env_var = "K")
    expect_false(has_tool_calls(api, list(raw = list(content = NULL))),
                 label = paste0(n, " on an absent body"))
  }
})

test_that("the base-class assembler returns NULL rather than erroring", {
  # Called on every stream, including providers that never see a tool call.
  api <- tidyllm:::APIProvider(short_name = "x", long_name = "X", api_key_env_var = "K")
  expect_null(assemble_stream_response(api, list()))
})

test_that("every provider accepting streamed tools has its own assembler", {
  # The guard against .stream with .tools and the assembler have to come and go
  # together. A provider that accepts the combination while inheriting the
  # do-nothing default would stream, find no tool calls in a NULL body, and
  # return the model's preamble as the final answer.
  llm  <- llm_message("hello")
  tool <- tidyllm_tool(function(city) "x", "Get weather", city = field_chr("City"))
  default <- S7::method(assemble_stream_response,
                        list(tidyllm:::APIProvider, S7::class_list))
  extra <- list(azure_openai = list(.deployment = "x",
                                    .endpoint_url = "https://x.openai.azure.com"))

  for (p in setdiff(SPLIT_PROVIDERS, c("cc", "perplexity"))) {
    b <- get(paste0(p, "_build_chat_request"), asNamespace("tidyllm"))
    built <- do.call(b, c(list(llm, .tools = tool, .stream = TRUE, .dry_run = TRUE),
                          extra[[p]]))

    resolved <- S7::method(assemble_stream_response,
                           list(S7::S7_class(built$api), S7::class_list))
    expect_false(identical(resolved, default),
                 label = paste0(p, " has its own assemble_stream_response()"))
  }
})

test_that("chat completions assembly accumulates split tool arguments by index", {
  # Both recorded fixtures happened to send each call whole, so the split case,
  # which OpenAI's own endpoint produces routinely, is only covered here.
  chunk <- function(delta, finish = NULL, ...) {
    list(choices = list(list(index = 0L, delta = delta, finish_reason = finish)), ...)
  }
  events <- list(
    chunk(list(role = "assistant", content = "Let me check. ")),
    chunk(list(tool_calls = list(list(index = 0L, id = "call_1", type = "function",
                                      `function` = list(name = "get_weather",
                                                        arguments = "{\"ci"))))),
    chunk(list(tool_calls = list(list(index = 0L,
                                      `function` = list(arguments = "ty\": \"Bern\"}"))))),
    chunk(list(tool_calls = list(list(index = 1L, id = "call_2", type = "function",
                                      `function` = list(name = "get_weather",
                                                        arguments = "{\"city\": \"Oslo\"}"))))),
    chunk(list(), finish = "tool_calls", model = "m", usage = list(total_tokens = 9L))
  )

  api  <- tidyllm:::api_chat_completions(short_name = "x", long_name = "X",
                                         api_key_env_var = "K")
  body <- assemble_stream_response(api, events)
  msg  <- body$choices[[1]]$message

  expect_identical(msg$content, "Let me check. ")
  expect_identical(body$choices[[1]]$finish_reason, "tool_calls")
  expect_identical(body$model, "m")
  expect_length(msg$tool_calls, 2)

  # The split call has to rejoin into parseable JSON, and the id and name that
  # arrived only on its first fragment have to survive.
  first <- msg$tool_calls[[1]]
  expect_identical(first$id, "call_1")
  expect_identical(first$`function`$name, "get_weather")
  expect_identical(jsonlite::fromJSON(first$`function`$arguments)$city, "Bern")
  expect_identical(jsonlite::fromJSON(msg$tool_calls[[2]]$`function`$arguments)$city, "Oslo")

  # The generics have to find them in the assembled body without adaptation.
  resp <- list(raw = list(content = body))
  expect_true(has_tool_calls(api, resp))
  expect_length(extract_tool_calls(api, resp), 2)
})

test_that("chat completions assembly rescues a lone index-less tool call", {
  # Some OpenAI-compatible servers omit `index` when a chunk carries one call.
  # Dropping it is silent: has_tool_calls() reads FALSE, no loop runs, and the
  # preamble is returned as the answer. A lone call therefore falls back to
  # slot 0; an index-less call arriving beside others still cannot be placed.
  api <- tidyllm:::api_chat_completions(short_name = "x", long_name = "X",
                                        api_key_env_var = "K")

  lone <- assemble_stream_response(api, list(list(choices = list(list(
    delta = list(tool_calls = list(list(id = "c1", type = "function",
                                        `function` = list(name = "get_weather",
                                                          arguments = "{\"city\": \"Bern\"}")))),
    finish_reason = "tool_calls")))))
  expect_length(lone$choices[[1]]$message$tool_calls, 1)
  expect_true(has_tool_calls(api, list(raw = list(content = lone))))

  # Two in one chunk, neither with an index: not attributable, so skipped
  # rather than merged into one call with concatenated arguments.
  ambiguous <- assemble_stream_response(api, list(list(choices = list(list(
    delta = list(tool_calls = list(
      list(id = "c1", `function` = list(name = "f", arguments = "{\"a\": 1}")),
      list(id = "c2", `function` = list(name = "f", arguments = "{\"a\": 2}"))
    )))))))
  expect_null(ambiguous$choices[[1]]$message$tool_calls)
})

test_that("chat completions assembly gives an argument-less call an empty object", {
  # `""` makes run_tool_calls() fail to parse, warn, and drop the result while
  # the assistant message still announces the call; the follow-up request is then
  # a tool_calls entry with no matching tool message, which endpoints reject.
  api <- tidyllm:::api_chat_completions(short_name = "x", long_name = "X",
                                        api_key_env_var = "K")
  body <- assemble_stream_response(api, list(list(choices = list(list(
    delta = list(tool_calls = list(list(index = 0L, id = "c1", type = "function",
                                        `function` = list(name = "now")))),
    finish_reason = "tool_calls")))))

  expect_identical(body$choices[[1]]$message$tool_calls[[1]]$`function`$arguments, "{}")
})

test_that("stream envelope merging survives a null-valued field", {
  # jsonlite keeps a JSON null as a named element with a NULL value, and
  # modifyList() reads that as "delete this key". OpenAI-compatible endpoints
  # send "usage": null on every chunk but the last when usage reporting is on.
  api <- tidyllm:::api_chat_completions(short_name = "x", long_name = "X",
                                        api_key_env_var = "K")
  events <- list(
    list(model = "m", usage = list(total_tokens = 7L),
         choices = list(list(delta = list(content = "hi")))),
    list(model = "m", usage = NULL,
         choices = list(list(delta = list(), finish_reason = "stop")))
  )
  body <- assemble_stream_response(api, events)

  expect_identical(body$usage$total_tokens, 7L)
})

test_that("ollama assembly accumulates thinking text", {
  api <- tidyllm:::api_ollama(short_name = "ollama", long_name = "Ollama",
                              api_key_env_var = "")
  body <- assemble_stream_response(api, list(
    list(model = "m", message = list(role = "assistant", thinking = "Let me ")),
    list(model = "m", message = list(role = "assistant", thinking = "think.")),
    list(model = "m", message = list(role = "assistant", content = "Done."),
         done = TRUE)
  ))

  expect_identical(body$message$thinking, "Let me think.")
  expect_identical(body$message$content, "Done.")
})

test_that("chat completions assembly keeps streamed logprobs where the parser looks", {
  # Streams used to carry their logprobs in a separate per-chunk path. Folding
  # them into the blocking shape is what let that second path be deleted, so a
  # regression here silently empties get_logprobs() for every streamed call.
  token <- function(t, lp) list(token = t, logprob = lp, bytes = list(1L),
                                top_logprobs = list())
  events <- list(
    list(choices = list(list(index = 0L, delta = list(content = "Hi"),
                             logprobs = list(content = list(token("Hi", -0.1)))))),
    list(choices = list(list(index = 0L, delta = list(content = "!"),
                             logprobs = list(content = list(token("!", -0.2))),
                             finish_reason = "stop")))
  )

  api  <- tidyllm:::api_chat_completions(short_name = "x", long_name = "X",
                                         api_key_env_var = "K")
  body <- assemble_stream_response(api, events)

  expect_length(body$choices[[1]]$logprobs$content, 2)
  parsed <- parse_logprobs(api, list(content = body))
  expect_length(parsed, 2)
  expect_identical(parsed[[1]]$token, "Hi")
})

test_that("claude assembly accumulates tool arguments per content block", {
  # The trap this guards: one shared buffer. Interleaved fragments from two
  # concurrent tool_use blocks concatenate into invalid JSON, and the recorded
  # live stream in local_tests/fixtures/ does exactly that.
  delta <- function(index, type, ...) {
    list(type = "content_block_delta", index = index,
         delta = c(list(type = type), list(...)))
  }
  events <- list(
    list(type = "message_start", message = list(id = "msg_1", model = "claude")),
    list(type = "content_block_start", index = 0L,
         content_block = list(type = "text", text = "")),
    delta(0L, "text_delta", text = "Checking both."),
    list(type = "content_block_start", index = 1L,
         content_block = list(type = "tool_use", id = "toolu_1",
                              name = "get_weather", input = list())),
    list(type = "content_block_start", index = 2L,
         content_block = list(type = "tool_use", id = "toolu_2",
                              name = "get_weather", input = list())),
    delta(1L, "input_json_delta", partial_json = "{\"city\""),
    delta(2L, "input_json_delta", partial_json = "{\"ci"),
    delta(1L, "input_json_delta", partial_json = ": \"Bern\"}"),
    delta(2L, "input_json_delta", partial_json = ""),
    delta(2L, "input_json_delta", partial_json = "ty\": \"Oslo\"}"),
    list(type = "message_delta", delta = list(stop_reason = "tool_use"),
         usage = list(output_tokens = 12L)),
    list(type = "message_stop")
  )

  api  <- tidyllm:::api_claude(short_name = "claude", long_name = "Claude",
                              api_key_env_var = "ANTHROPIC_API_KEY")
  body <- assemble_stream_response(api, events)

  expect_identical(body$stop_reason, "tool_use")
  expect_identical(body$content[[1]]$text, "Checking both.")

  resp  <- list(raw = list(content = body))
  expect_true(has_tool_calls(api, resp))
  calls <- extract_tool_calls(api, resp)
  expect_length(calls, 2)
  expect_identical(vapply(calls, function(c) c$input$city, character(1)),
                   c("Bern", "Oslo"))
  expect_identical(vapply(calls, function(c) c$id, character(1)),
                   c("toolu_1", "toolu_2"))
})

test_that("claude assembly keeps an empty tool input serialising as an object", {
  # No fragments at all is a call with no arguments. The distinction that matters
  # is jsonlite's: a NAMED empty list writes `{}`, an unnamed one writes `[]`,
  # and the API rejects `[]` with "Input should be an object" when the block is
  # sent back. So the `{}` from content_block_start is kept, not replaced.
  empty_object <- structure(list(), names = character())
  events <- list(
    list(type = "content_block_start", index = 0L,
         content_block = list(type = "tool_use", id = "toolu_1",
                              name = "now", input = empty_object)),
    list(type = "message_delta", delta = list(stop_reason = "tool_use"))
  )
  api  <- tidyllm:::api_claude(short_name = "claude", long_name = "Claude",
                              api_key_env_var = "ANTHROPIC_API_KEY")
  body <- assemble_stream_response(api, events)

  expect_identical(
    as.character(jsonlite::toJSON(body$content[[1]]$input, auto_unbox = TRUE)),
    "{}"
  )
})

test_that("claude assembly survives a tool call truncated mid-arguments", {
  # Hitting max_tokens during input_json_delta leaves a JSON prefix. Erroring on
  # it would throw away a reply the user has already watched stream past, so the
  # partial arguments are dropped and stop_reason ends the turn instead.
  events <- list(
    list(type = "content_block_start", index = 0L,
         content_block = list(type = "text", text = "")),
    list(type = "content_block_delta", index = 0L,
         delta = list(type = "text_delta", text = "Looking that up.")),
    list(type = "content_block_start", index = 1L,
         content_block = list(type = "tool_use", id = "toolu_1", name = "get_weather",
                              input = structure(list(), names = character()))),
    list(type = "content_block_delta", index = 1L,
         delta = list(type = "input_json_delta", partial_json = "{\"city\": \"Berl")),
    list(type = "message_delta", delta = list(stop_reason = "max_tokens"))
  )
  api <- tidyllm:::api_claude(short_name = "claude", long_name = "Claude",
                             api_key_env_var = "ANTHROPIC_API_KEY")

  expect_no_error(body <- assemble_stream_response(api, events))
  expect_identical(body$content[[1]]$text, "Looking that up.")
  expect_identical(body$stop_reason, "max_tokens")
  # stop_reason is not "tool_use", so the loop correctly declines to call a tool
  # whose arguments never finished arriving.
  expect_false(has_tool_calls(api, list(raw = list(content = body))))
})

test_that("claude assembly accumulates thinking text and its signature", {
  # `append_tool_messages()` sends these blocks back verbatim, so a thinking
  # block that lost its text or kept only the last fragment of its signature
  # makes Claude reject the continued turn. Reachable since .thinking, .stream
  # and .tools can now be combined.
  events <- list(
    list(type = "content_block_start", index = 0L,
         content_block = list(type = "thinking", thinking = "")),
    list(type = "content_block_delta", index = 0L,
         delta = list(type = "thinking_delta", thinking = "Two cities, ")),
    list(type = "content_block_delta", index = 0L,
         delta = list(type = "thinking_delta", thinking = "so two calls.")),
    list(type = "content_block_delta", index = 0L,
         delta = list(type = "signature_delta", signature = "sig-part-1")),
    list(type = "content_block_delta", index = 0L,
         delta = list(type = "signature_delta", signature = "sig-part-2")),
    list(type = "message_delta", delta = list(stop_reason = "tool_use"))
  )
  api  <- tidyllm:::api_claude(short_name = "claude", long_name = "Claude",
                              api_key_env_var = "ANTHROPIC_API_KEY")
  body <- assemble_stream_response(api, events)

  expect_identical(body$content[[1]]$thinking, "Two cities, so two calls.")
  # Concatenated, matching what extract_metadata_stream() does with the same
  # deltas; keeping only the last fragment would send back an invalid signature.
  expect_identical(body$content[[1]]$signature, "sig-part-1sig-part-2")
})

test_that("claude assembly ignores events it cannot place", {
  # Malformed wire data must not raise a subscript error in the middle of a
  # stream the user is already watching.
  events <- list(
    list(type = "content_block_start",
         content_block = list(type = "text", text = "")),
    list(type = "content_block_delta",
         delta = list(type = "text_delta", text = "orphan")),
    list(type = "content_block_start", index = 0L,
         content_block = list(type = "text", text = "")),
    list(type = "content_block_delta", index = 7L,
         delta = list(type = "text_delta", text = "no such block")),
    list(type = "content_block_delta", index = 0L,
         delta = list(type = "text_delta", text = "kept"))
  )
  api <- tidyllm:::api_claude(short_name = "claude", long_name = "Claude",
                             api_key_env_var = "ANTHROPIC_API_KEY")

  expect_no_error(body <- assemble_stream_response(api, events))
  expect_length(body$content, 1)
  expect_identical(body$content[[1]]$text, "kept")
})

test_that("claude assembly fills in built-in server tool arguments", {
  # server_tool_use blocks (claude_websearch()) fragment their input the same way
  # and are sent back the same way. Dropping their arguments leaves `input: {}`,
  # which Claude rejects on the continued turn.
  events <- list(
    list(type = "content_block_start", index = 0L,
         content_block = list(type = "server_tool_use", id = "srvtoolu_1",
                              name = "web_search",
                              input = structure(list(), names = character()))),
    list(type = "content_block_delta", index = 0L,
         delta = list(type = "input_json_delta", partial_json = "{\"query\": ")),
    list(type = "content_block_delta", index = 0L,
         delta = list(type = "input_json_delta", partial_json = "\"weather\"}")),
    list(type = "message_delta", delta = list(stop_reason = "tool_use"))
  )
  api  <- tidyllm:::api_claude(short_name = "claude", long_name = "Claude",
                              api_key_env_var = "ANTHROPIC_API_KEY")
  body <- assemble_stream_response(api, events)

  expect_identical(body$content[[1]]$input$query, "weather")
})

test_that("gemini assembly merges text parts but never merges function calls", {
  # Merging is what makes the assembled parts look like a blocking response
  # rather than one part per chunk, and it must not touch anything else: the
  # thoughtSignature has to ride back out on its own functionCall part or the
  # continued turn is rejected.
  events <- list(
    list(candidates = list(list(content = list(parts = list(list(text = "Look")),
                                               role = "model")))),
    list(candidates = list(list(content = list(parts = list(list(text = "ing up.")),
                                               role = "model")))),
    list(candidates = list(list(content = list(parts = list(
      list(functionCall = list(name = "get_weather", args = list(city = "Bern")),
           thoughtSignature = "sig"),
      list(functionCall = list(name = "get_weather", args = list(city = "Oslo")))
    ), role = "model"), finishReason = "STOP")),
    usageMetadata = list(totalTokenCount = 20L))
  )

  api  <- tidyllm:::api_gemini(short_name = "gemini", long_name = "Gemini",
                               api_key_env_var = "GOOGLE_API_KEY")
  body <- assemble_stream_response(api, events)
  parts <- body$candidates[[1]]$content$parts

  expect_length(parts, 3)
  expect_identical(parts[[1]]$text, "Looking up.")
  expect_identical(parts[[2]]$thoughtSignature, "sig")
  expect_identical(body$candidates[[1]]$finishReason, "STOP")

  resp <- list(raw = list(content = body))
  expect_true(has_tool_calls(api, resp))
  expect_length(extract_tool_calls(api, resp), 2)
})

test_that("ollama assembly joins the text and collects every tool call", {
  events <- list(
    list(model = "m", message = list(role = "assistant", content = "Look")),
    list(model = "m", message = list(role = "assistant", content = "ing up.",
                                     tool_calls = list(list(id = "c1",
                                       `function` = list(name = "get_weather",
                                                         arguments = list(city = "Bern")))))),
    list(model = "m", message = list(role = "assistant", content = "",
                                     tool_calls = list(list(id = "c2",
                                       `function` = list(name = "get_weather",
                                                         arguments = list(city = "Oslo")))))),
    list(model = "m", done = TRUE, done_reason = "stop",
         message = list(role = "assistant", content = ""))
  )

  api  <- tidyllm:::api_ollama(short_name = "ollama", long_name = "Ollama",
                               api_key_env_var = "")
  body <- assemble_stream_response(api, events)

  expect_identical(body$message$content, "Looking up.")
  expect_true(body$done)

  resp  <- list(raw = list(content = body))
  expect_true(has_tool_calls(api, resp))
  calls <- extract_tool_calls(api, resp)
  expect_length(calls, 2)
  expect_identical(calls[[2]]$`function`$arguments$city, "Oslo")
})

test_that("openai assembly projects the completed event's response object", {
  api <- tidyllm:::api_openai(short_name = "openai", long_name = "OpenAI",
                              api_key_env_var = "OPENAI_API_KEY")

  # No terminal event means no body; the caller must not be handed a half one.
  expect_null(assemble_stream_response(api, list(list(type = "response.created"))))

  response <- list(id = "resp_1", output = list(
    list(type = "function_call", name = "get_weather", call_id = "call_1",
         arguments = "{\"city\": \"Bern\"}")
  ))
  body <- assemble_stream_response(api, list(
    list(type = "response.output_item.added"),
    list(type = "response.completed", response = response)
  ))

  expect_identical(body, response)
  resp <- list(raw = list(content = body))
  expect_true(has_tool_calls(api, resp))
  expect_identical(extract_tool_calls(api, resp)[[1]]$call_id, "call_1")
})

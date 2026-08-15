# Offline tests for streamed responses that call tools.
#
# Three jobs:
#
#   1. It locks in the wire facts `assemble_stream_body()` has to handle, per
#      provider, as executable statements rather than prose in a design note.
#   2. It asserts that the shared pump still RETAINS the events the assembler
#      needs. Each provider's `parse_stream_event()` decides what to keep, and a
#      keep flag flipped for a plausible-looking reason would silently strip the
#      tool calls out of `raw_data` while every existing streaming test stayed
#      green.
#   3. It runs each provider's assembler over its fixture and asserts that the
#      unchanged tool generics find the calls in the result. That is the whole
#      claim of Phase B2: a streamed response reaches `process_tool_loop()` in
#      the same shape a blocking one does.
#
#   Rscript -e 'devtools::load_all("."); source("local_tests/features/stream_tools_replay.R")'
#
# Needs no API key and no network beyond localhost. Fixtures come from
# local_tests/record_stream_tool_fixtures.R.

source("local_tests/test_harness.R")
source("local_tests/stream_replay.R")

llt_suite("stream_tools_replay")

if (!requireNamespace("webfakes", quietly = TRUE)) {
  stop("webfakes is needed to replay recorded streams: install.packages('webfakes')")
}

server <- start_stream_replay_server()
on.exit(server$stop(), add = TRUE)

TOOL_FIXTURES <- grep("tools_stream", names(server$fixtures), value = TRUE)

#' Replay a fixture and return the events the pump kept.
kept_events <- function(name) {
  fx   <- server$fixtures[[name]]
  api  <- stream_fixture_api(fx)
  resp <- replay_stream_response(server, name)
  handle_stream(api, resp)$raw_data
}

#' Replay a fixture and return what a performed response would look like.
#'
#' The `raw$content` nesting is not decoration: it is exactly where
#' `perform_chat_request()` puts the assembled body, and where every
#' `has_tool_calls()` method looks.
replayed_response <- function(name) {
  api <- stream_fixture_api(server$fixtures[[name]])
  list(api = api,
       response = list(raw = list(content = assemble_stream_body(api, kept_events(name)))))
}

llt_test("all six tool-call fixtures are present", {
  for (p in c("claude", "chat_completions_groq", "chat_completions_mistral",
              "openai_responses", "gemini", "ollama")) {
    llt_expect_true(any(grepl(p, TOOL_FIXTURES)),
                    sprintf("no tool-call fixture for %s; re-record with local_tests/record_stream_tool_fixtures.R", p))
  }
})

llt_test("the pump survives every tool-call stream", {
  for (nm in TOOL_FIXTURES) {
    res <- tryCatch(kept_events(nm), error = function(e) conditionMessage(e))
    llt_expect_true(is.list(res), sprintf("%s: pump raised '%s'", nm, res))
    llt_expect_true(length(res) > 0, sprintf("%s: pump kept no events at all", nm))
  }
})

# ── Claude: per-block accumulation is mandatory ───────────────────────────────
#
# Arguments arrive as `input_json_delta` fragments that split mid-token and are
# scoped by content block. This is the one provider where a naive single-buffer
# accumulator is not merely fragile but provably wrong, so it gets the sharpest
# assertions.

llt_test("claude fragments tool arguments across events", {
  events <- kept_events("claude_tools_stream")
  frags <- Filter(function(e) identical(e$type, "content_block_delta") &&
                              identical(e$delta$type, "input_json_delta"), events)
  llt_expect_true(length(frags) > 2,
                  sprintf("expected fragmented arguments, got %d fragments", length(frags)))

  # An empty fragment is normal and must not be read as a terminator.
  llt_expect_true(any(!nzchar(vapply(frags, function(f) f$delta$partial_json, character(1)))),
                  "no empty fragment in the recording; the empty-fragment guard is untested")
})

llt_test("concatenating every claude fragment does NOT parse", {
  events <- kept_events("claude_tools_stream")
  frags <- Filter(function(e) identical(e$type, "content_block_delta") &&
                              identical(e$delta$type, "input_json_delta"), events)
  welded <- paste(vapply(frags, function(f) f$delta$partial_json, character(1)), collapse = "")

  parsed <- tryCatch(jsonlite::fromJSON(welded), error = function(e) NULL)
  llt_expect_true(is.null(parsed),
                  sprintf("a single buffer parsed, so this fixture no longer proves the point: <%s>", welded))
})

llt_test("claude fragments grouped by content block DO parse", {
  events <- kept_events("claude_tools_stream")
  frags <- Filter(function(e) identical(e$type, "content_block_delta") &&
                              identical(e$delta$type, "input_json_delta"), events)

  by_block <- split(vapply(frags, function(f) f$delta$partial_json, character(1)),
                    vapply(frags, function(f) as.character(f$index), character(1)))

  llt_expect_true(length(by_block) >= 2,
                  sprintf("expected at least two tool_use blocks, got %d", length(by_block)))

  for (idx in names(by_block)) {
    joined <- paste(by_block[[idx]], collapse = "")
    args <- tryCatch(jsonlite::fromJSON(joined), error = function(e) NULL)
    llt_expect_true(!is.null(args),
                    sprintf("block %s did not parse when joined: <%s>", idx, joined))
    llt_expect_true(!is.null(args$city),
                    sprintf("block %s parsed but has no 'city' argument", idx))
  }

  cities <- sort(unname(unlist(lapply(by_block, function(x)
    jsonlite::fromJSON(paste(x, collapse = ""))$city))))
  llt_expect_true(identical(cities, c("Berlin", "Reykjavik")),
                  sprintf("expected Berlin and Reykjavik, got %s", paste(cities, collapse = ", ")))
})

llt_test("claude marks tool use in message_delta stop_reason", {
  events <- kept_events("claude_tools_stream")
  deltas <- Filter(function(e) identical(e$type, "message_delta"), events)
  reasons <- unlist(lapply(deltas, function(e) e$delta$stop_reason))
  llt_expect_true("tool_use" %in% reasons,
                  sprintf("no stop_reason 'tool_use'; got %s", paste(reasons, collapse = ", ")))
})

llt_test("claude keeps the tool_use block names alongside the fragments", {
  events <- kept_events("claude_tools_stream")
  starts <- Filter(function(e) identical(e$type, "content_block_start") &&
                               identical(e$content_block$type, "tool_use"), events)
  llt_expect_true(length(starts) >= 2,
                  sprintf("expected two tool_use content_block_start events, got %d", length(starts)))
  for (s in starts) {
    llt_expect_true(nzchar(s$content_block$name %||% ""), "a tool_use block has no name")
    llt_expect_true(nzchar(s$content_block$id %||% ""), "a tool_use block has no id")
  }
})

# ── ChatCompletions family: accumulate by index ───────────────────────────────
#
# Arguments arrived whole in both of these recordings, but that is not a
# guarantee; OpenAI's Chat Completions endpoint routinely fragments them. The
# assembler must accumulate by `index` unconditionally, so the invariant tested
# here is that an index is always present to accumulate on.

for (nm in grep("chat_completions_.*tools_stream", TOOL_FIXTURES, value = TRUE)) {
  local({
    fixture_name <- nm
    llt_test(sprintf("%s keys tool calls by index", fixture_name), {
      events <- kept_events(fixture_name)
      calls <- unlist(lapply(events, function(e) {
        ch <- e$choices
        if (length(ch) == 0) return(NULL)
        ch[[1]]$delta$tool_calls
      }), recursive = FALSE)

      llt_expect_true(length(calls) > 0, "no tool_calls deltas in the stream")
      for (tc in calls) {
        llt_expect_true(!is.null(tc$index),
                        "a tool_calls delta has no index to accumulate on")
      }
    })

    llt_test(sprintf("%s terminates with finish_reason tool_calls", fixture_name), {
      events <- kept_events(fixture_name)
      reasons <- unlist(lapply(events, function(e) {
        ch <- e$choices
        if (length(ch) == 0) return(NULL)
        ch[[1]]$finish_reason
      }))
      llt_expect_true("tool_calls" %in% reasons,
                      sprintf("no finish_reason 'tool_calls'; got %s",
                              paste(reasons, collapse = ", ")))
    })
  })
}

# ── OpenAI Responses: the terminal event is already the whole body ────────────
#
# This is the finding that makes the OpenAI assembler nearly free, and it is
# worth a test precisely because it is surprising: the pump keeps exactly one
# event for this stream, and that event carries the complete `output` array with
# fully-formed `function_call` items. No fragment accumulation is needed at all,
# even though `response.function_call_arguments.delta` events do stream by.

llt_test("openai response.completed carries fully-formed function calls", {
  events <- kept_events("openai_responses_tools_stream")
  llt_expect_true(length(events) == 1,
                  sprintf("expected exactly one kept event, got %d", length(events)))

  completed <- events[[1]]
  llt_expect_true(identical(completed$type, "response.completed"),
                  sprintf("kept event is '%s', not response.completed", completed$type))

  output <- completed$response$output
  calls <- Filter(function(o) identical(o$type, "function_call"), output)
  llt_expect_true(length(calls) >= 2,
                  sprintf("expected two function_call items, got %d", length(calls)))

  for (fc in calls) {
    llt_expect_true(nzchar(fc$name %||% ""), "function_call item has no name")
    args <- tryCatch(jsonlite::fromJSON(fc$arguments), error = function(e) NULL)
    llt_expect_true(!is.null(args$city),
                    sprintf("function_call arguments did not parse: <%s>", fc$arguments))
  }
})

# ── Gemini and Ollama: complete calls, already parsed ─────────────────────────

llt_test("gemini streams functionCall parts with parsed args", {
  events <- kept_events("gemini_tools_stream_sse")
  parts <- unlist(lapply(events, function(e) {
    cand <- e$candidates
    if (length(cand) == 0) return(NULL)
    cand[[1]]$content$parts
  }), recursive = FALSE)

  calls <- Filter(function(p) !is.null(p$functionCall), parts)
  llt_expect_true(length(calls) >= 2,
                  sprintf("expected two functionCall parts, got %d", length(calls)))

  for (c in calls) {
    # Gemini sends args as a JSON object, not a string, so no parsing step.
    llt_expect_true(is.list(c$functionCall$args),
                    "functionCall args is not an already-parsed object")
    llt_expect_true(!is.null(c$functionCall$args$city),
                    "functionCall args has no 'city'")
  }
})

# Thought signatures have to survive back into the follow-up request or Gemini
# rejects the continued turn. Nothing consumes this yet; the assertion exists so
# the assembler is written knowing the field is on the wire.
llt_test("gemini attaches a thoughtSignature to the first functionCall", {
  events <- kept_events("gemini_tools_stream_sse")
  parts <- unlist(lapply(events, function(e) {
    cand <- e$candidates
    if (length(cand) == 0) return(NULL)
    cand[[1]]$content$parts
  }), recursive = FALSE)

  sigs <- Filter(function(p) !is.null(p$thoughtSignature), parts)
  llt_expect_true(length(sigs) >= 1,
                  "no thoughtSignature on any part; the assembler cannot round-trip thinking")
})

llt_test("ollama streams complete tool calls and a done terminator", {
  events <- kept_events("ollama_tools_stream")
  calls <- unlist(lapply(events, function(e) e$message$tool_calls),
                  recursive = FALSE)
  llt_expect_true(length(calls) >= 2,
                  sprintf("expected two tool calls, got %d", length(calls)))
  for (tc in calls) {
    llt_expect_true(is.list(tc$`function`$arguments),
                    "ollama tool arguments are not an already-parsed object")
    llt_expect_true(!is.null(tc$`function`$arguments$city),
                    "ollama tool arguments have no 'city'")
  }

  llt_expect_true(any(vapply(events, function(e) isTRUE(e$done), logical(1))),
                  "no event with done = TRUE terminated the ollama stream")
})

# ── What the assembler produces ──────────────────────────────────────────────
#
# The tests above describe the wire. These assert the point of the exercise: the
# tool generics, which were written for blocking responses and have not been
# touched, find the calls in an assembled stream.

#' How each provider names the tool and its city argument, once extracted.
#'
#' The generics deliberately return each provider's own call shape rather than a
#' normalised one, because `run_tool_calls()` is provider-specific too. So the
#' expectations have to un-normalise here.
CALL_READERS <- list(
  claude     = function(tc) list(name = tc$name, city = tc$input$city),
  openai     = function(tc) list(name = tc$name,
                                 city = jsonlite::fromJSON(tc$arguments)$city),
  gemini     = function(tc) list(name = tc$name, city = tc$args$city),
  ollama     = function(tc) list(name = tc$`function`$name,
                                 city = tc$`function`$arguments$city),
  chat_completions = function(tc) list(name = tc$`function`$name,
                                       city = jsonlite::fromJSON(tc$`function`$arguments)$city)
)

fixture_reader <- function(name) {
  if (grepl("^claude", name))           CALL_READERS$claude
  else if (grepl("^openai", name))      CALL_READERS$openai
  else if (grepl("^gemini", name))      CALL_READERS$gemini
  else if (grepl("^ollama", name))      CALL_READERS$ollama
  else                                  CALL_READERS$chat_completions
}

llt_test("the tool generics find calls in every assembled stream", {
  for (nm in TOOL_FIXTURES) {
    rr <- replayed_response(nm)
    llt_expect_true(has_tool_calls(rr$api, rr$response),
                    sprintf("%s: has_tool_calls() is FALSE on the assembled body", nm))

    calls <- extract_tool_calls(rr$api, rr$response)
    llt_expect_true(length(calls) >= 1,
                    sprintf("%s: assembled body yielded no tool calls", nm))

    read <- fixture_reader(nm)
    for (tc in calls) {
      got <- read(tc)
      llt_expect_true(nzchar(got$name %||% ""),
                      sprintf("%s: an assembled call has no tool name", nm))
      llt_expect_true(got$city %in% c("Berlin", "Reykjavik"),
                      sprintf("%s: assembled argument is '%s', not a recorded city",
                              nm, got$city %||% "<missing>"))
    }
  }
})

llt_test("claude's two calls survive assembly with distinct arguments", {
  # The provider where assembly is real work rather than a projection, so the
  # end-to-end result gets asserted rather than only the fragments.
  rr    <- replayed_response("claude_tools_stream")
  calls <- extract_tool_calls(rr$api, rr$response)

  llt_expect_true(length(calls) == 2,
                  sprintf("expected two assembled tool_use blocks, got %d", length(calls)))
  cities <- sort(vapply(calls, function(tc) tc$input$city, character(1)))
  llt_expect_true(identical(cities, c("Berlin", "Reykjavik")),
                  sprintf("assembled cities are %s", paste(cities, collapse = ", ")))
  ids <- vapply(calls, function(tc) tc$id, character(1))
  llt_expect_true(length(unique(ids)) == 2,
                  "the two assembled calls share a tool_use id, so results would be misrouted")
})

llt_test("assembly preserves the text claude streamed alongside its calls", {
  # Claude narrates before calling. That text is what the user already saw go
  # past on the console, so dropping it from the assembled body would make the
  # follow-up round contradict the transcript: `append_tool_messages()` sends
  # these very blocks back as the assistant turn.
  body   <- replayed_response("claude_tools_stream")$response$raw$content
  blocks <- body$content
  texts  <- Filter(function(b) identical(b$type, "text"), blocks)

  llt_expect_true(length(texts) >= 1, "no text block survived assembly")
  llt_expect_true(nzchar(texts[[1]]$text %||% ""),
                  "the assembled text block is empty despite streamed text")
  llt_expect_true(identical(body$stop_reason, "tool_use"),
                  sprintf("assembled stop_reason is '%s'", body$stop_reason %||% "<missing>"))
})

llt_test("chat_completions assembly keeps finish_reason and streamed reasoning", {
  # Groq's gpt-oss streams its chain of thought in a `reasoning` field beside
  # `content`, which the blocking body also carries; dropping it silently would
  # make the assembled body a lossy copy of the same turn.
  body   <- replayed_response("chat_completions_groq_tools_stream")$response$raw$content
  choice <- body$choices[[1]]

  llt_expect_true(identical(choice$finish_reason, "tool_calls"),
                  sprintf("assembled finish_reason is '%s'",
                          choice$finish_reason %||% "<missing>"))
  llt_expect_true(nzchar(choice$message$reasoning %||% ""),
                  "streamed reasoning deltas did not survive assembly")
})

llt_test("assembly keeps gemini's thoughtSignature on the functionCall part", {
  # `append_tool_messages()` sends the model's parts back verbatim; a dropped
  # signature makes Gemini reject the continued turn outright.
  rr    <- replayed_response("gemini_tools_stream_sse")
  parts <- rr$response$raw$content$candidates[[1]]$content$parts
  sigs  <- Filter(function(p) !is.null(p$thoughtSignature), parts)
  llt_expect_true(length(sigs) >= 1,
                  "no thoughtSignature survived assembly; the continued turn would be rejected")
})

llt_test("the base-class default is silent rather than an error", {
  # `perform_chat_request()` assembles on every stream, including providers that
  # will never see a tool call, so the default has to return quietly. No shipped
  # provider still inherits it: perplexity, the one provider with no tool
  # support, is a ChatCompletions subclass and gets that family's assembler.
  api <- tidyllm:::APIProvider(short_name = "x", long_name = "X", api_key_env_var = "K")
  llt_expect_true(is.null(assemble_stream_body(api, list())),
                  "the APIProvider default no longer returns NULL")
})

# ── The loop itself, end to end and offline ──────────────────────────────────
#
# Everything above tests the assembler in isolation. This tests the WIRING: that
# `perform_chat_request()` actually puts the assembled body where the tool loop
# looks, and that follow-up rounds stream. Both were provably untested before,
# reverting either one left every offline suite green.
#
# The replay server answers each round with the same fixture, so the model never
# stops asking for tools and the loop runs to its ceiling. Hitting the ceiling
# is therefore the success condition: it can only happen if every round found
# tool calls in a streamed response.

loop_error <- function(name, stream, rounds = 2) {
  fx   <- server$fixtures[[name]]
  api  <- stream_fixture_api(fx)
  tool <- tidyllm_tool(function(city) paste0(city, ": 6 degrees"),
                       "Get the temperature in a city",
                       city = field_chr("City name"))

  req  <- httr2::request(server$url(name)) |>
    httr2::req_body_json(list(model = "replay", stream = TRUE))

  # The loop performs through a closure now, which is also how it learns whether
  # a round streams: the caller decides, once, for every round.
  perform <- function(.request, .body) {
    perform_chat_request(.request, api, stream, 30, 1)
  }

  tryCatch({
    resp <- perform_chat_request(req, api, stream, 30, 1)
    process_tool_loop(api, resp, list(tool), list(model = "replay"), req,
                      .perform = perform, .max_tool_rounds = rounds)
    NA_character_
  }, error = function(e) conditionMessage(e))
}

for (nm in TOOL_FIXTURES) {
  local({
    fixture_name <- nm
    llt_test(sprintf("%s drives a streamed multi-round tool loop", fixture_name), {
      err <- loop_error(fixture_name, stream = TRUE)

      llt_expect_true(grepl("Maximum tool rounds", err %||% ""),
                      sprintf("%s: expected the loop to reach its ceiling, got: %s",
                              fixture_name, err))
    })
  })
}

llt_test("a streamed round performed as a blocking one fails loudly", {
  # The counterpart to the test above: if the performer the loop is given reads
  # blocking, the follow-up round parses a text/event-stream body as JSON.
  # Asserting which error comes back is what distinguishes the two, since both
  # paths error.
  err <- loop_error("claude_tools_stream", stream = FALSE)

  llt_expect_true(grepl("content type|event-stream|parse", err %||% "", ignore.case = TRUE),
                  sprintf("expected a content-type failure from the blocking read, got: %s", err))
})

# -- What moved to the CRAN suite --------------------------------------------
#
# The two invariants that used to close this file, that no provider still
# rejects `.stream` with `.tools` and that every provider accepting the
# combination has its own assembler, are pure introspection: no key, no network,
# no fixture. They live in tests/testthat/test_chat_pipeline.R so that R CMD
# check runs them, along with hand-built event lists covering the accumulation
# cases the recordings happen not to contain.

llt_report("stream_tools_replay")

# Offline characterization of streamed responses that call tools.
#
# Groundwork for Phase B2, the streaming tool loop. Nothing assembles stream
# events back into tool calls yet, so this suite does two jobs:
#
#   1. It locks in the wire facts the assembler will have to handle, per
#      provider, as executable statements rather than prose in a design note.
#   2. It asserts that the shared pump still RETAINS the events an assembler
#      needs. Each provider's `parse_stream_event()` decides what to keep, and a
#      keep flag flipped for a plausible-looking reason would silently strip the
#      tool calls out of `raw_data` while every existing streaming test stayed
#      green.
#
# When the assembler lands, extend this suite to assert its output instead of
# re-deriving the shapes inline.
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

# ── The guards that Phase B2 removes ─────────────────────────────────────────
#
# Every provider currently refuses `.stream = TRUE` together with `.tools`. That
# is why the fixtures above had to be recorded by mutating a built request body.
# When B2 lands, this test should be inverted rather than deleted, so the day the
# guards go is a deliberate, visible change.

llt_test("streaming with tools is still rejected client-side", {
  llm <- llm_message("hello")
  tool <- tidyllm_tool(function(city) "x", "Get weather", city = field_chr("City"))

  for (p in c("claude", "openai", "groq", "mistral", "ollama", "gemini")) {
    f <- get(paste0(p, "_chat"), asNamespace("tidyllm"))
    err <- tryCatch({
      f(llm, .tools = tool, .stream = TRUE, .dry_run = TRUE)
      NULL
    }, error = function(e) conditionMessage(e))

    llt_expect_true(!is.null(err),
                    sprintf("%s no longer rejects .stream with .tools; if Phase B2 landed, invert this test", p))
  }
})

llt_report("stream_tools_replay")

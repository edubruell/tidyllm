# Record raw streaming wire bytes for responses that CALL TOOLS.
#
# Groundwork for Phase B2, the streaming tool loop. The existing tool generics
# (`has_tool_calls()`, `extract_tool_calls()`, `append_tool_messages()`) all read
# a complete non-streaming body; a stream never produces that object. The plan is
# an assembler that folds accumulated stream events back into the body shape
# those generics already understand, so they can be reused unchanged rather than
# duplicated. These fixtures are what such an assembler will be written against,
# and they are recorded BEFORE it exists for the same reason the plain streaming
# fixtures were.
#
#   Rscript -e 'devtools::load_all("."); source("local_tests/record_stream_tool_fixtures.R")'
#
# Fixtures land in local_tests/fixtures/streams/ alongside the plain ones and are
# replayed offline. No CRAN test sees them and no dependency follows them.
#
# WHY THIS SCRIPT REACHES INTO THE REQUEST BODY
#
# Eleven providers reject `.stream = TRUE` together with `.tools` in
# `validate_inputs()`, so a streaming tool request cannot be built through the
# public API today. Those guards are exactly what Phase B2 deletes. Until then
# the 0.6.0 build/perform/finish split gives a clean way around them: the builder
# hands back `$body`, so the stream flag can be set on the body and the request
# re-serialized. That is a recording-time bypass of a client-side guard, not of
# anything the provider enforces.

library(tibble)

source("local_tests/stream_capture.R")

# ── Re-serialize a built request with streaming turned on ─────────────────────

#' Rebuild a built request's httr2 object from a modified body.
restream <- function(built, mutate_body = identity) {
  httr2::req_body_json(built$request, data = mutate_body(built$body))
}

# ── The tools ─────────────────────────────────────────────────────────────────

# Two calls, not one. Claude fragments tool arguments across `input_json_delta`
# events and delimits them by content block; concatenating every fragment into a
# single buffer welds two calls into `{"city": "Berlin"}{"city": "Reykjavik"}`,
# which does not parse. A single-call fixture would hide that entirely, so every
# prompt here asks for two lookups.
get_weather <- function(city) {
  temps <- list(Berlin = "12C", Reykjavik = "3C", Cairo = "31C")
  temps[[city]] %||% "unknown"
}
weather_tool <- tidyllm_tool(
  get_weather,
  "Get the current temperature in a city",
  city = field_chr("City name")
)

P_TOOLS <- "What is the weather in Berlin and in Reykjavik? Use the tool for each city."

message("\n── Recording streaming tool-call fixtures ────────────────────────────")

# ── Claude ────────────────────────────────────────────────────────────────────
#
# Tool arguments arrive as `input_json_delta` fragments grouped by content block
# position and delimited by `content_block_start` / `content_block_stop`. The
# terminator is a `message_delta` carrying `stop_reason: "tool_use"`.
record_fixture(
  "claude_tools_stream",
  restream(
    claude_build_chat_request(llm_message(P_TOOLS), .tools = weather_tool, .dry_run = TRUE),
    function(body) { body$stream <- TRUE; body }
  ),
  list(provider = "claude", kind = "tools")
)

# ── ChatCompletions family ────────────────────────────────────────────────────
#
# Tool calls arrive as `tool_calls` delta objects keyed by `index`, terminated by
# `finish_reason: "tool_calls"`. Groq sometimes delivers arguments whole; OpenAI
# routinely fragments them, so both are recorded. Accumulation must be by index
# unconditionally.
for (prov in list(
  list(name = "groq",    build = groq_build_chat_request),
  list(name = "mistral", build = mistral_build_chat_request)
)) {
  record_fixture(
    sprintf("chat_completions_%s_tools_stream", prov$name),
    restream(
      prov$build(llm_message(P_TOOLS), .tools = weather_tool, .dry_run = TRUE),
      function(body) {
        body$stream <- TRUE
        body$stream_options <- list(include_usage = TRUE)
        body
      }
    ),
    list(provider = prov$name, kind = "tools")
  )
}

# ── OpenAI Responses API ──────────────────────────────────────────────────────
#
# A third vocabulary again: function calls appear as `response.output_item.*`
# events with `response.function_call_arguments.delta` fragments.
record_fixture(
  "openai_responses_tools_stream",
  restream(
    openai_build_chat_request(llm_message(P_TOOLS), .tools = weather_tool, .dry_run = TRUE),
    function(body) { body$stream <- TRUE; body }
  ),
  list(provider = "openai", kind = "tools")
)

# ── Gemini ────────────────────────────────────────────────────────────────────
#
# Gemini commits to streaming in the URL, not the body, so the request is rebuilt
# by swapping the path rather than the body. Function calls arrive as
# `functionCall` parts, which the design note flags as untested mid-stream.
local({
  built <- gemini_build_chat_request(llm_message(P_TOOLS), .tools = weather_tool,
                                     .dry_run = TRUE)
  req <- built$request
  req$url <- sub(":generateContent", ":streamGenerateContent", req$url, fixed = TRUE)
  record_fixture(
    "gemini_tools_stream_sse",
    httr2::req_url_query(req, alt = "sse"),
    list(provider = "gemini", kind = "tools", transport = "sse")
  )
})

# ── Ollama ────────────────────────────────────────────────────────────────────
#
# Newline-delimited JSON rather than SSE, and the only local provider, so it is
# the one fixture that can be re-recorded without spending anything.
record_fixture(
  "ollama_tools_stream",
  restream(
    ollama_build_chat_request(llm_message(P_TOOLS), .tools = weather_tool,
                              .think = FALSE, .dry_run = TRUE),
    function(body) { body$stream <- TRUE; body }
  ),
  list(provider = "ollama", kind = "tools")
)

# ── Summary ───────────────────────────────────────────────────────────────────

files <- list.files(FIXTURE_DIR, pattern = "tools.*\\.rds$", full.names = TRUE)
summary_tbl <- purrr::map_dfr(files, function(f) {
  fx <- readRDS(f)
  tibble(
    name         = fx$name,
    provider     = fx$provider %||% NA_character_,
    status       = fx$status,
    content_type = sub(";.*$", "", fx$content_type %||% NA_character_),
    bytes        = length(fx$bytes),
    chunks       = length(fx$wire_chunks)
  )
})

message("\n── Recorded tool-call streams ────────────────────────────────────────")
print(summary_tbl, n = Inf)
invisible(summary_tbl)

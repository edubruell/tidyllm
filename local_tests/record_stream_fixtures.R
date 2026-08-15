# Record raw streaming wire bytes from the live providers.
#
# Step 1 of the 0.6.0 sequencing (workplan §9.1, design note §7). The refactor
# rewrites six `handle_stream()` methods against zero offline streaming
# coverage; these fixtures are the only safety net it will have, so they must be
# recorded BEFORE the pump lands.
#
# Raw bytes are recorded, not parsed output. Gemini's current parser emits a
# data.frame, which is worthless as a fixture the moment it moves to `alt=sse`,
# so Gemini is recorded under both endpoints to make parity across the switch
# provable.
#
#   Rscript -e 'devtools::load_all("."); source("local_tests/record_stream_fixtures.R")'
#
# Fixtures land in local_tests/fixtures/streams/ as .rds and are tracked in git.
# They are replayed offline by local_tests/features/stream_replay.R; the CRAN
# test suite neither sees them nor gains a dependency from them.

library(tibble)

source("local_tests/stream_capture.R")

# ── Prompts ───────────────────────────────────────────────────────────────────

# Short, deterministic-ish, and cheap. The multibyte prompt exists to put
# non-ASCII characters across chunk boundaries, which is the failure mode the
# current Gemini buffer-and-match parser was written around.
P_PLAIN     <- "Count from 1 to 5, separated by commas. Nothing else."
P_MULTIBYTE <- "Reply with exactly this and nothing else: 日本語テスト — Grüße, naïve café"

# ── Recordings ────────────────────────────────────────────────────────────────

message("\n── Recording streaming fixtures ──────────────────────────────────────")

# Claude: SSE, with a thinking variant because thinking blocks arrive as
# separate content blocks and 0.5.1 made them position-independent.
record_fixture(
  "claude_plain",
  llm_message(P_PLAIN) |> chat(claude(), .stream = TRUE, .dry_run = TRUE),
  list(provider = "claude", kind = "plain")
)
record_fixture(
  "claude_multibyte",
  llm_message(P_MULTIBYTE) |> chat(claude(), .stream = TRUE, .dry_run = TRUE),
  list(provider = "claude", kind = "multibyte")
)
record_fixture(
  "claude_thinking",
  claude_chat(llm_message("What is 17 * 23? Think it through."),
              .stream = TRUE, .thinking = TRUE, .dry_run = TRUE),
  list(provider = "claude", kind = "thinking")
)

# OpenAI Responses API: a different SSE event vocabulary from ChatCompletions.
record_fixture(
  "openai_responses_plain",
  llm_message(P_PLAIN) |> chat(openai(), .stream = TRUE, .dry_run = TRUE),
  list(provider = "openai", kind = "plain")
)
record_fixture(
  "openai_responses_multibyte",
  llm_message(P_MULTIBYTE) |> chat(openai(), .stream = TRUE, .dry_run = TRUE),
  list(provider = "openai", kind = "multibyte")
)

# ChatCompletions family: recorded through two different backends because the
# shared method has to survive both. Groq and Mistral differ in how they emit
# the final usage chunk.
record_fixture(
  "chat_completions_groq_plain",
  llm_message(P_PLAIN) |> chat(groq(), .stream = TRUE, .dry_run = TRUE),
  list(provider = "groq", kind = "plain")
)
record_fixture(
  "chat_completions_mistral_plain",
  llm_message(P_PLAIN) |> chat(mistral(), .stream = TRUE, .dry_run = TRUE),
  list(provider = "mistral", kind = "plain")
)
record_fixture(
  "chat_completions_groq_multibyte",
  llm_message(P_MULTIBYTE) |> chat(groq(), .stream = TRUE, .dry_run = TRUE),
  list(provider = "groq", kind = "multibyte")
)

# Gemini under BOTH endpoints. The current one returns a chunked JSON array,
# not SSE, which is why the existing parser buffers and pattern-matches; the
# refactor migrates to alt=sse. Parity across that switch has to be provable
# from recorded bytes, so both shapes are captured from the same prompt.
gemini_sse <- function(req) httr2::req_url_query(req, alt = "sse")

record_fixture(
  "gemini_plain_jsonarray",
  llm_message(P_PLAIN) |> chat(gemini(), .stream = TRUE, .dry_run = TRUE),
  list(provider = "gemini", kind = "plain", transport = "json-array")
)
record_fixture(
  "gemini_plain_sse",
  llm_message(P_PLAIN) |> chat(gemini(), .stream = TRUE, .dry_run = TRUE) |> gemini_sse(),
  list(provider = "gemini", kind = "plain", transport = "sse")
)
record_fixture(
  "gemini_multibyte_jsonarray",
  llm_message(P_MULTIBYTE) |> chat(gemini(), .stream = TRUE, .dry_run = TRUE),
  list(provider = "gemini", kind = "multibyte", transport = "json-array")
)
record_fixture(
  "gemini_multibyte_sse",
  llm_message(P_MULTIBYTE) |> chat(gemini(), .stream = TRUE, .dry_run = TRUE) |> gemini_sse(),
  list(provider = "gemini", kind = "multibyte", transport = "sse")
)
record_fixture(
  "gemini_thinking_sse",
  gemini_chat(llm_message("What is 17 * 23? Think it through."),
              .stream = TRUE, .thinking_budget = 512, .dry_run = TRUE) |> gemini_sse(),
  list(provider = "gemini", kind = "thinking", transport = "sse")
)

# Ollama: newline-delimited JSON, not SSE. Recorded warm; the cold-start case
# is the one the empty-read guard covers and it is recorded separately below.
#
# `.think = FALSE` matters here. The default model emits a long thinking trace,
# and because the current loop sleeps 0.25s per line, a 430-line fixture takes
# nearly two minutes to replay. Dropping the trace keeps replay usable without
# changing the wire format under test.
record_fixture(
  "ollama_plain",
  ollama_chat(llm_message(P_PLAIN), .stream = TRUE, .think = FALSE, .dry_run = TRUE),
  list(provider = "ollama", kind = "plain", state = "warm")
)
record_fixture(
  "ollama_multibyte",
  ollama_chat(llm_message(P_MULTIBYTE), .stream = TRUE, .think = FALSE, .dry_run = TRUE),
  list(provider = "ollama", kind = "multibyte", state = "warm")
)

# Cold start: unload the model first so the fixture contains the long silent
# gap that used to crash the loop with a JSON lexer error.
cold_ok <- tryCatch({
  httr2::request("http://localhost:11434/api/generate") |>
    httr2::req_body_json(list(model = "qwen3.5:4b", keep_alive = 0)) |>
    httr2::req_perform()
  TRUE
}, error = function(e) FALSE)

if (cold_ok) {
  record_fixture(
    "ollama_plain_cold",
    ollama_chat(llm_message(P_PLAIN), .stream = TRUE, .think = FALSE, .dry_run = TRUE),
    list(provider = "ollama", kind = "plain", state = "cold")
  )
} else {
  message("  skipping ollama_plain_cold (could not unload the model)")
}

# perplexity(): no credits on this account, so its stream cannot be recorded.
# Its handle_stream() is therefore the one provider the refactor will land
# without a fixture; see local_tests/fixtures/README.md.
message("  skipping perplexity (no credits on this account)")

# ── Summary ───────────────────────────────────────────────────────────────────

files <- list.files(FIXTURE_DIR, pattern = "\\.rds$", full.names = TRUE)
summary_tbl <- purrr::map_dfr(files, function(f) {
  fx <- readRDS(f)
  tibble(
    name         = fx$name,
    provider     = fx$provider %||% NA_character_,
    kind         = fx$kind %||% NA_character_,
    transport    = fx$transport %||% NA_character_,
    status       = fx$status,
    content_type = sub(";.*$", "", fx$content_type %||% NA_character_),
    bytes        = length(fx$bytes),
    chunks       = length(fx$wire_chunks)
  )
})

message("\n── Recorded ──────────────────────────────────────────────────────────")
print(summary_tbl, n = Inf)
invisible(summary_tbl)

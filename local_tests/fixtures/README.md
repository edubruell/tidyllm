# Streaming fixtures

Raw wire bytes recorded from live provider streams, plus tidyllm's reading of
them at the time of recording. Together they are the safety net for the 0.6.0
Phase A refactor, which rewrites six `handle_stream()` methods against what was
previously zero offline streaming coverage.

Recorded 2026-08-15, against commit `7d6a7db` (i.e. after the maintenance
backlog landed, before the pump).

```
streams/*.rds         raw bytes + original chunk boundaries + content-type
stream_baseline.rds   reply, event count and metadata the code produced then
```

These live under `local_tests/`, not `tests/testthat/`. The CRAN test suite gains
neither the fixtures nor a `webfakes` dependency; `webfakes` is a local-only
tool and is deliberately absent from `DESCRIPTION`.

## Workflow

```r
devtools::load_all(".")
source("local_tests/record_stream_fixtures.R")       # live; re-record only on purpose
source("local_tests/record_stream_tool_fixtures.R")  # live; streams that call tools
source("local_tests/record_stream_baseline.R")   # offline; refuses to overwrite
source("local_tests/features/stream_replay.R")        # offline; the regression suite
source("local_tests/features/stream_tools_replay.R")  # offline; tool-call characterization
```

The replay suite needs no API key and no network beyond localhost: a `webfakes`
process serves the recorded chunks back at their original boundaries, so httr2
opens a real streaming connection and the real `handle_stream()` runs against it.

Re-running the baseline recorder after a refactor would overwrite the very thing
it exists to protect, so it refuses to overwrite an existing baseline. Delete the
file by hand if the fixtures were genuinely re-recorded.

## Coverage

| provider | fixtures | transport |
|---|---|---|
| claude | plain, multibyte, thinking | SSE |
| openai (Responses) | plain, multibyte | SSE |
| groq, mistral (ChatCompletions) | 3 | SSE |
| gemini | plain, multibyte × {json-array, sse}; thinking sse | both endpoints |
| ollama | plain, multibyte, plain-cold | ndjson |
| perplexity | **none** | no credits on this account |
| claude, groq, mistral, openai, gemini, ollama | `*_tools_stream` | tool calls, see below |

`perplexity()` is the one streaming provider with no fixture, so its
`handle_stream()` will be refactored without a net. Verify that path with
`.dry_run` and a manual read, or record a fixture if credits ever appear.

Ollama is recorded with `.think = FALSE`. With the default thinking trace the
fixture is 218 KB / 430 lines, and because the current loop sleeps 0.25s per
line it takes nearly two minutes to replay. The wire format under test is
unchanged.

Gemini is recorded under **both** endpoints on purpose. The endpoint tidyllm
sends today returns a chunked JSON array rather than SSE, so the existing
buffer-and-match parser was the only thing that could have worked; the `alt=sse`
recordings are what Phase A migrates to, and having both makes parity across the
switch provable. The three `*_sse` fixtures are expected to **error** under the
current parser, and the replay suite asserts exactly that so the day they start
parsing is visible.

## Truncated-stream behaviour, measured 2026-08-15

Replaying only the first chunk closes the connection without the provider's
terminal event. This is 0.6.0 acceptance criterion 4, and it is reported rather
than asserted, because a suite that hangs is worse than one that tells you where
the hangs are.

| provider | truncated stream |
|---|---|
| groq, mistral (ChatCompletions) | **hangs** |
| openai (Responses) | **hangs** |
| claude | **hangs**, except when truncation lands mid-event, where the SSE parse error surfaces |
| gemini | raises |
| ollama | raises |

This refines the workplan's "four of six providers hang". By provider family it
is three; Claude's behaviour depends on where the connection is cut, which is
worse than a consistent hang because it will not reproduce reliably. Ollama
raises only because of the completion check added in `7d6a7db`; before that it
crashed on the empty read instead.

## Tool-call streams

Recorded 2026-08-15, after Phase A, and now the offline test bed for the
streaming tool loop. Replayed by `local_tests/features/stream_tools_replay.R`,
which characterizes the wire shapes and then runs each provider's
`assemble_stream_body()` over them, asserting that the unchanged tool
generics find the calls in the result.

They have **no baseline entry** and are excluded from the parity block in
`stream_replay.R`: they were recorded after the refactor, so there is no
pre-refactor reply to compare against, and the text reply is not the interesting
part of them. They are still covered by the transport and truncation blocks.

When these were recorded, every provider still rejected `.stream = TRUE`
together with `.tools` in `validate_inputs()`, so the requests could not be
built through the public API. The recorder goes through
`<provider>_build_chat_request()` and sets the stream flag on the returned
`$body` instead. Those guards are gone now, so a re-recording could use
`.stream = TRUE` directly; the bypass is kept because it is also how a fixture
gets recorded for a provider whose guard is still in place.

What the recordings show, per provider:

| provider | tool calls arrive as | grouping key | fragmented? |
|---|---|---|---|
| claude | `input_json_delta` fragments | content block index | **yes**, mid-token |
| groq, mistral | `tool_calls` delta objects | `tool_calls[].index` | not here, but not guaranteed |
| openai (Responses) | complete items on `response.completed` | `output` array position | no |
| gemini | `functionCall` parts, args already parsed | part order | no |
| ollama | complete `tool_calls` per line | `function.index` | no |

Two results that shaped the assemblers:

- **Claude is the only provider that forces per-block accumulation.** Its
  fragments split mid-token (`{"city` / `": "B` / `er` / `lin"}`) and welding
  them all into one buffer yields `{"city": "Berlin"}{"city": "Reykjavik"}`,
  which does not parse. The suite asserts both halves of that: the welded string
  fails to parse and the per-block strings succeed. One recorded fragment is the
  empty string, so the guard against reading that as a terminator is exercised.
- **OpenAI needs no accumulation at all.** The pump keeps exactly one event for
  that stream, `response.completed`, and it carries the complete `output` array
  with fully-formed `function_call` items; the assembler is a projection.

The suite also asserts that the pump still *retains* these events. Each
provider's `parse_stream_event()` decides what to keep, and a keep flag flipped
for a plausible-looking reason would strip the tool calls out of `raw_data`
while every existing streaming test stayed green.

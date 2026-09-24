# tidyllm 0.7.0 (development version)

Work in progress. This section covers the release's first two features: a
provider that talks to a locally installed Claude CLI, with the transport work it
needed, and a web search tool that works with every provider.

## `claude_cli()`: chat through the Claude CLI you already have

`claude_cli()` is a provider that sends nothing over the network itself. It runs
the `claude` command line tool installed on your own machine and reads its JSON
output back, using the login that tool already has. There is no API key to set,
and the usage counts against whatever plan the CLI is signed in to.

```r
llm_message("Explain R's S7 classes in three sentences.") |>
  chat(claude_cli())
```

Everything the CLI reports comes back through the usual accessors:
`get_metadata()` carries the token counts including cache reads, and its
`api_specific` column adds the session id, the stop reason, the number of turns
and `total_cost_usd`, which is the real dollar cost of that one call.

Streaming and `send_chat()` both work, so a CLI call can print as it arrives or
run in the background of a session that keeps going.

The CLI is an agent rather than a plain completion endpoint: left alone it can
read files, edit them and run shell commands. tidyllm turns all of that off,
because a call to `chat()` that quietly edits files in your working directory is
not what the rest of the package does. Pass `.cli_tools` to allow specific tools
back, or `.cli_tools = TRUE` to hand over to the CLI's own configuration.

```r
llm_message("Summarise the DESCRIPTION file here.") |>
  chat(claude_cli(.cli_tools = c("Read", "Glob")))
```

`.stateful = TRUE` leaves the conversation on the CLI's side: the first call
records a session id, and later calls resume it instead of replaying the whole
history.

`.json_schema` maps onto the CLI's own structured-output flag, so
`tidyllm_schema()` works here as it does everywhere else.

`claude_cli()` does not take `.tools`. The CLI runs its own tool loop and never
exposes tool-call blocks to a caller, so tidyllm's tool loop has nothing to act
on; `chat(claude_cli(), .tools = ...)` says so rather than silently ignoring it.

`claude_cli()` looks for the CLI on the PATH and, failing that, in the places the
installers write to. A GUI R session does not inherit the PATH from your shell
profile, so RStudio in particular can miss a perfectly good install in
`~/.local/bin`. To point at it yourself, set

```r
options(tidyllm_claude_cli_path = "/path/to/claude")
```

in your `.Rprofile`, or the `TIDYLLM_CLAUDE_CLI` environment variable, or pass
`claude_cli(.binary = "/path/to/claude")` for a single call.

The provider needs `processx`, which is in `Suggests` and checked where it is
used, so nothing changes for anyone who does not call it.

## `websearch_tool()`: web search for any model

Until now only some providers could search the web, each through its own
built-in tool. `websearch_tool()` gives the same ability to every provider that
supports tools, including local models through `ollama()` and `llamacpp()`:

```r
llm_message("Which central banks changed interest rates this week?") |>
  chat(ollama(), .tools = websearch_tool())
```

The model decides when to search and chooses the query; nothing else is up to
it. The number of results, whether full page text is included and how long that
text may be are fixed when you create the tool, so a model cannot run up the
search bill. Each search returns numbered results with title, URL, publication
date and an excerpt, and the tool asks the model to cite the URLs it uses.

The first search service is Tavily. It needs a `TAVILY_API_KEY`; the free plan
gives 1,000 searches a month without a credit card. Tavily's own options, such
as `topic = "news"`, `time_range = "week"` or `include_domains`, pass through
`...`, and a misspelled option is caught when the tool is created. A search that
fails, for instance because the monthly credits are used up, comes back to the
model as a message rather than stopping the conversation.

## Tool results reach Claude and Gemini as plain text

A tool that returns text used to reach `claude()` and `gemini()` in R's printed
form, `[1] "..."`, with every line break and quote escaped. They now get the text
as it is. Tools that return other values, such as a data frame or a named
vector, are still printed, as before.

## Streaming is no longer tied to HTTP

The stream pump used to ask httr2 directly whether a connection was finished and
how to close it. Those two questions now go through the provider, alongside the
reader that was already there, which is what lets a stream come from a local
process instead of an HTTP response. `send_chat()` likewise asks the provider how
to start, rather than always building an httr2 promise. No behaviour changes for
the twelve HTTP providers.

# tidyllm 0.6.0

**tidyllm no longer has to block.** A script can fire a request and keep working,
several prompts can run at once, and a Shiny app can stream tokens into its UI
without freezing itself or anyone else's session. The headline verbs are
`send_chat()` and `parallel_chat()`; underneath them sit a shared streaming pump
and a chat pipeline split that every provider now goes through. Streaming and
tool calls also stop being mutually exclusive.

No new required dependency: `later` and `promises` are in `Suggests` and checked
where they are used, and the Shiny path needs neither `promises` nor `coro`.

## `send_chat()`: a chat that does not block the session

There are now three ways to run a chat. `chat()` when you want the answer now.
`send_batch()` when you have thousands of prompts and want them at half price
overnight. And new in 0.6.0, `send_chat()` when you have *one* slow request and
a session you would rather keep using. All three end in an `LLMMessage`, and the
last two share the same `check_job()` / `fetch_job()` vocabulary.

```r
job <- llm_message("Summarise this 400-page report") |>
  send_chat(claude(), .stream = TRUE)

while (check_job(job) == "running") {
  do_something_else()
  cat("\r", nchar(get_partial(job)), "characters so far")
}

reply <- fetch_job(job)          # the LLMMessage chat() would have returned
```

`get_partial()` is the text so far and `cancel_job()` stops the request.
`.on_chunk` is the push form of the same thing: a function called with each
delta as it arrives, which is all a Shiny app needs to render a reply
token-by-token into a `reactiveVal`, with no `promises` and no `coro` involved.

Nothing runs on a thread or in a second process. The request is driven from R's
own event loop, waiting on curl's file descriptors rather than on a timer, in
the gaps between whatever else the session is doing. Two consequences follow
from that and are worth knowing: several jobs run genuinely concurrently, and a
blocking call of your own pauses them all for its duration.

Requires the `later` package, and `promises` as well for `.stream = FALSE`.
Neither is a new hard dependency; both are checked at the point of use.

Known limits of the first cut: a job with `.tools` performs its tool rounds
without yielding, so the session pauses for their duration, and a streamed job
is not retried after a transient 429 the way `chat()` is.

`check_job()` and `fetch_job()` are S3 generics now rather than a chain of
`if`s, so batch jobs, background research jobs and chat jobs are one vocabulary
reached by one mechanism.

## `parallel_chat()`: many prompts at once

```r
answers <- parallel_chat(list(physics = llm_message("What is a photon?"),
                              biology = llm_message("What is a ribosome?")),
                         claude())
```

Performs a list of messages concurrently against one provider and returns their
replies in the same order under the same names. Measured on three one-sentence
questions to `claude()`: 2.1 seconds against 5.4 for the same three in a loop.

`.max_active` bounds how many are in flight and `.throttle` caps requests per
second; both matter more than they look, because `httr2` applies retries across
the whole set rather than per request, so a high `.max_active` against a
rate-limited provider is a good way to collect 429s.

A failed request is returned in its own slot as the condition that failed,
rather than as a hole that would silently shorten a downstream `map()`.
Streaming and tool calls are refused rather than quietly ignored: use
`send_chat()`, which can hold several conversations at once.

## Shiny: an example app and an article

`tidyllm_example_app("model_explainer")` runs a small Shiny app that ships with
the package. It fits a linear model to a public dataset and streams two
explanations of the coefficients side by side, one in plain English and one from
a sceptical referee, while the app stays responsive. Every number the model sees
is computed in R and pasted into the prompt verbatim; the model does the
narrating and none of the arithmetic.

It defaults to a local `ollama()` model, so it runs with no API key and no spend,
and a dropdown switches it to Claude, OpenAI or Gemini. The source is a single
file, and it is the reference implementation for the things a real app needs:
`.on_chunk` into a `reactiveVal`, a status observer for the failures that carry
no delta, `cancel_job()` on a button and on `session$onSessionEnded()`, and a
follow-up turn on the immutable `LLMMessage`.

The new article *Using tidyllm in Shiny* walks through those patterns and closes
with what to watch out for, including one worth knowing before you design a UI: a
streaming `send_chat()` returns when the response headers arrive, so a server
that answers one request at a time (a stock Ollama) makes a second concurrent
job wait, while cloud providers stream both at once.

`shiny` and `wooldridge` are new in `Suggests`, for the app and one of its
datasets.

## Streaming and tool calls work together

`.stream = TRUE` and `.tools` used to be mutually exclusive: every provider
raised "Streaming is not supported for requests with tool calls" if both were
given. That restriction is gone for `claude()`, `openai()`, `gemini()`,
`ollama()`, `groq()`, `mistral()`, `deepseek()`, `openrouter()`, `llamacpp()`,
`azure_openai()` and `chat_completions()`. The reply streams to the console, the
tool calls run when the stream ends, and each follow-up round streams too.

```r
llm_message("What is the weather in Berlin and Reykjavik?") |>
  chat(claude(), .tools = weather_tool, .stream = TRUE)
```

The reason it was blocked is that the tool loop reads tool calls out of a
complete response body, which a stream never produced; it produced a list of
events instead. A new `assemble_stream_body()` generic folds those events
back into the body shape, so `has_tool_calls()`, `extract_tool_calls()`,
`run_tool_calls()` and `append_tool_messages()` are reused without a single
streaming-specific branch. Streamed and blocking responses now carry the same
`raw$content`, which is also what the async driver needs.

Only Claude requires real reassembly: it streams tool arguments as JSON
fragments that split mid-token and interleave between two concurrent calls, so
they are accumulated per content block rather than into one buffer. OpenAI's
`response.completed` event already carries fully-formed calls, and Gemini and
Ollama send their calls parsed.

Details worth knowing, all of them cases that only exist because the two can now
be combined:

* A streamed reply whose tool call is cut off by `max_tokens` mid-arguments no
  longer raises. The partial arguments are dropped and the turn ends on its own
  `stop_reason`, so a reply the user has already watched arrive is not thrown
  away.
* Claude's `thinking` blocks and their signatures survive assembly, so
  `.thinking = TRUE` works together with `.stream` and `.tools`. Built-in tools
  such as `claude_websearch()` keep their arguments too.
* Streamed logprobs still work. They used to be read by a separate branch that
  walked the per-chunk deltas; they are now collected into the same place a
  blocking response carries them, and both transports take one path.
* A provider that streams but has no assembler raises rather than silently
  skipping its tools and returning the model's preamble as the answer.

## Internal: the chat pipeline

Nothing user-visible changed here, but it is the largest structural change in
the release. Every `*_chat()` used to be one function body welding request
construction, the HTTP call, the tool loop and `add_message()` together, which
meant nothing but `*_chat()` itself could reach the middle of it.

* Twelve providers now split into `<provider>_build_chat_request()` plus a thin
  wrapper. The builder returns everything the response handling needs; the
  shared `finish_chat_response()` runs the tool loop, extracts the reply and
  metadata, tracks rate limits and appends the message.
* Whether a request streams is decided at build time rather than at perform
  time, because every provider commits to streaming in the request itself:
  Gemini in the URL path, the rest in the request body.
* `.dry_run = TRUE` still returns the bare `httr2` request, unchanged.
* `ratelimit_from_header()` and `parse_logprobs()` gained an `APIProvider`
  default returning `NULL`, and the providers that inherit a method they should
  not use override it back. Whether a provider reports rate limits or logprobs is
  now a property of its class rather than a flag each call site had to set
  correctly. A new `api_compatible` class covers `chat_completions(.compatible =
  TRUE)`, which is a third-party endpoint speaking the OpenAI dialect and does
  not return OpenAI's rate limit headers.
* `interpret_chat_response()` splits response interpretation away from transport,
  so a driver holding a response from `req_perform_promise()` or
  `req_perform_parallel()` can reach the same handling the blocking path uses.
* A streamed response is now interpreted by exactly the same code as a blocking
  one. `extract_metadata_stream()`, a generic with six methods, is gone: once
  `assemble_stream_body()` turns the events into a response body, the reply
  comes from `parse_chat_response()` and the metadata from `extract_metadata()`,
  and the streaming branch ends in `interpret_chat_response()` like every other
  path. Beyond deleting the duplicate, this is what makes an incomplete
  assembler detectable: the reply used to come from the pump's own text
  accumulator, so an assembler could drop content and no plain streaming test
  would notice.
* `process_tool_loop()` performs its follow-up rounds through a closure the
  caller supplies rather than by calling `perform_chat_request()` itself. It had
  no business deciding how a round is performed, and deciding it twice is what
  made `openai_chat(.stateful = TRUE)` apply its retry to the opening request
  only (see the bug fixes below). It is also the seam the event-loop driver
  needs, which will hand in a non-blocking performer.
* Streamed `gemini()` metadata therefore reports the same `api_specific` fields
  as a blocking call: `cachedContentTokenCount`, `avgLogprobs` and
  `groundingMetadata` appear, and the streaming-only `token_details` entry (a
  copy of the raw `usageMetadata`) is gone. Token counts, `finishReason` and
  `thinking_tokens` are unchanged.

## Bug fixes (this development cycle)

* `pdf_page_batch(.page_range=)` rendered the wrong pages. The text was subset to
  the requested range but the images were rendered from the position within that
  subset, so `.page_range = c(3, 5)` paired page 3's text with page 1's image.
  The page numbers are now carried through the whole function.

* `pdf_page_batch()` returns a **named** list, `page_1`, `page_2` and so on, with
  the numbers from the original document. `parallel_chat()` preserves names, so a
  reply can now be traced back to its page.

* A verb a provider does not implement at all says so. `send_chat()` on
  `chat_ellmer()` used to fail with a generic complaint about unsupported
  arguments; it now names the verb and the provider and explains that
  `chat_ellmer()` hands the conversation to an ellmer `Chat` object rather than
  building a request, so there is nothing for tidyllm to stream. The same holds
  for `parallel_chat()`, and for any other verb/provider pair that was never
  registered.

* Deprecation warnings for `claude(.file_ids=)` and `gemini(.fileid=)` no longer
  tell the user the feature "was likely used in the tidyllm package" and ask them
  to file an issue. The pipeline split moved these calls one frame deeper, which
  changed how `lifecycle` resolved the calling environment.

* `openai_chat(.stateful = TRUE)` recovers from an expired server-side context
  on any request of the turn, not just the first. Each round of the tool loop
  used to bypass the retry entirely, so a context that expired mid-conversation
  failed outright. The rebuild itself is still attempted only on the opening
  request, and now says why: the body it reconstructs is the conversation as it
  stood before the turn began, so using it later would discard the tool calls
  and results exchanged since. A round that falls back also tells the loop which
  request it actually sent, so the next round builds on that one.

* `perplexity()` attaches its search results to the metadata again. The hook
  read them from the response object rather than from the parsed body inside it,
  so `get_metadata()$api_specific$search_results` had always been `NULL`.
  Streamed replies carry them too, since the assembler now merges Perplexity's
  response-level `search_results` and `citations` fields.

## Streaming

* Every provider now streams through one shared pump. The six hand-rolled
  `repeat` loops are gone; a provider customises streaming by implementing
  `parse_stream_event()` and declaring its `stream_transport`, never by writing
  another loop.
* **A truncated or abnormally terminated stream raises instead of hanging.** The
  old loops relied solely on a provider-specific terminal event, so a closed
  connection, a mid-stream provider error or an unrecognised `finish_reason`
  span forever. The pump checks `resp_stream_is_complete()` on every empty read.
  Measured before the change, `openai()`, `claude()` and the whole
  ChatCompletions family hung; all providers now raise.
* `.timeout` finally applies to streaming, as an idle deadline between events
  rather than a total, so a long generation is not killed for being long. The
  streaming path previously had no timeout backstop at all.
* `perplexity()` streams now terminate on any `finish_reason`, not only
  `"stop"`; a reply cut short by `"length"` used to spin.
* `gemini()` streaming moved to the `alt=sse` endpoint. Without that query
  parameter the endpoint returns a pretty-printed JSON array in chunks with no
  SSE framing, which is why tidyllm buffered the text and pattern matched it.
  Gemini streaming metadata now reports `finishReason` and `thinking_tokens`
  alongside the token counts.
* Thinking deltas are distinguished from reply text on every provider that emits
  them, rather than being concatenated into the reply or dropped silently.
* The console path opens its connection with `blocking = TRUE` instead of
  spinning on empty reads. Output is unchanged; cadence may differ slightly, and
  Ollama no longer needs its 0.25s sleep per line.

## Credential handling

* `gemini()` no longer puts the API key in the URL query string. All fifteen Gemini
  request builders now send it as a redacted `x-goog-api-key` header, so
  `.dry_run = TRUE`, `req_verbose()` and any httr2 error carrying the URL no longer
  print the live key.
* `azure_openai()` marks its `api-key` header as redacted. It was stored as an
  ordinary string, so the key survived `print()` and `serialize()` on the request
  object.

## Bug fixes

* `chat_completions()` could not be reached through `chat()` with any common
  argument at all; `chat(..., .dry_run = TRUE)` and every other shared argument
  raised "not supported by the provider's `chat()` function". Provider functions
  that forward through `...` are now recognised as accepting any common argument.
* A missing API key raised `object 'api' not found` instead of the intended
  instruction naming the environment variable to set.
* `chat_ellmer()` sent the last user message twice. The full history, including the
  final user turn, was written onto the cloned ellmer `Chat` and then that same turn
  was sent again by `$chat()`. It now sets only the preceding turns, and it uses
  ellmer's public `$set_turns()` rather than reaching into the object's private
  environment.
* `chat_ellmer()` accepted `.stream = TRUE` and silently performed a non-streaming
  request. It now streams through ellmer's `$stream()`, and `get_metadata()` reports
  `stream = TRUE` for those replies.
* `claude_chat()` ignored `.max_tries` and always used the default of 3.
* `ollama_chat()` gains `.max_tries`, which was hardcoded to 3.
* The Ollama stream loop no longer crashes with a JSON lexer error on an empty read,
  and it raises instead of looping when the connection completes before the model
  reports `done`.
* The ChatCompletions stream loop recorded the last event twice. The duplicate is
  gone and the final usage-only chunk is now recorded where it is produced rather
  than on the `[DONE]` branch.
* `pdf_page_batch()` no longer emits one deprecation warning per page; it uses
  `.media = img()` instead of the deprecated `.imagefile`.
* Deleted the duplicate `openai` and `chatgpt` bindings in `R/api_chat_completions.R`,
  which shipped as dead code shadowed by the Responses API definitions.
* Deleted the `generate_callback_function()` generic, which wrote into an environment
  that is never created and would have errored if called.
* `R/api_ellmer.R` no longer short-circuits at the top level when ellmer is absent,
  which would have broken the NAMESPACE exports; `chat_ellmer()` checks for ellmer at
  call time instead.

# tidyllm 0.5.2

A bugfix release. No new providers, verbs, or media types.

## JSON schemas (structured output and tool definitions)

* `tidyllm_schema()` now sets `additionalProperties: false` on **every** object node of the assembled schema, not just the root. Schemas containing `field_object(..., .vector = TRUE)`, i.e. an array of objects, were rejected with an HTTP 400 `invalid_json_schema` by every provider enforcing OpenAI strict mode: `openai()` itself and any `openrouter()` route to OpenAI or Azure. The same recursive normalization is applied at the provider boundary, so raw list schemas and `ellmer` types are covered as well.
* `gemini()` strips `additionalProperties` recursively before sending a schema. Gemini rejects the key on any node, and previously only the root was stripped.
* Tool definitions get the same treatment. A `tidyllm_tool()` with a `field_object()` argument produced a nested object node without `additionalProperties`, which `openai()` rejected with a 400 because tidyllm sends `strict = TRUE` on tool schemas.
* The schema name is now read with `attr(..., exact = TRUE)`. Passing a hand-written list schema without a `name` attribute previously partial-matched the `names` attribute, so the property names went on the wire as the schema name and strict providers rejected the request.
* Note that an explicit `additionalProperties = TRUE`, for instance from `ellmer::type_object(.additional_properties = TRUE)`, is now normalized to `FALSE`, since that is what strict mode requires.

## Error messages

* Provider errors coming through an OpenAI-compatible gateway are no longer empty. `openrouter()` reports a generic `"Provider returned error"` in `error$message` and keeps the real upstream diagnostic in `error$metadata$raw`; tidyllm now unwraps that payload and names the upstream provider in the error it raises. This is what made the schema bug above so hard to diagnose in the field. It applies to the whole ChatCompletions family and to `azure_openai()`.
* The error type falls back to `error$code` when a provider reports no `error$type`, so the error header is no longer `Type: NULL`.

## Token metadata

* `get_metadata()` gains two top-level columns: `cached_tokens` (prompt tokens served from the provider's cache, the billing-relevant number) and `cache_creation_tokens` (cache writes; Claude only). Providers that do not report cache usage return `NA_integer_`, so "no caching" and "cache miss" stay distinguishable. Raw provider fields remain in `api_specific`, so existing code keeps working.
* Cache counts are now read where they were previously discarded: `openai()` (Responses API), `gemini()`, `deepseek()`, `chat_ellmer()`, and the whole ChatCompletions family including `openrouter()`. `openrouter()` additionally reports `cache_discount` in `api_specific`.
* `gemini()` and `groq()` replies now report `stream = FALSE` instead of `NA`, so a metadata tibble mixing providers is consistent.
* Claude streaming metadata is no longer a stub. Streamed replies now report `stop_reason`, `id`, `stop_sequence`, cache tokens and the thinking trace, matching the non-streaming path.

## Provider fixes

* `gemini()` tool calls work again. Gemini now rejects a `functionCall` part whose `thoughtSignature` was dropped ("Function call is missing a thought_signature"), which broke every multi-round tool call. tidyllm sends the model's parts back verbatim.
* `claude_websearch()` defaults to the current `web_search_20260318` tool version and gains `.allowed_callers` and `.response_inclusion`. Since `web_search_20260209` the API defaults `allowed_callers` to the code execution tool, which only models with programmatic tool calling support; web search therefore failed with a 400 on Claude Haiku 4.5 and other older models. tidyllm now sends `allowed_callers = "direct"` by default, so web search works on every model again. Pass `.allowed_callers = "code_execution_20260120"` for the dynamic filtering path.
* `ellmer_tool()` no longer produces a `tidyllm_field` whose type carries a stray name, and it uses the built-in tool's own description when `ellmer` provides one. `ellmer` is now declared as `Suggests: ellmer (>= 0.4.0)`, which is the version tidyllm's tool conversion actually requires.
* The `.cache` documentation for `claude_chat()` now states the per-model minimum cacheable prompt length. Anthropic silently caches nothing below that floor, and the floor is higher on the cheap models (4096 tokens on Haiku 4.5) than on Sonnet 5 (1024).

# tidyllm 0.5.1

## Anthropic API migration

The Anthropic Messages API changed for current-generation models (Claude Sonnet 5, Opus 4.7 and newer): the fixed-budget thinking interface and the sampling parameters `temperature`, `top_k`, and `top_p` are rejected with a 400 error. tidyllm 0.5.1 tracks these changes:

* `.thinking = TRUE` in `claude_chat()` and `send_claude_batch()` now maps to adaptive thinking (`thinking: {type: "adaptive"}`) on models that support it (Claude Sonnet 4.6, Opus 4.6 or newer). Older models keep the `budget_tokens` interface; `.thinking_budget` only applies there.
* New `.effort` argument on `claude_chat()` and `send_claude_batch()`: one of `"low"`, `"medium"`, `"high"`, `"xhigh"`, `"max"`. Controls thinking depth and token spend on models with adaptive thinking; replaces the thinking budget as the depth control.
* Sampling parameters passed to a model that rejects them now raise a clear client-side error before any request is sent. Older models accept them as before.
* Structured output requests moved from the deprecated top-level `output_format` parameter to `output_config.format`; the `structured-outputs` beta header is no longer sent (the feature is generally available).
* Response parsing and metadata extraction now handle thinking blocks in any position of the response content; batch fetching collapses text blocks correctly when thinking is enabled.
* `claude_websearch()` defaults to the `web_search_20260209` tool version with dynamic result filtering, and gains `.max_uses`, `.allowed_domains`, `.blocked_domains`, and `.version` arguments. Pass `.version = "web_search_20250305"` for models older than Claude Sonnet 4.6 / Opus 4.6.

## Prompt caching

* New `.cache` argument on `claude_chat()` and `send_claude_batch()`. `.cache = TRUE` enables Anthropic prompt caching with the default 5-minute time to live; `.cache = "1h"` requests a one-hour time to live. In `claude_chat()` the cache breakpoint is placed automatically at the end of the request; in `send_claude_batch()` the shared system prompt is cached across all requests in the batch, which is where batch workloads save the most.
* `get_metadata()` for Claude replies now reports `cache_creation_input_tokens` and `cache_read_input_tokens` in `api_specific`, so cache hits are verifiable.

## Other changes

* `gemini_embedding()` default model updated from `gemini-embedding-2-preview` to the generally available `gemini-embedding-2`.

# tidyllm 0.5.0

## Unified Media Interface

### `.media` argument on `llm_message()`

All non-text content now attaches to messages through a single `.media` argument that accepts any combination of media types. Pass a single object or a list:

```r
# Single image
llm_message("What is in this image?",
            .media = img("photo.jpg")) |>
  chat(claude())

# Multiple images in one message
llm_message("Describe the difference between these two images.",
            .media = list(img("before.jpg"), img("after.jpg"))) |>
  chat(openai())

# Mixed types: image + PDF together
llm_message("Does this figure match what is reported in Table 2?",
            .media = list(img("figure_3.png"),
                          pdf_file("paper.pdf", pages = 1:8))) |>
  chat(gemini())
```

### New media constructors

Three new constructors join `img()`:

- **`audio_file(path)`**: attach audio inline; supported by `gemini()`, `openrouter()`, and `mistral()` (Voxtral models)
- **`video_file(path)`**: attach video inline; supported by `gemini()` and `openrouter()`
- **`pdf_file(path, pages, .text_extract)`**: attach a PDF; Claude and Gemini receive the binary file (preserving layout, tables, and scanned content); all other providers receive extracted text automatically

```r
# Transcribe a recording
llm_message("Summarise what is discussed in this interview.",
            .media = audio_file("bosch_interview.mp3")) |>
  chat(gemini())

# Analyse a video clip with a JSON schema
video_schema <- tidyllm_schema(
  title      = field_chr("Title or subject of the clip"),
  era        = field_chr("Approximate decade or period depicted"),
  key_people = field_chr("Names mentioned, semicolon-separated")
)

llm_message("Analyse this video clip.",
            .media = video_file("documentary.mp4")) |>
  chat(gemini(), .json_schema = video_schema)

# Extract references from a scanned PDF (binary path, no OCR needed)
llm_message("Extract all references in APA format.",
            .media = pdf_file("1995_Neal_Industry_Specific.pdf")) |>
  chat(claude(), .json_schema = ref_schema)

# Force text extraction for any provider
llm_message("Summarise this report.",
            .media = pdf_file("annual_report.pdf", .text_extract = TRUE)) |>
  chat(openai())
```

### Multi-image support

All providers that accept images now handle multiple images per message. Pass them as a list inside `.media`. Claude supports up to 600 images per message; Gemini up to 3,600.

## Provider Files API

A unified set of verbs manages files stored on provider servers. Upload once, reuse across many requests:

```r
# Upload; returns a tidyllm_file handle
report <- upload_file(gemini(), .path = "quarterly_report.pdf")

# Attach the handle to any message via .files
llm_message("What were the key results this quarter?",
            .files = report) |>
  chat(gemini())

llm_message("List the top three risks in the document.",
            .files = report) |>
  chat(gemini())

# Inspect and manage uploaded files
list_files(gemini())
file_info(gemini(), report)      # accepts a tidyllm_file or a plain ID string
delete_file(gemini(), report)
```

The same pattern works with `claude()` and `openai()`. Provider support:

- **Gemini**: PDFs, images, audio, video, plain text, CSV, HTML, and more; files expire after 48 hours
- **Claude**: PDFs and images; no automatic expiry
- **OpenAI**: 80+ formats including PDFs, Office documents (DOCX, PPTX, XLSX), source code, ZIP archives, and images; note that images uploaded via the Files API cannot be used for vision tasks in chat; use inline `img()` instead

A `tidyllm_file` is provider-specific: a file uploaded to Claude cannot be sent to Gemini. tidyllm validates provider match before every request.

## OpenAI Provider Rewrite

### Responses API

`openai()` now uses the [Responses API](https://developers.openai.com/api/reference/resources/responses) (`POST /v1/responses`). All existing workflows continue to work unchanged. New capabilities unlocked by the rewrite:

```r
# Reasoning effort for o-series models
llm_message("Prove that there are infinitely many primes.") |>
  chat(openai(.model = "o4-mini"), .reasoning_effort = "high")

# Stateful multi-turn conversations (server retains context by ID)
first  <- llm_message("My name is Alex.") |>
  chat(openai(), .stateful = TRUE)

second <- llm_message("What is my name?") |>
  chat(openai(), .previous_response_id = first)
```

Batch processing (`send_batch(openai())`) continues to use the Chat Completions endpoint internally.

### Built-in server-executed tools

```r
# Web search: the server runs the search, results appear in the reply
llm_message("What happened in AI research this week?") |>
  chat(openai(), .tools = openai_websearch())

# Code interpreter
llm_message("Plot a histogram of 1,000 standard-normal samples.") |>
  chat(openai(), .tools = openai_code_interpreter())

# Mix built-in and custom tools in one call
llm_message("Find today's EUR/USD rate and convert 500 EUR.") |>
  chat(openai(), .tools = list(openai_websearch(), my_converter_tool))
```

### OpenAI deep research

```r
# Background research job (slow; typically 5 to 30 minutes)
job <- llm_message("Survey the literature on causal inference with LLMs.") |>
  deep_research(openai(.model = "o4-mini-deep-research"), .background = TRUE)

check_job(job)
result <- fetch_job(job)
```

## New Provider: `chat_completions()`

A `chat_completions()` provider for any OpenAI-compatible endpoint (vLLM, LiteLLM, Together AI, Anyscale, and others), without having to repurpose `openai()`:

```r
llm_message("Hello!") |>
  chat(chat_completions(
    .api_url        = "https://api.together.xyz/v1/",
    .api_key_env_var = "TOGETHER_API_KEY",
    .model          = "meta-llama/Llama-3-8b-chat-hf"
  ))
```

## Provider Enhancements

### Mistral

- New `.reasoning_effort` parameter for Magistral thinking models (`"low"`, `"medium"`, `"high"`):
  ```r
  llm_message("Is this argument valid?", .media = pdf_file("proof.pdf")) |>
    chat(mistral(.model = "magistral-medium-latest"), .reasoning_effort = "high")
  ```

### OpenRouter

- Audio and video support: `audio_file()` and `video_file()` now work with OpenRouter and are routed to the underlying model's audio/video endpoint. Filter for capable models by the `audio` modality at openrouter.ai/models.

## Deprecations

The following are soft-deprecated with warnings in 0.5.0 and will remain as permanent aliases:

- **`.imagefile`** on `llm_message()`: use `.media = img(path)` instead
- **`.pdf`** on `llm_message()`: use `.media = pdf_file(path)` instead
- **`claude_upload_file()`, `claude_delete_file()`, `claude_file_metadata()`, `claude_list_files()`**: use `upload_file(claude())`, `delete_file(claude())`, `file_info(claude())`, `list_files(claude())` instead
- **`gemini_upload_file()`, `gemini_delete_file()`, `gemini_file_metadata()`, `gemini_list_files()`**: use the corresponding `upload_file(gemini())` etc. verbs instead
- **`.file_ids`** on `claude_chat()` and **`.fileid`** on `gemini_chat()`: upload with `upload_file()` and attach with `.files` on `llm_message()` instead

## Small Changes

- `file_info()` and `delete_file()` accept a `tidyllm_file` object directly in addition to a plain ID string
- Default model for `claude()` updated to `claude-sonnet-4-6`; fast model updated to `claude-haiku-4-5`
- Default model for `gemini()` updated to `gemini-2.5-flash`; default embedding model updated to `gemini-embedding-2-preview`
- Default model for `voyage_embedding()` updated to `voyage-4`
- Default model for `openai()` updated to `gpt-5.5` (released April 2026)
- Default model for `deepseek()` updated to `deepseek-v4-pro` (DeepSeek V4, released April 2026); `.thinking = TRUE` now enables thinking mode via the `thinking` body parameter instead of switching to the deprecated `deepseek-reasoner` model name; both `deepseek-v4-pro` and `deepseek-v4-flash` support thinking mode

---

# tidyllm 0.4.0

## New Providers

### OpenRouter (`openrouter()`)

Access to 300+ models from a single API key via OpenRouter. Supports chat, embeddings, model listing, and fallback routing across providers:

```r
# Chat with any model on OpenRouter
llm_message("What is the capital of France?") |>
  chat(openrouter(.model = "anthropic/claude-3.5-sonnet"))

# List available models
list_models(openrouter())

# Check account credits
openrouter_credits()

# Retrieve generation metadata (tokens, cost) for a completed request
openrouter_generation(generation_id)
```

OpenRouter also supports **provider fallback routing** — specify a list of fallback providers to use if the primary model is unavailable.

### llama.cpp (`llamacpp()`)

Full support for local [llama.cpp](https://github.com/ggml-org/llama.cpp) servers, including chat, embeddings, reranking, and model management:

```r
# Chat with a local llama.cpp server
llm_message("Explain R to a Python developer") |>
  chat(llamacpp())

# Generate embeddings
c("text one", "text two") |> embed(llamacpp())

# Rerank documents by relevance
llamacpp_rerank("best R package for LLMs", c("tidyllm", "ellmer", "httr2"))

# Model management
llamacpp_list_local_models()          # list models in the model directory
list_hf_gguf_files("Qwen/Qwen2.5-7B-Instruct-GGUF")  # browse HuggingFace GGUF files
llamacpp_download_model("Qwen/Qwen2.5-7B-Instruct-GGUF", "qwen2.5-7b-instruct-q4_k_m.gguf")
llamacpp_delete_model("path/to/model.gguf")
llamacpp_health()                     # check server status
```

## New Verbs

### `deep_research()`, `check_job()`, `fetch_job()`

A new `deep_research()` verb for running long-horizon research tasks. Currently supported by `perplexity()` via the `sonar-deep-research` model:

```r
# Blocking — waits for completion and returns an LLMMessage
result <- llm_message("Compare Rust and Go for systems programming") |>
  deep_research(perplexity())

get_reply(result)
get_metadata(result)$api_specific[[1]]$citations

# Background — returns immediately, poll with check_job() / fetch_job()
job <- llm_message("Summarize the latest EU AI Act developments") |>
  deep_research(perplexity(), .background = TRUE)

check_job(job)   # poll status
result <- fetch_job(job)  # retrieve when complete
```

`check_job()` and `fetch_job()` are type-dispatching aliases — they delegate to `check_batch()`/`fetch_batch()` for batch objects, or to `perplexity_check_research()`/`perplexity_fetch_research()` for research jobs.

## Provider Enhancements

### Perplexity

- New `perplexity_deep_research()`, `perplexity_check_research()`, `perplexity_fetch_research()` functions for async deep research via the `sonar-deep-research` model
- `.json_schema` structured output support for both `perplexity_chat()` and `perplexity_deep_research()`
- New `.search_domain_filter` parameter to restrict or exclude domains (up to 10, prefix `-` to exclude)
- New `.reasoning_effort` parameter for `perplexity_deep_research()` (`"low"`, `"medium"`, `"high"`)

### Thinking modes

Extended thinking is now available for two additional providers:

- **Gemini**: `.thinking_budget` parameter in `gemini_chat()` sets the token budget for internal reasoning (works with `gemini-2.5-flash` and `gemini-2.5-pro`). Thinking output is stored in `get_metadata()$api_specific[[1]]$thinking`.
- **DeepSeek**: `.thinking = TRUE` in `deepseek_chat()` switches to the `deepseek-reasoner` model and captures the reasoning trace in `get_metadata()$api_specific[[1]]$thinking`.

### Tool use improvements

- **Unified multi-turn tool loop** across all providers — the same logic now handles iterative tool calls for OpenAI, Claude, Gemini, Mistral, Groq, Ollama, and OpenRouter
- **Enum and vector support in tool schemas** — `field_fct()` (enum) and vector fields are now correctly serialised in tool parameter schemas
- Multi-turn and parallel tool use verified across all supported providers

### Ellmer compatibility

- `ellmer_tool()` converts ellmer `ToolDef` objects to tidyllm `TOOL` objects, enabling tools defined in ellmer (and packages like `btw`) to be used directly with tidyllm:
  ```r
  btw_tool <- ellmer_tool(btw::btw_tool_files_list_files)
  llm_message("List files in the R/ folder") |>
    chat(claude(), .tools = btw_tool)
  ```
- `ellmer_tool()` also supports provider-native **builtin tools** such as `ellmer::claude_tool_web_search()`:
  ```r
  web_search <- ellmer_tool(ellmer::claude_tool_web_search())
  llm_message("Latest AI safety news?") |>
    chat(claude(), .tools = web_search)
  ```
- `chat_ellmer()` lets you use any ellmer `Chat` object as a tidyllm provider, bridging the two ecosystems

### Groq

- `.json_schema` structured output support for `groq_chat()` and Groq batch requests
- Dropped legacy `json_object` mode in favour of proper JSON schema responses
- Fixed JSON attribute propagation from `send_groq_batch()` to `fetch_groq_batch()`

### Voyage AI

- `voyage_rerank()` — new reranking function using the `rerank-2` model; returns a tibble sorted by relevance score
- `.output_dimension` parameter for `voyage_embedding()` — control output vector size (256, 512, 1024, 2048) for Voyage-4 models

### OpenRouter

- `openrouter_embedding()` — generate embeddings via OpenRouter
- `openrouter_credits()` — check account balance and credit usage
- `openrouter_generation()` — retrieve token and cost metadata for a completed generation

### Azure OpenAI

- Fixed `check_azure_openai_batch()` and `fetch_azure_openai_batch()` to handle `null` values for `created_at` and `expires_at` fields returned by some deployments

## Small Changes / Housekeeping

- Default chat model for `ollama()` changed to `qwen3.5:4b` (faster, better instruction following for local use)
- Default embedding model for `ollama()` changed to `qwen3-embedding:0.6b`
- Verb dispatch refactored into a shared `dispatch_to_provider()` helper, reducing duplication across all verbs

## Bug Fixes

- Fixed JSON attribute not being propagated from `send_mistral_batch()` and `send_groq_batch()` to their respective fetch functions, causing structured-output batch results to be returned as raw text
- Fixed `perplexity_deep_research()` API request format for the async endpoint

# Version 0.3.5

## Key Improvements

- Rudimentary support for file uploads in `claude()`. At the moment only implemented for the `chat()` verb
```r
example_file <- here::here("vignettes","die_verwandlung.pdf") |> 
  claude_upload_file()

llm_message("Summarize the document in 100 words") |>
  chat(claude(.file_ids = example_file$file_id)) 
  
#> Message History:
#> system:
#> You are a helpful assistant
#> --------------------------------------------------------------
#> user:
#> Summarize the document in 100 words
#> --------------------------------------------------------------
#> assistant:
#> This document is the German text of Franz Kafka's novella
#> "Die Verwandlung" (The Metamorphosis), published through
#> Project Gutenberg. The story follows Gregor Samsa, a
#> traveling salesman who wakes up one morning transformed into
#> a monstrous insect-like creature. Unable to work and support
#> his family, Gregor becomes isolated in his room while his
#> family struggles with the burden of his transformation.
#> His sister Grete initially cares for him, bringing food
#> and cleaning his room, but over time the family's situation
#> deteriorates financially and emotionally. The story explores
#> themes of alienation, family duty, and dehumanization as
#> Gregor gradually loses his human identity and connection to
#> his family. Eventually, Gregor dies, and his family, though
#> initially grief-stricken, ultimately feels relieved and
#> optimistic about their future without the burden of caring
#> for him. The text includes the complete three-part novella
#> along with Project Gutenberg licensing information.
#> --------------------------------------------------------------  
```

- **Expanded Perplexity Support:** The `perplexity()` provider now supports  more Perplexity API parameters, allowing you to set reasoning and search effort.
- **Gemini Batches:** Batch support for `gemini()` with most functionality of `chat()` requests.


# Version 0.3.4
This release marks a **major internal refactor** accompanied by a suite of subtle yet impactful improvements. While many changes occur under the hood, they collectively deliver a more robust, flexible, and maintainable framework.

## Key Improvements

- **Robust Streaming:**
  - **New Streaming Backend:** Streaming is now handled via `httr2::req_perform_connection()` (httr2 ≥ 1.1.1), resulting in a more stable and reliable experience.
  - **Metadata for Streaming:** Streaming requests now also support metadata extraction, logprobs, and other features, making them even more informative.

- **Optimized Internal Processing:**
  - **S7 Methods Integration:** Improved handling of streams and chat parsing using more proper S7 methods instead of clunky old function generation.
  - **OpenAI Request Construction:** Both OpenAI and Azure OpenAI (along with their batch functions) now use a common request construction function to reduce code duplication and simplify maintenance.

- **Schema support:**
 -  New `field_object()` function to allow for nested schemata 

- **Expanded API Features:**
  - **JSON Schema Support:** 
    - `mistral()` now accepts the `.json_schema` argument.
    - `claude()` incorporates `.json_schema` via a JSON-extractor tool, in line with Anthropic's guidelines.
  - **Batch API for groq():** A new batch processing interface has been implemented for groq().

## Bug Fixes

- **claude() Batch Requests:** Fixed an issue where system prompts were not transmitted correctly in batch mode.
- **gemini() Prompt Handling:** Resolved a bug causing system prompts to be omitted from API calls in older versions.


# Dev-Version 0.3.3

## Thinking support in Claude
Claude now supports reasoning:

```r
conversation <- llm_message("Are there an infinite number of prime numbers such that n mod 4 == 3?") |>
   chat(claude(.thinking=TRUE)) |>
  print()
   
#> Message History:
#> system:
#> You are a helpful assistant
#> --------------------------------------------------------------
#> user:
#> Are there an infinite number of prime numbers such that n
#> mod 4 == 3?
#> --------------------------------------------------------------
#> assistant:
#> # Infinitude of Primes Congruent to 3 mod 4
#> 
#> Yes, there are infinitely many prime numbers $p$ such
#> that $p \equiv 3 \pmod{4}$ (when $p$ divided by 4 leaves
#> remainder 3).
#> 
#> ## Proof by Contradiction
#> 
#> I'll use a proof technique similar to Euclid's classic proof
#> of the infinitude of primes:
#> 
#> 1) Assume there are only finitely many primes $p$ such that
#> $p \equiv 3 \pmod{4}$. Let's call them $p_1, p_2, ..., p_k$.
#> 
#> 2) Consider the number $N = 4p_1p_2...p_k - 1$
#> 
#> 3) Note that $N \equiv 3 \pmod{4}$ since $4p_1p_2...p_k
#> \equiv 0 \pmod{4}$ and $4p_1p_2...p_k - 1 \equiv -1 \equiv 3
#> \pmod{4}$
#> 
#> 4) $N$ must have at least one prime factor $q$
#> 
#> 5) For any $i$ between 1 and $k$, we have $N \equiv -1
#> \pmod{p_i}$, so $N$ is not divisible by any of the primes
#> $p_1, p_2, ..., p_k$
#> 
#> 6) Therefore, $q$ is a prime not in our original list
#> 
#> 7) Furthermore, $q$ must be congruent to 3 modulo 4:
#> - $q$ cannot be 2 because $N$ is odd
#> - If $q \equiv 1 \pmod{4}$, then $\frac{N}{q} \equiv 3
#> \pmod{4}$ would need another prime factor congruent to 3
#> modulo 4
#> - So $q \equiv 3 \pmod{4}$
#> 
#> 8) This contradicts our assumption that we listed all primes
#> of the form $p \equiv 3 \pmod{4}$
#> 
#> Therefore, there must be infinitely many primes of the form
#> $p \equiv 3 \pmod{4}$.
#> --------------------------------------------------------------

#Thinking process is stored in API-specific metadata
conversation |> 
   get_metadata() |>
   dplyr::pull(api_specific) |>
   purrr::map_chr("thinking") |>
   cat()
   
#> The question is asking if there are infinitely many prime numbers $p$ such that $p \equiv 3 \pmod{4}$, i.e., when divided by 4, the remainder is 3.
#> 
#> I know that there are infinitely many prime numbers overall. The classic proof is Euclid's proof by contradiction: if there were only finitely many primes, we could multiply them all together, add 1, and get a new number not divisible by any of the existing primes, which gives us a contradiction.
#> 
#> For primes of the form $p \equiv 3 \pmod{4}$, we can use a similar proof strategy. 
#> 
#> Let's assume there are only finitely many primes $p_1, p_2, \ldots, p_k$ such that $p_i \equiv 3 \pmod{4}$ for all $i$. 
#> 
#> Now, consider the number $N = 4 \cdot p_1 \cdot p_2 \cdot \ldots \cdot p_k - 1$. 
#> 
#> Note that $N \equiv -1 \equiv 3 \pmod{4}$. 
#> 
#> Now, let's consider the prime factorization of $N$. If $N$ is itself prime, then we have found a new prime $N$ such that $N \equiv 3 \pmod{4}$, which contradicts our assumption that we enumerated all such primes.
#> 
> ...
```

## Bugfixes

- Bugfix for `gemini()`: Sytem prompts were not sent to the API in older versions

# Version 0.3.2

## Tool usage introduced to tidyllm

A first  tool usage system inspired by a similar system in `ellmer` has been introduced to tidyllm. At the moment tool use is available 
for `claude()`, `openai()`, `mistral()`, `ollama()`, `gemini()` and `groq()`: 
```r
get_current_time <- function(tz, format = "%Y-%m-%d %H:%M:%S") {
  format(Sys.time(), tz = tz, format = format, usetz = TRUE)
}

time_tool <- tidyllm_tool(
  .f = get_current_time,
  .description = "Returns the current time in a specified timezone. Use this to determine the current time in any location.",
  tz = field_chr("The time zone identifier (e.g., 'Europe/Berlin', 'America/New_York', 'Asia/Tokyo', 'UTC'). Required."),
  format = field_chr("Format string for the time output. Default is '%Y-%m-%d %H:%M:%S'.")
)


llm_message("What's the exact time in Stuttgart?") |>
  chat(openai,.tools=time_tool)
  
#> Message History:
#> system:
#> You are a helpful assistant
#> --------------------------------------------------------------
#> user:
#> What's the exact time in Stuttgart?
#> --------------------------------------------------------------
#> assistant:
#> The current time in Stuttgart (Europe/Berlin timezone) is
#> 2025-03-03 09:51:22 CET.
#> --------------------------------------------------------------  
```  
You can use the `tidyllm_tool()` function to define tools available to a large language model. 
Once a tool or a list of tools is passed to a model, it can request to run these
these functions in your current session and use their output for further generation context. 

## Support for DeepSeek added

tidyllm now supports the deepseek API as provider via `deepseek_chat()` or the `deepseek()` provider function. 
Deepseek supports logprobs just like `openai()`, which you can get via `get_logprobs()`. 
At the moment tool usage for deepseek is very inconsistent.

## Support for Voyage.ai and Multimodal Embeddings Added

Voyage.ai introduces a **unique multimodal embeddings feature**, allowing you to generate embeddings not only for text but also for images. 
The new `voyage_embedding()` function in **tidyllm** enables this functionality by seamlessly handling different input types, 
working with both the new feature as well as the same inputs as for other embedding functions. 

The new `img()` function lets you create image objects for embedding. You can mix text and `img()` objects in a list and send them to Voyage AI for multimodal embeddings:

```r
list("tidyllm", img(here::here("docs", "logo.png"))) |>
  embed(voyage)
#> # A tibble: 2 × 2
#>   input          embeddings   
#>   <chr>          <list>       
#> 1 tidyllm        <dbl [1,024]>
#> 2 [IMG] logo.png <dbl [1,024]>
```

In this example, both text (`"tidyllm"`) and an image (`logo.png`) are embedded together. The function returns a tibble where the `input` column contains the text and labeled image names, and the `embeddings` column contains the corresponding embedding vectors.

## New Tests and Bugfixes

- Several Bugfixes in `tidyllm_schema()` and `tidyllm_tool()`
- New Tests for less covered APIs. 

# Version 0.3.1 

 ⚠️ There is a bad bug in the latest CRAN release in the `fetch_openai_batch()` function that is only fixed in version 0.3.2. For the release 0.3.1. the `fetch_openai_batch()` function throws errors if the logprobs are turned off.

## Changes compared to last release

- **New schema field functions inspired by ellmer and more schema compatibility with ellmer:** If you have ellmer installed you can now directly use ellmer type objects in the  `.json_schema` option of api-functions. Moreover, `tidyllm_schema()` now accepts ellmer types as field definitions. In addition four ellmer-inspired type-definition functions`field_chr()`, `field_dbl()`, `field_lgl()` and `field_fct()` were added that allow you to set description fields in schemata

```r
 ellmer_adress <-ellmer::type_object(
    street = ellmer::type_string("A famous street"),
    houseNumber = ellmer::type_number("a 3 digit number"),
    postcode = ellmer::type_string(),
    city = ellmer::type_string("A large city"),
    region = ellmer::type_string(),
    country = ellmer::type_enum(values = c("Germany", "France"))
  ) 

person_schema <-  tidyllm_schema(
                person_name = "string",
                age = field_dbl("An age between 25 and 40"),
                is_employed = field_lgl("Employment Status in the last year")
                occupation = field_fct(.levels=c("Lawyer","Butcher")),
                address = ellmer_adress
                )

address_message <- llm_message("imagine an address") |>
  chat(openai,.json_schema = ellmer_adress)
  
person_message  <- llm_message("imagine a person profile") |>
  chat(openai,.json_schema = person_schema)
```

- Support for logprobs in `openai_chat()` and `send_openai_batch()` and new `get_logprobs()` function:
```r
badger_poem <- llm_message("Write a haiku about badgers") |>
    chat(openai(.logprobs=TRUE,.top_logprobs=5))

 badger_poem |> get_logprobs()
#> # A tibble: 19 × 5
#>   reply_index token          logprob bytes     top_logprobs
#>          <int> <chr>            <dbl> <list>    <list>      
#>  1           1 "In"       -0.491      <int [2]> <list [5]>  
#>  2           1 " moon"    -1.12       <int [5]> <list [5]>  
#>  3           1 "lit"      -0.00489    <int [3]> <list [5]>  
#>  4           1 " forest"  -1.18       <int [7]> <list [5]>  
#>  5           1 ","        -0.00532    <int [1]> <list [5]>  
```
- Bugfix in OpenAI metadata extraction
- New `ollama_delete_model()` function
- `list_models()` is now a verb supporting most providers.
```r
list_models(openai)
#> # A tibble: 52 × 3
#>    id                                   created             owned_by
#>    <chr>                                <chr>               <chr>   
#>  1 gpt-4o-mini-audio-preview-2024-12-17 2024-12-13 18:52:00 system  
#>  2 gpt-4-turbo-2024-04-09               2024-04-08 18:41:17 system  
#>  3 dall-e-3                             2023-10-31 20:46:29 system  
#>  4 dall-e-2                             2023-11-01 00:22:57 system  
```
- New synchronous `send_ollama_batch()` function to make use of the fast parallel request features of Ollama. 
- New batch functions for Azure Openai (thanks [Jia Zhang](https://github.com/JiaZhang42))
- New parameters for `openai()` reasoning models supported
- Default models updated for `perplexity()` and `gemini()`
- Fixed bug in the print method of `LLMMessage`

# Version 0.3.0 

**tidyllm 0.3.0** represents a major milestone for **tidyllm**

The largest changes compared to **0.2.0** are:

## New Verb-Based Interface  

- **New Verb-Based Interface**: Users can now use verbs like `chat()`, `embed()`, `send_batch()`, `check_batch()`, and `fetch_batch()` to interact with APIs. These functions always work with a combination of verbs and providers:
  - **Verbs** (e.g., `chat()`, `embed()`, `send_batch()`) define the type of action you want to perform.
  - **Providers** (e.g., `openai()`, `claude()`, `ollama()`) are an arguement of verbs and specify the API to handle the action with and take provider-specific arguments

Each verb and provider combination routes the interaction to provider-specific functions like `openai_chat()` or `claude_chat()` that do the work in the background. These functions can  also be called directly as an alternative more verbose and  provider-specific interface. 

### Old Usage:  
```r
llm_message("Hello World") |>
  openai(.model = "gpt-4o")
```

### New Usage:
```r
# Recommended Verb-Based Approach
llm_message("Hello World") |>
  chat(openai(.model = "gpt-4o"))
  
# Or even configuring a provider outside
my_ollama <- ollama(.model = "llama3.2-vision:90B",
       .ollama_server = "https://ollama.example-server.de",
       .temperature = 0)

llm_message("Hello World") |>
  chat(my_ollama)

# Alternative Approach is to use more verbose specific functions:
llm_message("Hello World") |>
  openai_chat(.model = "gpt-4o")
```

### Backward Compatibility:

  - The old functions (`openai()`, `claude()`, etc.) still work if you directly supply an `LLMMessage` as arguement, but issue deprecation warnings when used directly for chat.
  - Users are encouraged to transition to the new interface for future-proof workflows.

## Breaking Changes:

- The output format of embedding APIs was changed from a matrix to a tibble with an input column and a list column containing one embedding vector and one input per row.
- `R6`-based saved `LLMMessage` objects are no longer compatible with the new version. Saved objects from earlier versions need to be re-created

## Other Major Features:

- `gemini()` and `perplexity()` as new supported API providers. `gemini()` brings interesting Video and Audio features as well as search grounding to **tidyllm**.  `perplexity()` also offers well cited search grounded assitant replies
- Batch-Processing for `mistral()`
- New Metadata-Extraction function `get_reply_metadata()` to get information on token usage, or on other relevant metadata (like sources used for grounding)

## Improvements:
- Refactored Package Internals: 
  - Transitioned from `R6` to `S7` for the main `LLMMessage` class, improving maintainability, interoperability, and future-proofing.
  - Consolidated all API-specific functionality into dedicated files
  
# Version 0.2.7

## Major Features

- Batch API functions for the Mistral API
- Search Grounding with the `.grounding_threshold` argument added of  the `gemini_chat()` function allowing you to use Google searches to ground model responses to a search result Gemini models. For example, asking about the maintainer of an obscure R package works with grounding but does only lead to a hallucination without: 
```r
llm_message("What is tidyllm and who maintains this package?") |>
  gemini_chat(.grounding_threshold = 0.3)
```

- Perplexity as additional API provider available through `perplexity_chat()`. The neat feature of perplexity is the up-to-date web search it does with detailed citations. Cited sources are available in the `api_specific`-list column of `get_metadata()`
- `.json_schema` support for `ollama()` available with Ollama 0.5.0

## Improvements

- Metadata extraction is now handled by api-specific methods. `get_metadata()` returns a list column with API-specific metadata

# Version 0.2.6

## Large Refactor of package internals

- Switch from `R6` to `S7` for the main `LLMMessage` class
- Several bug-fixes for `df_llm_message()`
- API formatting methods are now in the code files for API providers
- Rate-limit header extraction for tracking and streaming callback generation are now methods for `APIProvider` classes 
- All api-specific code is now in the `api_openai.R`,`api_gemini.R`,etc. files
- Support for `as_tibble()` S3 Generic for `LLMMessage`
- Rate limit tracking and output for verbose mode in API-functions moved to a single function `track_rate_limit()`
- Unnecessary `.onattach()` removed
- Bugfix in callback method of Gemini streaming responses (still not ideal, but works)
- Embedding functions refactored to reduce repeated code
- API-key check moved into API-object method
- Slight refactoring for batch functions (there is still quite a bit of potential to reduce duplication)

## Breaking Changes

- Old `R6`-based `LLMMessage`-objects are not compatible with the new version anymore! This also applies to saved objects, like lists of batch files. 

## Minor Features

- Google Gemini now supports working with multiple files in one message for the file upload functionality

```r
here::here("local_wip","example.mp3") |> gemini_upload_file()
here::here("local_wip","legrille.mp4") |> gemini_upload_file()

file_tibble <- gemini_list_files()

llm_message("What are these two files about?") |>
  gemini_chat(.fileid=file_tibble$name)
```



# Version 0.2.5

## Major Features

Better embedding functions with improved output and error handling and new documentation. New article on using embeddings with **tidyllm**. Support for embedding models on azure with `azure_openai_embedding()`

## Breaking Changes

- The output format of `embed()` and the related API-specific functions was changed from a matrix to a tibble with an input column and a list column containing one embedding vector and one input per row.

# Version 0.2.4

## Refinements of the new interface

One disadvantage of the first iteration of the new interface was that all arguements that needed to be passed to provider-specific functions, were going through the provider function. This feels, unintuitive, because users expect common arguments (e.g., .model, .temperature) to be set directly in main verbs like `chat()` or `send_batch()`.Moreover,  provider functions don't expose arguments for autocomplete, making it harder for users to explore options. Therefore, the main API verbs now directly accept common arguements, and check them against the available arguements for each API.

## Bug-fixes

- New error message for not setting a provider in main verbs
- Missing export of main verbs fixed
- Wrong documentation fixed

# Version 0.2.3

## Major Interface Overhaul

`tidyllm` has introduced a verb-based interface overhaul to provide a more intuitive and flexible user experience. Previously, provider-specific functions like `claude()`, `openai()`, and others were directly used for chat-based workflows. Now, these functions primarily serve as provider configuration for some general verbs like `chat()`.

### Key Changes:
- **New Verb-Based Interface**: Users can now use verbs like `chat()`, `embed()`, `send_batch()`, `check_batch()`, and `fetch_batch()` to interact with APIs. These functions always work with a combination of verbs and providers:
  - **Verbs** (e.g., `chat()`, `embed()`, `send_batch()`) define the type of action you want to perform.
  - **Providers** (e.g., `openai()`, `claude()`, `ollama()`) are an arguement of verbs and specify the API to handle the action with and take provider-specific arguments

Each verb and provider combination routes the interaction to provider-specific functions like `openai_chat()` or `claude_chat()` that do the work in the background. These functions can  also be called directly as an alternative more verbose and  provider-specific interface. 
  
### Old Usage:
```r
llm_message("Hello World") |>
  openai(.model = "gpt-4o")
```

### New Usage:
```r
# Recommended Verb-Based Approach
llm_message("Hello World") |>
  chat(openai(.model = "gpt-4o"))
  
# Or even configuring a provider outside
my_ollama <- ollama(.model = "llama3.2-vision:90B",
       .ollama_server = "https://ollama.example-server.de",
       .temperature = 0)

llm_message("Hello World") |>
  chat(my_ollama)

# Alternative Approach is to use more verbose specific functions:
llm_message("Hello World") |>
  openai_chat(.model = "gpt-4o")
```

- **Backward Compatibility**:
  - The old functions (`openai()`, `claude()`, etc.) still work if you directly supply an `LLMMessage` as arguement, but issue deprecation warnings when used directly for chat.
  - Users are encouraged to transition to the new interface for future-proof workflows.
  
# Version 0.2.2

## Major Features

- Added functions to work with the Google Gemini API, with the new  `gemini()` main API-function
- Support for the file upload workflows for Gemini:
```r
#Upload a file for use with gemini
upload_info <- gemini_upload_file("example.mp3")

#Make the file available during a Gemini API call
llm_message("Summarize this speech") |>
  gemini(.fileid = upload_info$name)
  
#Delte the file from the Google servers
gemini_delete_file(upload_info$name)
```

- Brings video and audio support to tidyllm
- Google Gemini is the second API to fully support `tidyllm_schema()`
- `gemini()`-requests allow for a wide range of  file types  that can be used for context in messages
- Supported document formats for `gemini()` file workflows:
  - **PDF**: `application/pdf`
  - **TXT**: `text/plain`
  - **HTML**: `text/html`
  - **CSS**: `text/css`
  - **Markdown**: `text/md`
  - **CSV**: `text/csv`
  - **XML**: `text/xml`
  - **RTF**: `text/rtf`
- Supported code formats for `gemini()` file workflows:
  - **JavaScript**: `application/x-javascript`, `text/javascript`
  - **Python**: `application/x-python`, `text/x-python`
- Supported image formats for `gemini()` file workflows:
  - **PNG**: `image/png`
  - **JPEG**: `image/jpeg`
  - **WEBP**: `image/webp`
  - **HEIC**: `image/heic`
  - **HEIF**: `image/heif`
- Supported video formats for `gemini()` file workflows:
  - **MP4**: `video/mp4`
  - **MPEG**: `video/mpeg`
  - **MOV**: `video/mov`
  - **AVI**: `video/avi`
  - **FLV**: `video/x-flv`
  - **MPG**: `video/mpg`
  - **WEBM**: `video/webm`
  - **WMV**: `video/wmv`
  - **3GPP**: `video/3gpp`
- Supported audio formats for `gemini()` file workflows:
  - **WAV**: `audio/wav`
  - **MP3**: `audio/mp3`
  - **AIFF**: `audio/aiff`
  - **AAC**: `audio/aac`
  - **OGG Vorbis**: `audio/ogg`
  - **FLAC**: `audio/flac`
  
# Version 0.2.1

## Major Features:

- Added `get_metadata()` function to retrieve and format metadata from `LLMMessage` objects.
- Enhanced the `print` method for `LLMMessage` to support printing metadata, controlled via the new `tidyllm_print_metadata` option or a new `.meta`-arguement for the print method.


```r
conversation <- llm_message("Write a short poem about software development") |>
  claude()
  
#Get metdata on token usage and model as tibble  
get_metadata(conversation)

#or print it with the message
print(conversation,.meta=TRUE)

#Or allways print it
options(tidyllm_print_metadata=TRUE)
```

## Bug-fixes:

- Fixed a bug in `send_openai_batch()` caused by  a missing `.json`-arguement not being passed for messages without schema

# Version 0.2.0

New CRAN release. Largest changes compared to **0.1.0**:

**Major Features:**

- Batch Request Support: Added support for batch requests with both Anthropic and OpenAI APIs, enabling large-scale request handling.
- Schema Support: Improved structured outputs in JSON mode with advanced `.json_schema` handling in `openai()`, enhancing support for well-defined JSON responses.
- Azure OpenAI Integration: Introduced `azure_openai()` function for accessing the Azure OpenAI service, with full support for rate-limiting and batch operations tailored to Azure’s API structure.
- Embedding Model Support: Added embedding generation functions for the OpenAI, Ollama, and Mistral APIs, supporting message content and media embedding.
- Mistral API Integration: New `mistral()` function provides full support for Mistral models hosted in the EU, including rate-limiting and streaming capabilities.
- PDF Batch Processing: Introduced the `pdf_page_batch()` function, which processes PDFs page by page, allowing users to define page-specific prompts for detailed analysis.
- Support for OpenAI-compatible APIs: Introduced a `.compatible` argument (and flexible url and path) in `openai()` to allow compatibility with third-party OpenAI-compatible APIs.

**Improvements:**

- API Format Refactoring: Complete refactor of `to_api_format()` to reduce code duplication, simplify API format generation, and improve maintainability.
- Improved Error Handling: Enhanced input validation and error messaging for all API-functions functions, making troubleshooting easier.
- Rate-Limiting Enhancements: Updated rate limiting to use `httr2::req_retry()` in addition to the rate-limit tracking functions in tidyllm, using 429 headers to wait for rate limit resets.
- Expanded Testing: Added comprehensive tests for API functions using `httptest2`

**Breaking Changes:**

- Redesigned Reply Functions: `get_reply()` was split into `get_reply()` for text outputs and `get_reply_data()` for structured outputs, improving type stability compared to an earlier function that had different outputs based on a `.json`-arguement.
- Deprecation of `chatgpt()`: The `chatgpt()` function has been deprecated in favor of `openai()` for feature alignment and improved consistency.

**Minor Updates and Bug Fixes:**

- Expanded PDF Support in `llm_message()`: Allows extraction of specific page ranges from PDFs, improving flexibility in document handling.
- New `ollama_download_model()` function to download models from the Ollama API
- All sequential chat API functions now support streaming

# Version 0.1.11 

## Major Features

- Support for both the Anthropic and the OpenAI batch request API added
- New `.compatible`-arguement in `openai()` to allow working with compatible third party APIs

## Improvements

- **Complete refactor of `to_api_format()`**: API format generation now has much less code duplication and is more maintainable.


# Version 0.1.10

## Breaking Changes

- `get_reply()` was split into two type-stable functions: `get_reply()` for text and `get_reply_data()` for structured outputs.

## Improvements

- **Rate limiting updated to use `httr2::req_retry()`**: Rate limiting now uses the right 429 headers where they come. 

# Version 0.1.9 

## Major Features

- **Enhanced Input Validation**: All API functions now have improved input validation, ensuring better alignment with API documentation

- **Improved error handling**  More human-readable error messages for failed requests from the API
  
- **Advanced JSON Mode in `openai()`**: The `openai()` function now supports advanced `.json_schemas`, allowing structured output in JSON mode for more precise responses.

- **Reasoning Models Support**: Support for O1 reasoning models has been added, with better handling of system prompts in the `openai()` function.

- **Streaming callback functions refactored:** Given that the streaming callback format for Open AI, Mistral and Groq is nearly identical the three now rely on the same callback function. 

## Breaking Changes

- **`chatgpt()` Deprecated**: The `chatgpt()` function has been deprecated in favor of `openai()`. Users should migrate to `openai()` to take advantage of the new features and enhancements.

## Improvements

- **Better Error Handling**: The `openai()`, `ollama()`, and `claude()` functions now return more informative error messages when API calls fail, helping with debugging and troubleshooting.

---

# Version 0.1.8

## Major Features

- **Embedding Models Support:** Embedding model support for three APIs:
  - Embedding functions process message histories and combine text from message content and media attachments for embedding models.
  - `ollama_embedding()` to generate embeddings using the Ollama API.
  - `openai_embedding()` to generate embeddings using the OpenAI API.
  - `mistral_embedding()` to generate embeddings using the Mistral API.

## Improvements

- **PDF Page Support in `llm_message()`:** The `llm_message()` function now supports specifying a range of pages in a PDF by passing a list with `filename`, `start_page`, and `end_page`. This allows users to extract and process specific pages of a PDF.

---

# Version 0.1.7

## Major Features

- **PDF Page Batch Processing**: Introduced the `pdf_page_batch()` function, which processes PDF files page by page, extracting text and converting each page into an image, allowing for a general prompt or page-specific prompts. The function generates a list of `LLMMessage` objects that can  be sent to an API and work with the batch-API functions in **tidyllm**.

---

# Version 0.1.6

## Major Features

- **Support for the Mistral API**: New `mistral()` function to use Mistral Models on Le Platforme on servers hosted in the EU, with rate-limiting and streaming support.

---

# Version 0.1.5

## Major Features

- **Message Retrieval Functions**: Added functions to retrieve single messages from conversations:
  - `last_user_message()` pulls the last message the user sent.
  - `get_reply()` gets the assistant reply at a given index of assistant messages.
  - `get_user_message()` gets the user message at a given index of user messages.

## Improvements

- **Easier Troubleshooting in API-function**: All API functions now support the `.dry_run` argument, allowing users to generate an `httr2`-request for easier debugging and inspection.
- **API Function Tests:** Implemented `httptest2`-based tests with mock responses for all API functions, covering both basic functionality and rate-limiting.

---

# Version 0.1.4

## Major Features

- **New Ollama functions**:
  + **Model Download:** Introduced the `ollama_download_model()` function to download models from the Ollama API. It supports a streaming mode that provides live progress bar updates on the download progress.
  
## Improvements

- Refactoring of `llm_message()`

---

# Version 0.1.3

## Major Features

- The `groq()` function now supports images.
- More complete streaming support across API-functions.

## Breaking Changes

- **Groq Models**: System prompts are no longer sent for Groq models, since many models on Groq do not support them and all multimodal models on Groq disallow them.

---

# Version 0.1.2

## Improvements

- **New unit tests for `llm_message()`**.
- Improvements in streaming functions.

---

# Version 0.1.1

## Major Features

- **JSON Mode**: JSON mode is now more widely supported across all API functions, allowing for structured outputs when APIs support them. The `.json` argument is now passed only to API functions, specifying how the API should respond, and it is not needed anymore in `last_reply()`.
  
- **Improved `last_reply()` Behavior**: The behavior of the `last_reply()` function has changed. It now automatically handles JSON replies by parsing them into structured data and falling back to raw text in case of errors. You can still force raw text replies even for JSON output using the `.raw` argument.

## Breaking Changes

- **`last_reply()`**: The `.json` argument is no longer used, and JSON replies are automatically parsed. Use `.raw` to force raw text replies.


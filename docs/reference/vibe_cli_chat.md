# Chat with Mistral models through your own installed Mistral Vibe CLI

[`vibe_cli()`](https://edubruell.github.io/tidyllm/reference/vibe_cli.md)
runs Mistral's `vibe` command line tool installed on your machine and
reads its JSON output back. Vibe uses the API key stored by
`vibe --setup`, or your `MISTRAL_API_KEY`.

## Usage

``` r
vibe_cli_chat(
  .llm,
  .model = NULL,
  .system_prompt_mode = "prepend",
  .cli_tools = FALSE,
  .max_turns = NULL,
  .max_budget_usd = NULL,
  .stateful = FALSE,
  .session_id = NULL,
  .binary = "vibe",
  .verbose = FALSE,
  .timeout = 300,
  .stream = FALSE,
  .dry_run = FALSE
)
```

## Arguments

- .llm:

  An `LLMMessage` object.

- .model:

  Character; a Mistral model id such as "mistral-large-4". Default NULL
  uses the model Vibe is configured with.

- .system_prompt_mode:

  One of "prepend" or "ignore". With "prepend" the message's system
  prompt is put before the conversation in the prompt text. With
  "ignore" it is dropped.

- .cli_tools:

  Logical; FALSE, the default, refuses every tool call. TRUE hands over
  to Vibe's own configuration, which may let it run commands and edit
  files.

- .max_turns:

  Integer; the most assistant turns Vibe may take. Default NULL leaves
  it to Vibe.

- .max_budget_usd:

  Numeric; stop the run once it has cost this many US dollars.

- .stateful:

  Logical; if TRUE, Vibe keeps the conversation on its side. The first
  call sends the conversation and records the session id in the
  metadata; later calls resume that session and send only the newest
  user message instead of replaying the history. Default FALSE sends the
  whole conversation every time.

- .session_id:

  Character; resume this Vibe session explicitly. Normally left NULL,
  because `.stateful = TRUE` picks the id up from the message history on
  its own.

- .binary:

  Character; the command to run, or a full path to it. Defaults to
  "vibe", which is looked for on the PATH and then in the places the
  installers use. If R does not find it, set
  `options(tidyllm_vibe_cli_path = "/path/to/vibe")` in your
  `.Rprofile`, or the `TIDYLLM_VIBE_CLI` environment variable.

- .verbose:

  Logical; if TRUE, prints rate limit information after the response.

- .timeout:

  Integer; seconds to wait for the whole run.

- .stream:

  Logical; accepted so that
  [`send_chat()`](https://edubruell.github.io/tidyllm/reference/send_chat.md)
  works, but the reply always arrives in one piece, because Vibe does
  not stream it.

- .dry_run:

  Logical; if TRUE, returns the command that would be run instead of
  running it.

## Value

A new `LLMMessage` object containing the original messages plus the
reply from Vibe.

## Details

Vibe is a coding agent, not a plain completion endpoint. By default
tidyllm runs it in Vibe's ask-first mode, in which every tool call needs
an approval that a non-interactive run cannot give, so Vibe can neither
read nor change files. If the model still tries a tool, the refusal
costs a turn; `.max_turns` caps how many it may spend. Set
`.cli_tools = TRUE` to use Vibe's own configuration instead.

Vibe has no system prompt option, so a message's system prompt is put in
front of the prompt text. Vibe does not report token counts, cost or the
model in its output, so those metadata fields are `NA`, except the model
when you set `.model`. Replies arrive in one piece; there is no
streaming.

## Examples

``` r
if (FALSE) { # \dontrun{
llm_message("What is R's S7 class system?") |>
  chat(vibe_cli())

llm_message("Explain vapply() in two sentences.") |>
  chat(vibe_cli(.model = "mistral-large-4"))
} # }
```

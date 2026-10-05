# Chat with Claude through your own installed Claude CLI

[`claude_cli()`](https://edubruell.github.io/tidyllm/reference/claude_cli.md)
is the odd one out among tidyllm's providers: it sends nothing over the
network itself. It runs the `claude` command line tool that is already
installed and signed in on your machine, and reads its JSON output back.
There is no API key to set, and usage counts against whatever plan the
CLI is logged in to rather than against an Anthropic API key.

## Usage

``` r
claude_cli_chat(
  .llm,
  .model = NULL,
  .system_prompt_mode = "append",
  .json_schema = NULL,
  .cli_tools = FALSE,
  .stateful = FALSE,
  .session_id = NULL,
  .max_budget_usd = NULL,
  .append_system_prompt = NULL,
  .binary = "claude",
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

  Character; the model the CLI should use, for example
  "claude-sonnet-5-5" or "claude-haiku-4-5-20251001". Default NULL uses
  whatever the CLI is configured to use.

- .system_prompt_mode:

  One of "append" or "ignore". With "append" the message's system prompt
  is added to the CLI's own system prompt, which is the only way the CLI
  accepts one. With "ignore" it is dropped.

- .json_schema:

  A schema to enforce an output structure; a list, or an ellmer type
  object. Passed to the CLI's own `--json-schema` flag.

- .cli_tools:

  Controls the CLI's built-in tools. FALSE, the default, disables all of
  them, so the call behaves like an ordinary completion. A character
  vector allows exactly those tools, for example
  `c("Read", "WebSearch")`. TRUE hands over to the CLI's own
  configuration, which on a default install includes Bash, Write and
  Edit.

- .stateful:

  Logical; if TRUE the CLI keeps the conversation on its side. The first
  call sends only the newest user message and records the session id in
  the metadata; later calls resume that session instead of replaying the
  history. Default FALSE sends the whole conversation every time, the
  way every other tidyllm provider does.

- .session_id:

  Character; resume this CLI session explicitly. Normally left NULL,
  because `.stateful = TRUE` picks the id up from the message history on
  its own.

- .max_budget_usd:

  Numeric; hand the CLI a spending ceiling for this call.

- .append_system_prompt:

  Character; extra system prompt text, added after the message's own
  system prompt.

- .binary:

  Character; the command to run, or a full path to it. Defaults to
  "claude", which is looked for on the PATH and then in the places the
  installers use. A GUI R session often has a shorter PATH than your
  terminal, so if the CLI is somewhere unusual, set it once with
  `options(tidyllm_claude_cli_path = "/path/to/claude")` in your
  `.Rprofile`, or the `TIDYLLM_CLAUDE_CLI` environment variable, rather
  than passing it at every call.

- .verbose:

  Logical; if TRUE, prints rate limit information after the response.

- .timeout:

  Integer; seconds to wait. On a stream this is the idle deadline
  between events rather than a total, so a long answer is not cut off
  for being long.

- .stream:

  Logical; if TRUE, prints the reply as it arrives.

- .dry_run:

  Logical; if TRUE, returns the command that would be run instead of
  running it.

## Value

A new `LLMMessage` object containing the original messages plus the
CLI's response.

## Details

[`claude_cli()`](https://edubruell.github.io/tidyllm/reference/claude_cli.md)
finds the CLI on the PATH, and, failing that, in the directories the
installers write to. RStudio and other GUI front ends do not inherit the
PATH from your shell profile, so the CLI can be perfectly installed and
still invisible to
[`Sys.which()`](https://rdrr.io/r/base/Sys.which.html); the fallback is
what covers that. To point at it explicitly, set
`options(tidyllm_claude_cli_path = "/path/to/claude")`.

The CLI is an agent, not a plain completion endpoint. By default it can
read files, edit them and run shell commands. tidyllm turns all of that
off unless you ask for it, because a call to
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md) that
quietly edits files in the working directory is not what the rest of
this package does. Pass `.cli_tools` to allow specific tools back.

## Examples

``` r
if (FALSE) { # \dontrun{
llm_message("What is R's S7 class system?") |>
  chat(claude_cli())

# Let the CLI read files, but not write or run anything
llm_message("Summarise the DESCRIPTION file in this folder.") |>
  chat(claude_cli(.cli_tools = c("Read", "Glob")))

# Keep the conversation on the CLI's side
first  <- llm_message("Start a review of my package.") |>
  chat(claude_cli(.stateful = TRUE))
second <- first |>
  llm_message("Now look at the tests.") |>
  chat(claude_cli(.stateful = TRUE))
} # }
```

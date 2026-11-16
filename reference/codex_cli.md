# Chat through a locally installed Codex CLI

`codex_cli()` routes a chat to the `codex` command line tool installed
on your own machine. It uses the login Codex already has, or your OpenAI
API key with `.use_api_key = TRUE`. See
[`codex_cli_chat()`](https://edubruell.github.io/tidyllm/reference/codex_cli_chat.md)
for the arguments and for what happens to Codex's own shell and browser
tools.

## Usage

``` r
codex_cli(..., .called_from = NULL)
```

## Arguments

- ...:

  Arguments passed to the Codex CLI chat function.

- .called_from:

  Internal; the verb that dispatched here.

## Value

The result of the requested action; for
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md), an
updated `LLMMessage`.

## Examples

``` r
if (FALSE) { # \dontrun{
llm_message("Explain R's S7 classes in three sentences.") |>
  chat(codex_cli())
} # }
```

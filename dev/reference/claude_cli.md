# Chat through a locally installed Claude CLI

`claude_cli()` routes a chat to the `claude` command line tool installed
on your own machine, using the login it already has. It takes no API
key. See
[`claude_cli_chat()`](https://edubruell.github.io/tidyllm/dev/reference/claude_cli_chat.md)
for the arguments and for what happens to the CLI's own file and shell
tools.

## Usage

``` r
claude_cli(..., .called_from = NULL)
```

## Arguments

- ...:

  Arguments passed to the CLI chat function.

- .called_from:

  Internal; the verb that dispatched here.

## Value

The result of the requested action; for
[`chat()`](https://edubruell.github.io/tidyllm/dev/reference/chat.md),
an updated `LLMMessage`.

## Examples

``` r
if (FALSE) { # \dontrun{
llm_message("Explain R's S7 classes in three sentences.") |>
  chat(claude_cli())
} # }
```

# Chat through a locally installed Mistral Vibe CLI

`vibe_cli()` routes a chat to Mistral's `vibe` command line tool
installed on your own machine. See
[`vibe_cli_chat()`](https://edubruell.github.io/tidyllm/reference/vibe_cli_chat.md)
for the arguments and for what happens to Vibe's own tools.

## Usage

``` r
vibe_cli(..., .called_from = NULL)
```

## Arguments

- ...:

  Arguments passed to the Vibe CLI chat function.

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
  chat(vibe_cli())
} # }
```

# Alias for the Ellmer Provider Function

The
[`chat_ellmer()`](https://edubruell.github.io/tidyllm/dev/reference/chat_ellmer.md)
function acts as a provider interface for interacting with ellmer chat
objects through `tidyllm`'s verb interface

## Usage

``` r
ellmer(..., .called_from = NULL)
```

## Arguments

- ...:

  Additional parameters to pass through (for consistency with other
  providers).

- .called_from:

  An internal argument specifying which action (e.g., `chat`) the
  function is invoked from.

- .ellmer_chat:

  An ellmer chat object to use as the backend.

## Value

A provider function that can be used with
[`tidyllm::chat()`](https://edubruell.github.io/tidyllm/dev/reference/chat.md).

## Details

This function allows you to use any ellmer chat object (e.g., from
[`ellmer::chat_anthropic()`](https://ellmer.tidyverse.org/reference/chat_anthropic.html),
[`ellmer::chat_openai()`](https://ellmer.tidyverse.org/reference/chat_openai.html),
etc.) as a stateless backend for tidyllm. The ellmer object is cloned
for each interaction to maintain tidyllm's stateless approach.

# Retrieve Log Probabilities from Assistant Replies

Extracts token log probabilities from assistant replies within an
`LLMMessage` object. Each row represents a token with its log
probability and top alternative tokens.

## Usage

``` r
get_logprobs(.llm, .index = NULL)
```

## Arguments

- .llm:

  An `LLMMessage` object containing the message history.

- .index:

  A positive integer specifying which assistant reply's log
  probabilities to extract. If `NULL` (default), log probabilities for
  all replies are returned.

## Value

A tibble containing log probabilities for the specified assistant reply
or all replies.

## Details

An empty tibble is output if no logprobs were requested. Works with
[`openai_chat()`](https://edubruell.github.io/tidyllm/dev/reference/openai_chat.md),
[`llamacpp_chat()`](https://edubruell.github.io/tidyllm/dev/reference/llamacpp_chat.md),
and other providers that support logprobs.

Columns include:

- `reply_index`: The index of the assistant reply in the message
  history.

- `token`: The generated token.

- `logprob`: The log probability of the generated token.

- `bytes`: The byte-level encoding of the token.

- `top_logprobs`: A list column containing the top alternative tokens
  with their log probabilities.

## See also

[`get_metadata()`](https://edubruell.github.io/tidyllm/dev/reference/get_metadata.md)

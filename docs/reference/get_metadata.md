# Retrieve Metadata from Assistant Replies

Retrieves metadata from assistant replies within an `LLMMessage` object.
It returns the metadata as a tibble.

## Usage

``` r
get_metadata(.llm, .index = NULL)

last_metadata(.llm)
```

## Arguments

- .llm:

  An `LLMMessage` object containing the message history, or a list of
  them.

- .index:

  A positive integer specifying which assistant reply's metadata to
  extract. If `NULL` (default), metadata for all replies is returned.

## Value

A tibble containing metadata for the specified assistant reply or all
replies.

## Details

Metadata columns may include:

- `model`: The model used for generating the reply.

- `timestamp`: The time when the reply was generated.

- `prompt_tokens`: The number of tokens in the input prompt.

- `completion_tokens`: The number of tokens in the assistant's reply.

- `total_tokens`: The total number of tokens (prompt + completion).

- `cached_tokens`: Prompt tokens served from the provider's prompt
  cache. `NA_integer_` for providers that do not report cache usage; `0`
  means a reported cache miss.

- `cache_creation_tokens`: Prompt tokens written to the cache. Currently
  reported by
  [`claude()`](https://edubruell.github.io/tidyllm/reference/claude.md)
  only; `NA_integer_` elsewhere.

- `api_specific`: A list column with API-specific metadata, including
  the raw provider-specific cache fields.

For convenience, `last_metadata()` is provided to retrieve the metadata
for the last message.

Given a list of replies, as returned by
[`parallel_chat()`](https://edubruell.github.io/tidyllm/reference/parallel_chat.md)
or
[`fetch_batch()`](https://edubruell.github.io/tidyllm/reference/fetch_batch.md),
the tibble starts with `reply_id`, `reply` and `error` as described in
[`get_reply_data()`](https://edubruell.github.io/tidyllm/reference/get_reply_data.md),
one row per reply and turn.

## See also

`last_metadata()`

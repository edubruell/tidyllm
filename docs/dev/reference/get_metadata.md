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

  An `LLMMessage` object containing the message history.

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

- `api_specific`: A list column with API-specific metadata.

For convenience, `last_metadata()` is provided to retrieve the metadata
for the last message.

## See also

`last_metadata()`

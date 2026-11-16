# Retrieve a User Message by Index

Extracts the content of a user's message from an `LLMMessage` object at
a specific index.

## Usage

``` r
get_user_message(.llm, .index = NULL)

last_user_message(.llm)
```

## Arguments

- .llm:

  An `LLMMessage` object.

- .index:

  A positive integer indicating which user message to retrieve. Defaults
  to `NULL`, which retrieves the last message.

## Value

Returns the content of the user's message at the specified index. If no
messages are found, returns `NA_character_`.

## Details

For convenience, `last_user_message()` is provided as a wrapper to
retrieve the latest user message without specifying an index.

## See also

`last_user_message()`

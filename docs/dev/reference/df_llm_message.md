# Convert a Data Frame to an LLMMessage Object

This function converts a data frame into an `LLMMessage` object
representing a conversation history. The data frame must have specific
columns (`role` and `content`), with each row representing a message.

## Usage

``` r
df_llm_message(.df)
```

## Arguments

- .df:

  A data frame with at least two rows and columns `role` and `content`.
  The `role` column should contain "user", "assistant", or "system". The
  `content` column should contain the corresponding message text.

## Value

An `LLMMessage` object representing the structured conversation.

## See also

[`llm_message()`](https://edubruell.github.io/tidyllm/dev/reference/llm_message.md)

Other Message Creation Utilities:
[`llm_message()`](https://edubruell.github.io/tidyllm/dev/reference/llm_message.md)

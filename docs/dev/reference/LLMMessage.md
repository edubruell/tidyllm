# Large Language Model Message Class

`LLMMessage` is an S7 class for managing a conversation history intended
for use with large language models (LLMs). Please use
[`llm_message()`](https://edubruell.github.io/tidyllm/dev/reference/llm_message.md)to
create or modify `LLMMessage` objects.

## Usage

``` r
LLMMessage(message_history = list(), system_prompt = character(0))
```

## Arguments

- message_history:

  A list containing messages. Each message is a named list with keys
  like `role`, `content`, `media`, etc.

- system_prompt:

  A character string representing the default system prompt used for the
  conversation.

## Details

The `LLMMessage` class includes the following features:

- Stores message history in a structured format.

- Supports attaching media and metadata to messages.

- Provides generics like `add_message()`, `has_image()`, and
  `remove_message()` for interaction.

- Enables API-specific formatting through the `to_api_format()` generic.

- `message_history`: A list containing messages. Each message is a named
  list with keys like `role`, `content`, `media`, etc.

- `system_prompt`: A character string representing the default system
  prompt used for the conversation.

# Create or Update Large Language Model Message Object

This function creates a new `LLMMessage` object or updates an existing
one. It supports adding text prompts and various media types, such as
images, PDFs, text files, or plots.

## Usage

``` r
llm_message(
  .llm = NULL,
  .prompt = NULL,
  .role = "user",
  .system_prompt = "You are a helpful assistant",
  .imagefile = NULL,
  .pdf = NULL,
  .textfile = NULL,
  .capture_plot = FALSE,
  .f = NULL
)
```

## Arguments

- .llm:

  An existing LLMMessage object or an initial text prompt.

- .prompt:

  Text prompt to add to the message history.

- .role:

  The role of the message sender, typically "user" or "assistant".

- .system_prompt:

  Default system prompt if a new LLMMessage needs to be created.

- .imagefile:

  Path to an image file to be attached (optional).

- .pdf:

  Path to a PDF file to be attached (optional). Can be a character
  vector of length one (file path), or a list with `filename`,
  `start_page`, and `end_page`.

- .textfile:

  Path to a text file to be read and attached (optional).

- .capture_plot:

  Boolean to indicate whether a plot should be captured and attached as
  an image (optional).

- .f:

  An R function or an object coercible to a function via
  [`rlang::as_function`](https://rlang.r-lib.org/reference/as_function.html),
  whose output should be captured and attached (optional).

## Value

Returns an updated or new LLMMessage object.

## See also

[`df_llm_message()`](https://edubruell.github.io/tidyllm/reference/df_llm_message.md)

Other Message Creation Utilities:
[`df_llm_message()`](https://edubruell.github.io/tidyllm/reference/df_llm_message.md)

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
  .media = NULL,
  .files = NULL,
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

- .media:

  An inline media object or a list of them. Accepted types:
  [`img()`](https://edubruell.github.io/tidyllm/reference/img.md),
  [`audio_file()`](https://edubruell.github.io/tidyllm/reference/audio_file.md),
  [`video_file()`](https://edubruell.github.io/tidyllm/reference/video_file.md),
  [`pdf_file()`](https://edubruell.github.io/tidyllm/reference/pdf_file.md).

- .files:

  A `tidyllm_file` object or list of them returned by
  [`upload_file()`](https://edubruell.github.io/tidyllm/reference/upload_file.md).
  These are remote file references stored on a provider's server.

- .imagefile:

  Path to an image file to be attached (optional). Deprecated; use
  `.media = img(path)` instead.

- .pdf:

  Path to a PDF file to be attached (optional). Deprecated; use
  `.media = pdf_file(path)` instead.

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

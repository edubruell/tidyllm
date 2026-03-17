# Batch Process PDF into LLM Messages

This function processes a PDF file page by page. For each page, it
extracts the text and converts the page into an image. It creates a list
of LLMMessage objects with the text and the image for multimodal
processing. Users can specify a range of pages to process and provide a
custom function to generate prompts for each page.

## Usage

``` r
pdf_page_batch(
  .pdf,
  .general_prompt,
  .system_prompt = "You are a helpful assistant",
  .page_range = NULL,
  .prompt_fn = NULL
)
```

## Arguments

- .pdf:

  Path to the PDF file.

- .general_prompt:

  A default prompt that is applied to each page if `.prompt_fn` is not
  provided.

- .system_prompt:

  Optional system prompt to initialize the LLMMessage (default is "You
  are a helpful assistant").

- .page_range:

  A vector of two integers specifying the start and end pages to
  process. If NULL, all pages are processed.

- .prompt_fn:

  An optional custom function that generates a prompt for each page. The
  function takes the page text as input and returns a string. If NULL,
  `.general_prompt` is used for all pages.

## Value

A list of LLMMessage objects, each containing the text and image for a
page.

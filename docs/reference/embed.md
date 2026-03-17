# Generate text embeddings

The `embed()` function allows you to embed a text via a specified
provider. It routes the input to the appropriate provider-specific
embedding function.

## Usage

``` r
embed(
  .input,
  .provider = getOption("tidyllm_embed_default"),
  .model = NULL,
  .truncate = NULL,
  .timeout = NULL,
  .dry_run = NULL,
  .max_tries = NULL
)
```

## Arguments

- .input:

  A character vector of texts v, a list of texts and image objects, or
  an `LLMMessage` object

- .provider:

  A function or function call specifying the language model provider and
  any additional parameters. This should be a call to a provider
  function like
  [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md),
  [`ollama()`](https://edubruell.github.io/tidyllm/reference/ollama.md),
  etc. You can also set a default provider function via the
  `tidyllm_embed_default` option.

- .model:

  The embedding model to use

- .truncate:

  Whether to truncate inputs to fit the model's context length

- .timeout:

  Timeout for the API request in seconds

- .dry_run:

  If TRUE, perform a dry run and return the request object.

- .max_tries:

  Maximum retry attempts for requests

## Value

A tibble with two columns: `input` and `embeddings`. The `input` column
contains the texts sent to embed, and the `embeddings` column is a list
column where each row contains an embedding vector of the sent input.

## Examples

``` r
if (FALSE) { # \dontrun{
c("What is the meaning of life, the universe and everything?",
 "How much wood would a woodchuck chuck?",
 "How does the brain work?") |>
 embed(gemini)
 } # }
```

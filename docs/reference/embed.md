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
  .max_tries = NULL,
  .task = NULL,
  .dimensions = NULL,
  .media_columns = FALSE
)
```

## Arguments

- .input:

  A character vector of texts, a list mixing texts with
  [`img()`](https://edubruell.github.io/tidyllm/reference/img.md),
  [`audio_file()`](https://edubruell.github.io/tidyllm/reference/audio_file.md)
  and
  [`video_file()`](https://edubruell.github.io/tidyllm/reference/video_file.md)
  objects, or an `LLMMessage` object. Each element of a list is embedded
  as its own row. A nested list, such as
  `list(list("A cat", img("cat.jpg")))`, is one fused item that gets a
  single vector. `provider_capabilities(.what = "embed_media")` shows
  which provider embeds which media type.

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

- .task:

  `"query"` or `"document"`. Adds the task prefix that EmbeddingGemma 2
  and `gemini-embedding-2` read from the text (and sets `input_type` for
  [`voyage()`](https://edubruell.github.io/tidyllm/reference/voyage.md)).
  Media get no prefix. Leave `NULL` for models that take no prefix.

- .dimensions:

  Number of dimensions to return, for providers and models that can
  shorten their vectors:
  [`gemini()`](https://edubruell.github.io/tidyllm/reference/gemini.md),
  [`openrouter()`](https://edubruell.github.io/tidyllm/reference/openrouter.md),
  [`ollama()`](https://edubruell.github.io/tidyllm/reference/ollama.md),
  [`openai()`](https://edubruell.github.io/tidyllm/reference/openai.md)
  (`text-embedding-3` models) and
  [`mistral()`](https://edubruell.github.io/tidyllm/reference/mistral.md)
  (`codestral-embed`).
  [`voyage()`](https://edubruell.github.io/tidyllm/reference/voyage.md)
  calls this argument `.output_dimension`.
  [`llamacpp()`](https://edubruell.github.io/tidyllm/reference/llamacpp.md)
  has no such argument, because llama-server ignores it.

- .media_columns:

  If `TRUE`, add a `modality` and a `path` column after `input`.
  `modality` is `"text"`, `"image"`, `"audio"` or `"video"`; a fused
  item lists its parts joined by `" + "`, such as `"text + image"`.
  `path` holds the file path of the media in the item (joined by `" + "`
  for several files) and is `NA` for text. Has no effect with
  `.dry_run = TRUE`, which returns the request. Defaults to `FALSE`.

## Value

A tibble with the columns `input` and `embeddings`, plus `modality` and
`path` when `.media_columns = TRUE`. The `input` column contains the
texts sent to embed, or a label such as `[IMG] cat.jpg` for a media
file. A fused item shows its parts joined by `" + "`, for example
`A cat + [IMG] cat.jpg`. The `embeddings` column is a list column where
each row contains an embedding vector of the sent input.

## Examples

``` r
if (FALSE) { # \dontrun{
c("What is the meaning of life, the universe and everything?",
 "How much wood would a woodchuck chuck?",
 "How does the brain work?") |>
 embed(gemini)
 } # }
```

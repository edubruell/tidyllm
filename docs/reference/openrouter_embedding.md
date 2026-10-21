# Generate Embeddings Using the OpenRouter API

Sends text to an embedding model accessible via OpenRouter and returns
embedding vectors. Note that embedding models are not listed in
`list_models(openrouter())` — specify the model ID directly. Known
supported models include `"openai/text-embedding-3-small"`,
`"openai/text-embedding-3-large"`, and `"mistralai/mistral-embed"`.

Images, audio and video work with multimodal embedding models such as
`"google/gemini-embedding-2"`; `"voyageai/voyage-multimodal-3.5"` and
`"nvidia/llama-nemotron-embed-vl-1b-v2:free"` take text and images only.
`https://openrouter.ai/api/v1/embeddings/models` lists the input types
per model.

## Usage

``` r
openrouter_embedding(
  .input,
  .model = "openai/text-embedding-3-small",
  .timeout = 120,
  .dry_run = FALSE,
  .max_tries = 3,
  .task = NULL,
  .dimensions = NULL
)
```

## Arguments

- .input:

  An `LLMMessage` object, a character vector of texts, or a list mixing
  texts with
  [`img()`](https://edubruell.github.io/tidyllm/reference/img.md),
  [`audio_file()`](https://edubruell.github.io/tidyllm/reference/audio_file.md)
  and
  [`video_file()`](https://edubruell.github.io/tidyllm/reference/video_file.md)
  objects (a nested list is one fused item).

- .model:

  The embedding model ID (default: `"openai/text-embedding-3-small"`).

- .timeout:

  Request timeout in seconds (default: 120).

- .dry_run:

  If TRUE, returns the request object without executing it (default:
  FALSE).

- .max_tries:

  Maximum retries (default: 3).

- .task:

  `NULL` (default), `"query"` or `"document"`. Adds the task prefix that
  `gemini-embedding-2` reads from the text; media get none. Leave `NULL`
  for other models.

- .dimensions:

  Number of dimensions to return, for models that can shorten their
  vectors (default: the model's own size).

## Value

A tibble with columns `input` (text, or a label such as `[IMG] cat.jpg`)
and `embeddings` (list of numeric vectors).

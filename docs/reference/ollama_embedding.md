# Generate Embeddings Using Ollama API

Images (PNG, JPEG, GIF, WebP) and WAV or OGG audio need a multimodal
embedding model such as `"embeddinggemma-2:740m"` and Ollama 0.40.4 or
later. Ollama does not embed video or MP3.

## Usage

``` r
ollama_embedding(
  .input,
  .model = "qwen3-embedding:0.6b",
  .truncate = TRUE,
  .ollama_server = "http://localhost:11434",
  .timeout = 120,
  .dry_run = FALSE,
  .task = NULL,
  .dimensions = NULL
)
```

## Arguments

- .input:

  A character vector of texts to embed, a list mixing texts with
  [`img()`](https://edubruell.github.io/tidyllm/reference/img.md) and
  [`audio_file()`](https://edubruell.github.io/tidyllm/reference/audio_file.md)
  objects, or an `LLMMessage` object. A nested list is one fused item
  with at most one image and one audio file.

- .model:

  The embedding model identifier (default: "all-minilm").

- .truncate:

  Whether to truncate inputs to fit the model's context length (default:
  TRUE).

- .ollama_server:

  The URL of the Ollama server to be used (default:
  "http://localhost:11434").

- .timeout:

  Timeout for the API request in seconds (default: 120).

- .dry_run:

  If TRUE, perform a dry run and return the request object.

- .task:

  `NULL` (default), `"query"` or `"document"`. Adds the EmbeddingGemma 2
  task prefix to text; media get none.

- .dimensions:

  Number of dimensions to return, for models that can shorten their
  vectors (default: the model's own size).

## Value

A tibble with columns `input` (text, or a label such as `[IMG] cat.jpg`)
and `embeddings` (list of numeric vectors).

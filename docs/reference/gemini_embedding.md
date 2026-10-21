# Generate Embeddings Using the Google Gemini API

`gemini-embedding-2` embeds text, images (JPEG or PNG, at most 6 per
item), audio (WAV or MP3, up to 180 seconds) and video (MP4, up to 120
seconds) into one space. Media files are sent inline and must not exceed
20 MB. The model reads task instructions from the text, so `.task` adds
a text prefix and no `taskType` is sent.

## Usage

``` r
gemini_embedding(
  .input,
  .model = "gemini-embedding-2",
  .truncate = TRUE,
  .timeout = 120,
  .dry_run = FALSE,
  .max_tries = 3,
  .task = NULL,
  .dimensions = NULL
)
```

## Arguments

- .input:

  A character vector of texts to embed, a list mixing texts with
  [`img()`](https://edubruell.github.io/tidyllm/reference/img.md),
  [`audio_file()`](https://edubruell.github.io/tidyllm/reference/audio_file.md)
  and
  [`video_file()`](https://edubruell.github.io/tidyllm/reference/video_file.md)
  objects, or an `LLMMessage` object. Each list element is one row; a
  nested list is one fused item with a single vector.

- .model:

  The embedding model identifier (default: "gemini-embedding-2").

- .truncate:

  Whether to truncate inputs to fit the model's context length (default:
  TRUE).

- .timeout:

  Timeout for the API request in seconds (default: 120).

- .dry_run:

  If TRUE, perform a dry run and return the request object.

- .max_tries:

  Maximum retry attempts for requests (default: 3).

- .task:

  `NULL` (default), `"query"` or `"document"`. Adds the task prefix to
  text; media get none.

- .dimensions:

  Number of dimensions to return (`gemini-embedding-2` accepts 128 to
  3072 and renormalises the shortened vector). Default: the model's
  3072.

## Value

A tibble with columns `input` (text, or a label such as `[IMG] cat.jpg`)
and `embeddings` (list of numeric vectors).

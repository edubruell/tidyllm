# Generate Embeddings Using Voyage AI API

This function creates embedding vectors from text or multimodal inputs
(text and images) using the Voyage AI API. It supports three types of
input:

## Usage

``` r
voyage_embedding(
  .input,
  .model = "voyage-4",
  .output_dimension = NULL,
  .timeout = 120,
  .dry_run = FALSE,
  .max_tries = 3,
  .verbose = FALSE,
  .task = NULL
)
```

## Arguments

- .input:

  Input to embed. Can be:

  - A character vector of texts

  - An `LLMMessage` object (all textual components will be embedded)

  - A list containing a mix of character strings and
    [`img()`](https://edubruell.github.io/tidyllm/reference/img.md)
    objects

- .model:

  The embedding model identifier. For text-only: "voyage-3.5-lite"
  (default). For multimodal inputs: "voyage-multimodal-3.5" is used
  automatically.

- .output_dimension:

  Optional integer to control output vector size (default: NULL, uses
  model default).

- .timeout:

  Timeout for the API request in seconds (default: 120).

- .dry_run:

  If TRUE, perform a dry run and return the request object without
  sending.

- .max_tries:

  Maximum retry attempts for requests (default: 3).

- .verbose:

  Should information about current rate limits be printed? (default:
  FALSE).

- .task:

  `NULL` (default), `"query"` or `"document"`; sent as Voyage's
  `input_type`.

## Value

A tibble with two columns: `input` and `embeddings`.

- The `input` column contains the input texts or media labels

- The `embeddings` column is a list column where each row contains an
  embedding vector

## Details

1.  Character vector: Embeds each text string separately

2.  LLMMessage object: Extracts and embeds text content from messages

3.  List of mixed content: Processes text strings and image objects
    created with
    [`img()`](https://edubruell.github.io/tidyllm/reference/img.md).
    Each element is one row; a nested list is one fused item.

For multimodal inputs, the function automatically switches to Voyage's
multimodal API and formats the response with appropriate labels (e.g.,
`"[IMG] image.png"`). Voyage does not embed audio, and tidyllm has not
tested video. Image files must not exceed 20 MB.

## Examples

``` r
if (FALSE) { # \dontrun{
# Text embeddings
voyage_embedding("How does photosynthesis work?")

# Multimodal embeddings
list("A banana", img("banana.jpg"), "Yellow fruit") |>
  voyage_embedding()
} # }
```

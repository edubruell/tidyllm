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
  .verbose = FALSE
)
```

## Arguments

- .input:

  Input to embed. Can be:

  - A character vector of texts

  - An `LLMMessage` object (all textual components will be embedded)

  - A list containing a mix of character strings and `tidyllm_image`
    objects created with
    [`img()`](https://edubruell.github.io/tidyllm/dev/reference/img.md)

- .model:

  The embedding model identifier. For text-only: "voyage-3.5-lite"
  (default). For multimodal inputs: "voyage-multimodal-3" is used
  automatically.

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

## Value

A tibble with two columns: `input` and `embeddings`.

- The `input` column contains the input texts or image labels

- The `embeddings` column is a list column where each row contains an
  embedding vector

## Details

1.  Character vector: Embeds each text string separately

2.  LLMMessage object: Extracts and embeds text content from messages

3.  List of mixed content: Processes a combination of text strings and
    image objects created with
    [`img()`](https://edubruell.github.io/tidyllm/dev/reference/img.md)

For multimodal inputs, the function automatically switches to Voyage's
multimodal API and formats the response with appropriate labels (e.g.,
`"[IMG] image.png"`) for images.

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

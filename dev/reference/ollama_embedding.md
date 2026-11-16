# Generate Embeddings Using Ollama API

Generate Embeddings Using Ollama API

## Usage

``` r
ollama_embedding(
  .input,
  .model = "qwen3-embedding:0.6b",
  .truncate = TRUE,
  .ollama_server = "http://localhost:11434",
  .timeout = 120,
  .dry_run = FALSE
)
```

## Arguments

- .input:

  Aa charachter vector of texts to embed or an `LLMMessage` object

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

## Value

A matrix where each column corresponds to the embedding of a message in
the message history.

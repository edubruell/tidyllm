# Delete a model from the Ollama API

This function sends a DELETE request to the Ollama API to remove a
specified model.

## Usage

``` r
ollama_delete_model(.model, .ollama_server = "http://localhost:11434")
```

## Arguments

- .model:

  The name of the model to delete.

- .ollama_server:

  The base URL of the Ollama API (default is "http://localhost:11434").

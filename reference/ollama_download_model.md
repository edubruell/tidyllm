# Download a model from the Ollama API

This function sends a request to the Ollama API to download a specified
model from Ollama's large online library of models.

## Usage

``` r
ollama_download_model(.model, .ollama_server = "http://localhost:11434")
```

## Arguments

- .model:

  The name of the model to download.

- .ollama_server:

  The base URL of the Ollama API (default is "http://localhost:11434").

# Retrieve and return model information from the Ollama API

This function connects to the Ollama API and retrieves information about
available models, returning it as a tibble.

## Usage

``` r
ollama_list_models(.ollama_server = "http://localhost:11434")
```

## Arguments

- .ollama_server:

  The URL of the ollama server to be used

## Value

A tibble containing model information, or NULL if no models are found.

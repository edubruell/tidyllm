% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ollama.R
\name{ollama_embedding}
\alias{ollama_embedding}
\title{Generate Embeddings Using Ollama API}
\usage{
ollama_embedding(
  .llm,
  .model = "all-minilm",
  .truncate = TRUE,
  .ollama_server = "http://localhost:11434",
  .timeout = 120,
  .dry_run = FALSE
)
}
\arguments{
\item{.llm}{An existing LLMMessage object (or a charachter vector of texts to embed)}

\item{.model}{The embedding model identifier (default: "all-minilm").}

\item{.truncate}{Whether to truncate inputs to fit the model's context length (default: TRUE).}

\item{.ollama_server}{The URL of the Ollama server to be used (default: "http://localhost:11434").}

\item{.timeout}{Timeout for the API request in seconds (default: 120).}

\item{.dry_run}{If TRUE, perform a dry run and return the request object.}
}
\value{
A matrix where each column corresponds to the embedding of a message in the message history.
}
\description{
Generate Embeddings Using Ollama API
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/openai.R
\name{openai_embedding}
\alias{openai_embedding}
\title{Generate Embeddings Using OpenAI API}
\usage{
openai_embedding(
  .llm,
  .model = "text-embedding-3-small",
  .truncate = TRUE,
  .timeout = 120,
  .dry_run = FALSE,
  .min_tokens_reset = 0L,
  .wait = TRUE
)
}
\arguments{
\item{.llm}{An existing LLMMessage object (or a character vector of texts to embed)}

\item{.model}{The embedding model identifier (default: "text-embedding-3-small").}

\item{.truncate}{Whether to truncate inputs to fit the model's context length (default: TRUE).}

\item{.timeout}{Timeout for the API request in seconds (default: 120).}

\item{.dry_run}{If TRUE, perform a dry run and return the request object.}

\item{.min_tokens_reset}{Integer specifying the minimum token threshold before waiting for reset.}

\item{.wait}{Logical; if TRUE, respects rate limits by waiting when necessary (default: TRUE).}
}
\value{
A matrix where each column corresponds to the embedding of a message in the message history.
}
\description{
Generate Embeddings Using OpenAI API
}

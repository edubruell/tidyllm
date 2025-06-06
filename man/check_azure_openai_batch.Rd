% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_azure_openai.R
\name{check_azure_openai_batch}
\alias{check_azure_openai_batch}
\title{Check Batch Processing Status for Azure OpenAI Batch API}
\usage{
check_azure_openai_batch(
  .llms = NULL,
  .endpoint_url = Sys.getenv("AZURE_ENDPOINT_URL"),
  .batch_id = NULL,
  .dry_run = FALSE,
  .max_tries = 3,
  .timeout = 60
)
}
\arguments{
\item{.llms}{A list of LLMMessage objects.}

\item{.endpoint_url}{Base URL for the API (default:  Sys.getenv("AZURE_ENDPOINT_URL")).}

\item{.batch_id}{A manually set batch ID.}

\item{.dry_run}{Logical; if TRUE, returns the prepared request object without executing it (default: FALSE).}

\item{.max_tries}{Maximum retries to perform the request (default: 3).}

\item{.timeout}{Integer specifying the request timeout in seconds (default: 60).}
}
\value{
A tibble with information about the status of batch processing.
}
\description{
This function retrieves the processing status and other details of a specified Azure OpenAI batch ID
from the Azure OpenAI Batch API.
}

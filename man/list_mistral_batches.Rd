% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_mistral.R
\name{list_mistral_batches}
\alias{list_mistral_batches}
\title{List Mistral Batch Requests}
\usage{
list_mistral_batches(
  .limit = 100,
  .max_tries = 3,
  .timeout = 60,
  .status = NULL,
  .created_after = NULL
)
}
\arguments{
\item{.limit}{Maximum number of batches to retrieve (default: 20).}

\item{.max_tries}{Maximum retry attempts for requests (default: 3).}

\item{.timeout}{Request timeout in seconds (default: 60).}

\item{.status}{Filter by status. (default: NULL)}

\item{.created_after}{created after a string specifiying a date-time  (default: NULL)}
}
\value{
A tibble with batch details for all batches fitting the request
}
\description{
Retrieves batch request details from the OpenAI Batch API.
}

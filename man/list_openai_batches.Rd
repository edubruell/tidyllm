% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_openai.R
\name{list_openai_batches}
\alias{list_openai_batches}
\title{List OpenAI Batch Requests}
\usage{
list_openai_batches(.limit = 20, .max_tries = 3, .timeout = 60)
}
\arguments{
\item{.limit}{Maximum number of batches to retrieve (default: 20).}

\item{.max_tries}{Maximum retry attempts for requests (default: 3).}

\item{.timeout}{Request timeout in seconds (default: 60).}
}
\value{
A tibble with batch details: batch ID, status, creation time, expiration time,
and request counts (total, completed, failed).
}
\description{
Retrieves batch request details from the OpenAI Batch API.
}

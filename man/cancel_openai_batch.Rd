% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_openai.R
\name{cancel_openai_batch}
\alias{cancel_openai_batch}
\title{Cancel an In-Progress OpenAI Batch}
\usage{
cancel_openai_batch(.batch_id, .dry_run = FALSE, .max_tries = 3, .timeout = 60)
}
\arguments{
\item{.batch_id}{Character; the unique identifier for the batch to cancel.}

\item{.dry_run}{Logical; if \code{TRUE}, returns the constructed request without executing it (default: \code{FALSE}).}

\item{.max_tries}{Integer; maximum number of retries if the request fails (default: \code{3}).}

\item{.timeout}{Integer; request timeout in seconds (default: \code{60}).}
}
\value{
A list containing the response from the OpenAI API about the cancellation status.
}
\description{
This function cancels an in-progress batch created through the OpenAI API.
The batch will be moved to a "cancelling" state and, eventually, "cancelled."
}

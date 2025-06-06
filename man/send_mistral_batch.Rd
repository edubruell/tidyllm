% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_mistral.R
\name{send_mistral_batch}
\alias{send_mistral_batch}
\title{Send a Batch of Requests to the Mistral API}
\usage{
send_mistral_batch(
  .llms,
  .model = "mistral-small-latest",
  .endpoint = "/v1/chat/completions",
  .metadata = NULL,
  .temperature = 0.7,
  .top_p = 1,
  .max_tokens = 1024,
  .min_tokens = NULL,
  .frequency_penalty = NULL,
  .logit_bias = NULL,
  .presence_penalty = NULL,
  .seed = NULL,
  .stop = NULL,
  .safe_prompt = FALSE,
  .json_schema = NULL,
  .dry_run = FALSE,
  .overwrite = FALSE,
  .max_tries = 3,
  .timeout = 60,
  .id_prefix = "tidyllm_mistral_req_"
)
}
\arguments{
\item{.llms}{A list of LLMMessage objects containing conversation histories.}

\item{.model}{The Mistral model version (default: "mistral-small-latest").}

\item{.endpoint}{The API endpoint (default: "/v1/chat/completions").}

\item{.metadata}{Optional metadata for the batch.}

\item{.temperature}{Sampling temperature to use, between \code{0.0} and \code{1.5} (default: \code{0.7}).}

\item{.top_p}{Nucleus sampling parameter, between \code{0.0} and \code{1.0} (default: \code{1}).}

\item{.max_tokens}{The maximum number of tokens to generate in the completion (default: \code{1024}).}

\item{.min_tokens}{The minimum number of tokens to generate (optional).}

\item{.frequency_penalty}{Numeric value (or NULL) for frequency penalty.}

\item{.logit_bias}{A named list modifying the likelihood of specific tokens (or NULL).}

\item{.presence_penalty}{Numeric value (or NULL) for presence penalty.}

\item{.seed}{Random seed for deterministic outputs (optional).}

\item{.stop}{Sequence(s) at which to stop generation (optional).}

\item{.safe_prompt}{Logical; if TRUE, injects a safety prompt (default: FALSE).}

\item{.json_schema}{A JSON schema object for structured output (optional).}

\item{.dry_run}{Logical; if TRUE, returns the prepared request without executing it (default: FALSE).}

\item{.overwrite}{Logical; if TRUE, allows overwriting existing custom IDs (default: FALSE).}

\item{.max_tries}{Maximum retry attempts for requests (default: 3).}

\item{.timeout}{Request timeout in seconds (default: 60).}

\item{.id_prefix}{Prefix for generating custom IDs (default: "tidyllm_mistral_req_").}
}
\value{
The prepared LLMMessage list with a batch_id attribute.
}
\description{
This function creates and submits a batch of messages to the Mistral API for asynchronous processing.
}


#' Initialize or Retrieve API-specific Environment
#'
#' This function initializes a named environment for storing rate limit information
#' specific to an API. It ensures that each API's rate limit data is stored separately.
#'
#' @param api_name The name of the API for which to initialize or retrieve the environment.
#' @export
initialize_api_env <- function(.api_name) {
  if (!exists(.api_name, envir = .tidyllm_rate_limit_env)) {
    .tidyllm_rate_limit_env[[.api_name]] <- new.env()
  }
  invisible(NULL)
}


#' Update the standard API rate limit info in the hidden .tidyllm_rate_limit_env environment
#'
#' This function initializes stores ratelimit information from API functions for future use
#'
#' @param .api_name The name of the API for which to initialize or retrieve the environment.
#' @param .response_object A preparsed response object cotaining info on remaining requests, tokens and rest times
#' @export
update_rate_limit <- function(.apiname,.response_object){
  .tidyllm_rate_limit_env[[.apiname]]$last_request        <- .response_object$this_request_time
  .tidyllm_rate_limit_env[[.apiname]]$requests_remaining  <- .response_object$ratelimit_requests_remaining
  .tidyllm_rate_limit_env[[.apiname]]$requests_reset_time <- .response_object$ratelimit_requests_reset_time
  .tidyllm_rate_limit_env[[.apiname]]$tokens_remaining    <- .response_object$ratelimit_tokens_remaining
  .tidyllm_rate_limit_env[[.apiname]]$tokens_reset_time   <- .response_object$ratelimit_tokens_reset_time
  invisible(NULL)
}


#' Wait for ratelimit restore times to ellapse if necessary
#'
#' This function implements a standardized wait for rate limit resets
#'
#' @param .api_name The name of the API for which rate limit we want to wait
#' @param .min_tokens_reset A token boundary at which we wish to reset (Typically should be larger than the size of the message)
#' @export
wait_rate_limit <- function(.apiname,.min_tokens_reset){
  
  #Read info from .tidyllm_rate_limit_env
  requests_remaining  <- .tidyllm_rate_limit_env[[.apiname]]$requests_remaining
  requests_reset_time <- .tidyllm_rate_limit_env[[.apiname]]$requests_reset_time
  requests_reset_difftime <- as.numeric(difftime(requests_reset_time, now(tzone = "UTC"), units = "secs"))
  tokens_reset_time    <- .tidyllm_rate_limit_env[[.apiname]]$tokens_reset_time
  tokens_remaining    <- .tidyllm_rate_limit_env[[.apiname]]$tokens_remaining
  tokens_reset_difftime <- as.numeric(difftime(tokens_reset_time, now(tzone = "UTC"), units = "secs"))
  
  #Wait if rate limit is likely to be hit
  if(requests_remaining  == 1){
    glue::glue("Waiting till requests rate limit is reset: {round(requests_reset_difftime},2)}")
    Sys.sleep(requests_reset_difftime)
  }
  if(tokens_reset_difftime > 0 & .min_tokens_reset>0 & tokens_remaining<.min_tokens_reset){
    glue::glue("Waiting till the token rate limit is reset: round({tokens_reset_difftime},2) seconds") |> cat()
    Sys.sleep(tokens_reset_difftime)
  }
}

#' An internal function to parse the duration strings that OpenAI APIs return for ratelimit resets
#'
#'This internal function parses duration strings as returned by the OpenAI API
#'
#' @param .duration_str A duration string. 
#' @return A numeric number of seconds 
# Function to parse various time formats into seconds
parse_duration_to_seconds <- function(.duration_str) {
  if(grepl("ms", .duration_str)) {
    # Format is in milliseconds, e.g., "176ms"
    duration_sec <- as.numeric(sub("ms", "", .duration_str)) / 1000
  } else if(grepl("m", .duration_str) && grepl("s", .duration_str)) {
    # Format is in minutes and seconds, e.g., "20m47.458s"
    minutes <- as.numeric(sub("m.*", "", .duration_str))
    seconds <- as.numeric(sub(".*m", "", sub("s", "", .duration_str)))
    duration_sec <- minutes * 60 + seconds
  } else if(grepl("s", .duration_str)) {
    # Format is just in seconds, e.g., "47s"
    duration_sec <- as.numeric(sub("s", "", .duration_str))
  } else {
    # Default to zero if format is unrecognized
    duration_sec <- 0
  }
  return(duration_sec)
}

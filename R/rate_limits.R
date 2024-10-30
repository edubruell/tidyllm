
#' Initialize or Retrieve API-specific Environment
#'
#' This function initializes a named environment for storing rate limit information
#' specific to an API. It ensures that each API's rate limit data is stored separately.
#'
#' @param .api_name The name of the API for which to initialize or retrieve the environment
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
update_rate_limit <- function(.api_name,.response_object){
  initialize_api_env(.api_name=.api_name)
  .tidyllm_rate_limit_env[[.api_name]]$last_request        <- .response_object$this_request_time
  .tidyllm_rate_limit_env[[.api_name]]$requests_remaining  <- .response_object$ratelimit_requests_remaining
  .tidyllm_rate_limit_env[[.api_name]]$requests_reset_time <- .response_object$ratelimit_requests_reset_time
  .tidyllm_rate_limit_env[[.api_name]]$tokens_remaining    <- .response_object$ratelimit_tokens_remaining
  .tidyllm_rate_limit_env[[.api_name]]$tokens_reset_time   <- .response_object$ratelimit_tokens_reset_time
  invisible(NULL)
}


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


#' Extract rate limit information from API response headers
#'
#' @param .response_headers Headers from the API response
#' @param .api The API type ("claude", "openai","groq")
#' @return A list containing rate limit information
ratelimit_from_header <- function(.response_headers, .api) {
  switch(.api,
         "claude" = {
           list(
             this_request_time = strptime(.response_headers["date"], 
                                          format="%a, %d %b %Y %H:%M:%S", tz="GMT"),
             ratelimit_requests = as.integer(
               .response_headers["anthropic-ratelimit-requests-limit"]),
             ratelimit_requests_remaining = as.integer(
               .response_headers["anthropic-ratelimit-requests-remaining"]),
             ratelimit_requests_reset_time = as.POSIXct(
               .response_headers["anthropic-ratelimit-requests-reset"]$`anthropic-ratelimit-requests-reset`,
               format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
             ratelimit_tokens = as.integer(
               .response_headers["anthropic-ratelimit-tokens-limit"]),
             ratelimit_tokens_remaining = as.integer(
               .response_headers["anthropic-ratelimit-tokens-remaining"]),
             ratelimit_tokens_reset_time = as.POSIXct(
               .response_headers["anthropic-ratelimit-tokens-reset"]$`anthropic-ratelimit-tokens-reset`,
               format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
           )
         },
         "openai" = {
           request_time <- strptime(.response_headers["date"]$date, 
                                    format="%a, %d %b %Y %H:%M:%S", tz="GMT")
           ratelimit_requests_reset_dt <- parse_duration_to_seconds(
             .response_headers["x-ratelimit-reset-requests"]$`x-ratelimit-reset-requests`)
           ratelimit_tokens_reset_dt <- parse_duration_to_seconds(
             .response_headers["x-ratelimit-reset-tokens"]$`x-ratelimit-reset-tokens`)
           
           list(
             this_request_time = request_time,
             ratelimit_requests = as.integer(
               .response_headers["x-ratelimit-limit-requests"]),
             ratelimit_requests_remaining = as.integer(
               .response_headers["x-ratelimit-remaining-requests"]),
             ratelimit_requests_reset_time = request_time + ratelimit_requests_reset_dt,
             ratelimit_tokens = as.integer(
               .response_headers["x-ratelimit-limit-tokens"]),
             ratelimit_tokens_remaining = as.integer(
               .response_headers["x-ratelimit-remaining-tokens"]),
             ratelimit_tokens_reset_time = request_time + ratelimit_tokens_reset_dt
           )
         },
         "groq" ={
           #Do some parsing for the rl list 
           request_time                  <- strptime(.response_headers["date"]$date, format="%a, %d %b %Y %H:%M:%S", tz="GMT")
           ratelimit_requests_reset_dt   <- parse_duration_to_seconds(.response_headers["x-ratelimit-reset-requests"]$`x-ratelimit-reset-requests`)
           ratelimit_requests_reset_time <- request_time + ratelimit_requests_reset_dt
           ratelimit_tokens_reset_dt     <- parse_duration_to_seconds(.response_headers["x-ratelimit-reset-tokens"]$`x-ratelimit-reset-tokens`)
           ratelimit_tokens_reset_time   <- request_time + ratelimit_tokens_reset_dt
           
           #Ratelimit list
           list(
             this_request_time             = request_time ,
             ratelimit_requests            = as.integer(.response_headers["x-ratelimit-limit-requests"]),
             ratelimit_requests_remaining  = as.integer(.response_headers["x-ratelimit-remaining-requests"]),
             ratelimit_requests_reset_time = ratelimit_requests_reset_time ,
             ratelimit_tokens              = as.integer(.response_headers["x-ratelimit-limit-tokens"]),
             ratelimit_tokens_remaining    = as.integer(.response_headers["x-ratelimit-remaining-tokens"]),
             ratelimit_tokens_reset_time   = ratelimit_tokens_reset_time
           )
         },
         "azure_openai" = {
           request_time <- strptime(.response_headers["date"]$date, 
                                    format="%a, %d %b %Y %H:%M:%S", tz="GMT")
           
           # Extract remaining requests and tokens
           ratelimit_requests_remaining <- as.integer(
             .response_headers["x-ratelimit-remaining-requests"]$`x-ratelimit-remaining-requests`)
           ratelimit_tokens_remaining <- as.integer(
             .response_headers["x-ratelimit-remaining-tokens"]$`x-ratelimit-remaining-tokens`)
           
           # Assuming reset occurs every 60 seconds (at least I got minutes in my azure console)
           reset_interval <- 60         
           
           ratelimit_requests_reset_time <- request_time + reset_interval
           ratelimit_tokens_reset_time <- request_time + reset_interval
           
           list(
             this_request_time = request_time,
             ratelimit_requests = NA,
             ratelimit_requests_remaining = ratelimit_requests_remaining,
             ratelimit_requests_reset_time = ratelimit_requests_reset_time,
             ratelimit_tokens = NA,
             ratelimit_tokens_remaining = ratelimit_tokens_remaining,
             ratelimit_tokens_reset_time = ratelimit_tokens_reset_time
           )
         },
         stop(sprintf("Unsupported API type: %s", .api))
  )
}

#' Get the current rate limit information for all or a specific API
#'
#' This function retrieves the rate limit details for the specified API,
#' or for all APIs stored in the .tidyllm_rate_limit_env if no API is specified.
#'
#' @param .api_name (Optional) The name of the API whose rate limit info you want to get
#'                   If not provided, the rate limit info for all APIs in the environment will be returned
#' @return A tibble containing the rate limit information.

#' @export
rate_limit_info <- function(.api_name = NULL) {
  # If no API name is provided, print for all APIs in .tidyllm_rate_limit_env
  api_names <- ls(envir = .tidyllm_rate_limit_env)
  
  if (length(api_names) == 0) {
    message("No rate limit information available in .tidyllm_rate_limit_env.")
    return(tibble::tibble())
  }
  
  if (!is.null(.api_name)) {
    # If a specific API is provided, check if it exists
    if (!exists(.api_name, envir = .tidyllm_rate_limit_env)) {
      stop(glue::glue("No rate limit information found for the API: {.api_name}."))
    }
    api_names <- .api_name  # Restrict to the provided API
  }
  
  # Use lapply to iterate over all relevant APIs and print their rate limits
  rl_tibble <- purrr::map_dfr(api_names, function(api) {
    rl_info <- .tidyllm_rate_limit_env[[api]]
    
    # Check for incomplete rate limit data
    if (is.null(rl_info$last_request) ||
        is.null(rl_info$requests_remaining) ||
        is.null(rl_info$tokens_remaining)) {
      warning(glue::glue("Incomplete rate limit data for API: {api}."))
    }
    
    
    tibble::tibble(api =api, 
                   last_request = rl_info$last_request,
                   requests_remaining  = rl_info$requests_remaining,
                   requests_reset_time = rl_info$requests_reset_time,
                   tokens_remaining    = rl_info$tokens_remaining,
                   tokens_reset_time   = rl_info$tokens_reset_time)
  })
  rl_tibble
}


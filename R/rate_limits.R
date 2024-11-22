
#' Initialize or Retrieve API-specific Environment
#'
#' This function initializes a named environment for storing rate limit information
#' specific to an API. It ensures that each API's rate limit data is stored separately.
#'
#' @param .api_name The name of the API for which to initialize or retrieve the environment
#' @noRd
initialize_api_env <- function(.api_name) {
  if (!exists(.api_name, envir = .tidyllm_rate_limit_env)) {
    .tidyllm_rate_limit_env[[.api_name]] <- new.env()
  }
  invisible(NULL)
}


#' Track the standard API rate limit info in the hidden .tidyllm_rate_limit_env environment
#'
#' @param .api_name The name of the API for which to initialize or retrieve the environment.
#' @param .headers The response headers from an API-answer.
#' @param .verbose Logical indicating whether to print rate limit details.
#' 
#' @noRd
track_rate_limit <- function(.api,.headers,.verbose) {
  api            <- .api@short_name
  api_long_name  <- .api@long_name
  tryCatch({
    rl <- ratelimit_from_header(.api, .headers)
    
    initialize_api_env(api)
    .tidyllm_rate_limit_env[[api]]$last_request        <- rl$this_request_time
    .tidyllm_rate_limit_env[[api]]$requests_remaining  <- rl$ratelimit_requests_remaining
    .tidyllm_rate_limit_env[[api]]$requests_reset_time <- rl$ratelimit_requests_reset_time
    .tidyllm_rate_limit_env[[api]]$tokens_remaining    <- rl$ratelimit_tokens_remaining
    .tidyllm_rate_limit_env[[api]]$tokens_reset_time   <- rl$ratelimit_tokens_reset_time
    
    # Show rate limit info if verbose
    if (.verbose) {
      glue::glue(
        "{api_long_name} answer received at {rl$this_request_time}.
        Remaining requests rate limit: {rl$ratelimit_requests_remaining}/{rl$ratelimit_requests}
        Requests rate limit reset at: {rl$ratelimit_requests_reset_time}
        Remaining tokens rate limit: {rl$ratelimit_tokens_remaining}/{rl$ratelimit_tokens}
        Tokens rate limit reset at: {rl$ratelimit_tokens_reset_time}\n"
      ) |> message()
    }
    
  }, error = function(e) {
    warning(glue::glue("Failed to track rate limit for {api_long_name}: {e$message}"))
  })
  
  invisible(NULL)
}

#'
#'This internal function parses duration strings as returned by the OpenAI API
#'
#' @param .duration_str A duration string. 
#' @return A numeric number of seconds 
#' @noRd
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


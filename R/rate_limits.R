
#' Initialize or Retrieve API-specific Environment
#'
#' This function initializes or retrieves a named environment for storing rate limit information
#' specific to an API. It ensures that each API's rate limit data is stored separately.
#'
#' @param api_name The name of the API for which to initialize or retrieve the environment.
#' @return An environment object for the specified API.
#' @export
initialize_api_env <- function(api_name) {
  if (!exists(api_name, envir = .rate_limit_env)) {
    .tidyllm_rate_limit_env[[api_name]] <- new.env()
  }
  invisible(NULL)
}
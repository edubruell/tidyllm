#' @export
.onLoad <- function(libname, pkgname) {
  # Initialize the parent environment for rate limits on package load
  if (!exists(".tidyllm_rate_limit_env", envir = .GlobalEnv)) {
    .GlobalEnv$.tidyllm_rate_limit_env <- new.env(parent = emptyenv())
  }
  
  message("Package ", pkgname, " loaded. Rate limit environment initialized.")
}

#' @export
.onAttach <- function(libname, pkgname) {
  # Re-initialize or ensure the environment is set up when package is attached
  if (!exists(".tidyllm_rate_limit_env", envir = .GlobalEnv)) {
    .GlobalEnv$.tidyllm_rate_limit_env <- new.env(parent = emptyenv())
  }
  
  message("Package ", pkgname, " attached. Rate limit environment ready.")
}
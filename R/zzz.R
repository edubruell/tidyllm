#' @export
.onLoad <- function(libname, pkgname) {
  # Initialize the parent environment for rate limits on package load
  if (!exists(".tidyllm_rate_limit_env", envir = .GlobalEnv)) {
    .GlobalEnv$.tidyllm_rate_limit_env <- new.env(parent = emptyenv())
  }
  #Initialize the parent environment for streaming backups on package load
  if (!exists(".tidyllm_stream_env", envir = .GlobalEnv)) {
    .GlobalEnv$.tidyllm_stream_env <- new.env(parent = emptyenv())
  }
}

#' @export
.onAttach <- function(libname, pkgname) {
  # Re-initialize or ensure the environment is set up when package is attached
  if (!exists(".tidyllm_rate_limit_env", envir = .GlobalEnv)) {
    .GlobalEnv$.tidyllm_rate_limit_env <- new.env(parent = emptyenv())
  }
  #Initialize the parent environment for streaming backups on package load
  if (!exists(".tidyllm_stream_env", envir = .GlobalEnv)) {
    .GlobalEnv$.tidyllm_stream_env <- new.env(parent = emptyenv())
  }
}
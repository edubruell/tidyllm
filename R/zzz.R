#' @export
.onLoad <- function(libname, pkgname) {
  options(tidyllm_print_metadata = FALSE) # Default is to not print metadata
  S7::methods_register()
  
  # Initialize the parent environment for rate limits on package load
  if (!exists(".tidyllm_rate_limit_env", envir = .GlobalEnv)) {
    .GlobalEnv$.tidyllm_rate_limit_env <- new.env(parent = emptyenv())
  }
}

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL
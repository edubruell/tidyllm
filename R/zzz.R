#' @export
.onLoad <- function(libname, pkgname) {
  options(tidyllm_print_metadata = FALSE) # Default is to not print metadata
  S7::methods_register()
  
  # Initialize the parent environment for rate limits on package load
  if (!exists(".tidyllm_rate_limit_env", envir = .GlobalEnv)) {
    .GlobalEnv$.tidyllm_rate_limit_env <- new.env(parent = emptyenv())
  }

  # `promises::as.promise` is a generic in a suggested package, so the method is
  # registered at load time instead of in NAMESPACE, which would make promises a
  # hard requirement. It is what lets a chat job drive a Shiny `ExtendedTask`.
  register_promise_method()
}

#' Register `as.promise()` for chat jobs whenever promises is available
#'
#' Registers now if `promises` is already loaded, and otherwise arranges to
#' register when it loads, so installing `promises` after tidyllm still works.
#' This is the standard `s3_register()` dance; it is written out because neither
#' `rlang` nor anything else tidyllm imports exports that helper.
#'
#' @noRd
register_promise_method <- function() {
  register <- function(...) {
    registerS3method("as.promise", "tidyllm_chat_job", as_promise_chat_job,
                     envir = asNamespace("promises"))
  }
  if (isNamespaceLoaded("promises")) register()
  setHook(packageEvent("promises", "onLoad"), register)
  invisible(NULL)
}

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL
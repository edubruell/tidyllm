#' Is an input an integer valued numeric (needed to validate inputs)
#'
#' This internal function is a small helper used to validate inputs in functions
#'
#' @param .x An input that is checked to see whether it is an interger valued numeric
#' @return A logical
#' @noRd
is_integer_valued <- function(.x) {
  # Check if the input is numeric
  if (is.numeric(.x)) {
    # Further check if the numeric input is effectively an integer
    if (.x == as.integer(.x)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

#' Validate Input Conditions for R Functions
#'
#' This internal function validates specified conditions for function inputs and stops the function execution if any condition is not met. It uses a named vector of predicates where each name is the error message associated with the predicate condition.
#'
#' @param .predicates A named vector where each element is a logical condition and the name of each element is the corresponding error message to be displayed if the condition is FALSE.
#' @return None; the function will stop execution and throw an error if a validation fails.
#' @examples
#' validate_inputs(c(
#'   "Input must be numeric" = is.numeric(5),
#'   "Input must be integer" = 5 == as.integer(5)
#' ))
#' @noRd
validate_inputs <- function(.predicates) {
  # Use lapply to iterate over predicates and stop on the first failure
  results <- lapply(names(.predicates), function(error_msg) {
    if (!.predicates[[error_msg]]) {
      stop(error_msg)
    }
  })
}

# Helper function to filter mmessages by roles
#'
#' @param .message_hisotry A message history in the format used within LLMMessage
#' @param .roles A vector of roles (default: `c("user", "assistant")`)
#' @return A filtered message_history
#' @noRd
filter_roles = function(.message_history,
                        .roles = c("user", "assistant")) {
  Filter(function(x) "role" %in% names(x) && x$role %in% .roles, .message_history)
}


#' Extract Response Metadata
#'
#' This helper function extracts and formats metadata from LLM API responses.
#' It standardizes the model used, timestamp of the response, and token usage statistics 
#' (prompt, completion, and total tokens) across various LLM API providers.
#'
#' @param .response A list containing the response object from an LLM API call.
#'                  The structure of this list may vary depending on the provider 
#' @return A named list containing the standardized metadata: model, timestamp, prompt tokens, 
#'         completion tokens, and total tokens.
#' @importFrom lubridate as_datetime
#' @importFrom rlang %||%
#' @noRd
extract_response_metadata <- function(.response) {
  timestamp <- .response$created_at %||% .response$created %||% NA
  timestamp <- if (is.numeric(timestamp)) {
    lubridate::as_datetime(timestamp)
  } else if (is.character(timestamp)) {
    lubridate::as_datetime(timestamp, format = "%Y-%m-%dT%H:%M:%OSZ")
  } else {
    NA
  }
  
  prompt_tokens <- .response$usage$prompt_tokens %||% .response$usage$input_tokens %||% NA
  completion_tokens <- .response$usage$completion_tokens %||% .response$usage$output_tokens %||% NA
  
  total_tokens <- .response$usage$total_tokens %||% 
    if (!is.na(prompt_tokens) && !is.na(completion_tokens)) {
      prompt_tokens + completion_tokens
    } else {
      NA
    }
  
  list(
    model = .response$model %||% NA,
    timestamp = timestamp,
    prompt_tokens = prompt_tokens,
    completion_tokens = completion_tokens,
    total_tokens = total_tokens
  )
}

#' The base class for API-provider specific classes
#'
#' @noRd
APIProvider <- new_class("APIProvider",properties = list(
  short_name = class_character,
  long_name  = class_character,
  api_key_env_var = class_character
))

generate_callback_function <- new_generic("generate_callback_function","api")
ratelimit_from_header      <- new_generic("ratelimit_from_header",c("api", "headers"))
parse_chat_function        <- new_generic("parse_chat_response","api")
get_api_key                <- new_generic("get_api_key","api")


#Default method for the streaming callback function
#'
#' @noRd
method(generate_callback_function,APIProvider) <- function(api) {
  # Default testing callback implementation
  function(.data) {
    if (is.null(.tidyllm_stream_env$testing_chunck_list)) {
      .tidyllm_stream_env$testing_chunck_list <- list()
    }
    
    # Append new data to the buffer
    new_data <- rawToChar(.data, multiple = FALSE)
    .tidyllm_stream_env$testing_chunck_list <- append(.tidyllm_stream_env$testing_chunck_list, new_data)
    length(.tidyllm_stream_env$testing_chunck_list) |> cat("\n")
    utils::flush.console()
    TRUE
  }
}

#Default method for the API key checks
#'
#' @noRd
method(get_api_key, APIProvider) <- function(api,dry_run=FALSE) {
  api_key <- Sys.getenv(api@api_key_env_var)
  if (api_key == "" & dry_run==FALSE) {
    paste0("API key is not set. Please set it with: Sys.setenv(",api@api_key_env_var," = \"YOUR-KEY-GOES-HERE\")." ) |>
      rlang::abort()
  }
  return(api_key)
}

#api_fart <- APIProvider(short_name = "Fart",long_name = "fart.ai",api_key_env_var = "FOPENAI_API_KEY" )
#get_api_key(api_fart,TRUE)

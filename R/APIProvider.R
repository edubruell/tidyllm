#' The base class for API-provider specific classes
#'
#' @noRd
APIProvider <- new_class("APIProvider",properties = list(
  short_name = class_character,
  long_name  = class_character
))

generate_callback_function <- new_generic("generate_callback_function","api")
ratelimit_from_header      <- new_generic("ratelimit_from_header",c("api", "headers"))
parse_chat_function        <- new_generic("parse_chat_response","api")
parse_embedding_function   <- new_generic("parse_embedding_function","api")


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






#' Perform a Chat API request to interact with language models
#'
#' @param .request The httr2 request object.
#' @param .api An api provider object
#' @param .stream Stream the response if TRUE.
#' @param .timeout Request timeout in seconds.
#' @param .max_tries Maximum retry attempts for requests (default: 3).
#'
#' @return A list containing the assistant's reply and response headers.
#' @noRd
perform_chat_request <- function(.request, 
                                .api, 
                                .stream = FALSE, 
                                .timeout = 60, 
                                .max_tries = 3) {

  api_name <- .api@long_name
  
  if (.stream == TRUE) {
    # Initialize the streaming environment variable
    .tidyllm_stream_env$stream <- ""
    message("\n---------\nStart ", api_name, " streaming: \n---------\n")
    
    # Generate the appropriate callback function
    callback_fn <- generate_callback_function(.api)
    
    # Perform the streaming request and process it with the callback function
    response <- httr2::req_perform_stream(
      .request,
      callback = callback_fn,
      buffer_kb = 0.05, 
      round = "line"
    )
    
    # Assign the final streamed response
    assistant_reply <- .tidyllm_stream_env$stream
    # And delete the content of the environment variable just in case
    .tidyllm_stream_env$stream <- ""
    
    # Capture response headers for rate limiting information
    response_headers <- httr2::resp_headers(response)
    
  } else {
    # Non-streaming mode
    response <- .request |>
      httr2::req_timeout(.timeout) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_retry(max_tries = .max_tries,  
                       retry_on_failure = TRUE,
                       is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 503)) |>
      httr2::req_perform()
    
    # Parse the response body as JSON when not streaming
    body_json <- httr2::resp_body_json(response)
    parse_chat_response <- parse_chat_function(.api)
    assistant_reply <- parse_chat_response(body_json)
    
    # Capture response headers for rate limiting information
    response_headers <- httr2::resp_headers(response)
  }
  
  if(.stream == FALSE) metadata <- extract_response_metadata(body_json)
  if(.stream == TRUE)  metadata <- NULL
  
  list(assistant_reply  = assistant_reply, 
       headers          = response_headers,
       meta             = metadata)
}



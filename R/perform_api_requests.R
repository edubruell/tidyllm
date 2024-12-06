#' Perform a generic request with retry and backoff
#'
#' @param .request The httr2 request object.
#' @param .timeout Request timeout in seconds.
#' @param .max_tries Maximum retry attempts for requests (default: 3).
#'
#' @return A list containing the assistant's reply and response headers.
#' @noRd
perform_generic_request <- function(.request, 
                                    .timeout = 60, 
                                    .max_tries = 3) {
  response <- .request |>
    httr2::req_timeout(.timeout) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_retry(
      max_tries = .max_tries,
      retry_on_failure = TRUE,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 503)
    ) |>
    httr2::req_perform()
  
  list(
    content = httr2::resp_body_json(response),
    headers = httr2::resp_headers(response),
    status  = httr2::resp_status(response)
  )
}



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
    response_data <- perform_generic_request(.request, .timeout, .max_tries)
    parse_chat_response <- parse_chat_function(.api)
    assistant_reply <- parse_chat_response(response_data$content)
    # Capture response headers for rate limiting information
    response_headers <- response_data$headers
    metadata <- extract_metadata(.api,response_data$content)
    #metadata <- extract_response_metadata(response_data$content)
  }
  
  if(.stream == TRUE)  metadata <- NULL
  
  list(assistant_reply  = assistant_reply, 
       headers          = response_headers,
       meta             = metadata,
       raw              = response_data)
}


#' Perform Embedding Request
#'
#' A utility function to perform embedding requests.
#'
#' @param .request The prepared httr2 request object.
#' @param .timeout Timeout for the API request in seconds.
#' @param .max_tries Maximum number of retry attempts.
#' @param .fn_extract_embeddings A functiont to extract embeddings
#' @return An embedding response tibble
#' @keywords internal
#' @noRd
perform_embedding_request <- function(.request, .timeout, .max_tries, .input_texts, .fn_extract_embeddings) {
  response_data <- perform_generic_request(.request, .timeout, .max_tries)
  
  embeddings <- .fn_extract_embeddings(
    response_data$content,
    response_data$status >= 400,  # Check if the status indicates an error
    response_data$headers
  )
  
  if (is.null(embeddings) || length(embeddings) == 0) {
    stop("No embeddings returned in the response.")
  }
  
  tibble::tibble(
    input = .input_texts,
    embeddings = embeddings
  )
}


#perform_embedding_request <- function(.request, 
#                                      .timeout, 
#                                      .max_tries,
#                                      .input_texts,
#                                      .fn_extract_embeddings
#                                      ) {
#  response <- .request |>
#    httr2::req_timeout(.timeout) |>
#    httr2::req_error(is_error = function(resp) FALSE) |>
#    httr2::req_retry(
#      max_tries = .max_tries,
#      retry_on_failure = TRUE,
#      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 503)
#    ) |>
#    httr2::req_perform()
#  
#  response_content <- httr2::resp_body_json(response)
#  response_headers <- httr2::resp_headers(response)
#  
#  # Parse the response and extract embeddings
#  embeddings <- .fn_extract_embeddings(response_content,
#                                       httr2::resp_is_error(response),
#                                       response_headers)
#    
#  # Check if embeddings are present
#  if (is.null(embeddings) | length(embeddings)==0) {
#    stop("No embeddings returned in the response.")
#  }
#  
#  tibble::tibble(
#    input = .input_texts,
#    embeddings = embeddings
#  )
#}
#


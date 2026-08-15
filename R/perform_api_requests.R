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
    message("\n---------\nStart ", api_name, " streaming: \n---------\n")
    # `blocking = TRUE` waits for bytes instead of spinning on empty reads. The
    # console path has nothing else to do while it waits, and the event-loop
    # driver (0.6.0 Phase B) is what needs the non-blocking form.
    response <- httr2::req_perform_connection(.request, blocking = TRUE)
    # `.timeout` now applies to streaming too, as an idle deadline between
    # events rather than a total, so a long generation is not killed for being
    # long. Before 0.6.0 the streaming path had no timeout backstop at all.
    stream_response <- handle_stream(.api, response, .idle_timeout = .timeout)

    # `raw` carries the same shape a blocking request produces, so that
    # everything downstream of the transport reads one shape. The tool loop is
    # the reason: it looks for tool calls in `raw$content`, and before 0.6.0 a
    # stream put its bare event list there instead, which is why streaming and
    # tools could not be combined.
    #
    # The raw events are deliberately not stashed beside the body: nothing
    # downstream reads them any more, and keeping a second representation of the
    # same response is what let the streaming and blocking paths diverge in the
    # first place.
    response_data <- list(
      content = assemble_stream_response(.api, stream_response$raw_data),
      headers = httr2::resp_headers(response),
      status  = httr2::resp_status(response)
    )

    # Once the events are a response body, a stream needs no interpretation of
    # its own: the reply and the metadata come out of the same two generics the
    # blocking path uses. `stream_response$reply` is what the sink already
    # printed and is deliberately *not* used here. Reading the reply back out of
    # the assembled body is what makes a lossy assembler visible; while the two
    # accumulators ran side by side, an assembler could drop content and every
    # plain streaming test would still pass, because the reply came from the
    # other one.
    interpreted <- interpret_chat_response(.api, response_data)
    interpreted$meta$stream <- TRUE
    return(interpreted)
  }

  interpret_chat_response(
    .api,
    perform_generic_request(.request, .timeout, .max_tries)
  )
}


#' Turn a completed non-streaming response into the shape `finish_chat_response()` expects
#'
#' Split out of `perform_chat_request()` because that function fuses transport
#' with interpretation, and the async and parallel drivers do their own
#' transport: `req_perform_promise()` and `req_perform_parallel()` both hand back
#' a bare response with no api object attached. Without this seam each of them
#' would have to re-implement the non-streaming branch, which is the duplication
#' the 0.6.0 pipeline split exists to prevent.
#'
#' @param .api An api provider object.
#' @param .response_data The list returned by `perform_generic_request()`, i.e.
#'   `content`, `headers` and `status`.
#'
#' @return A list of `assistant_reply`, `headers`, `meta` and `raw`.
#' @noRd
interpret_chat_response <- function(.api, .response_data) {
  list(
    assistant_reply = parse_chat_response(.api, .response_data$content),
    headers         = .response_data$headers,
    meta            = extract_metadata(.api, .response_data$content),
    raw             = .response_data
  )
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





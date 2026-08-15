#' The shared streaming pump
#'
#' One loop for every provider. Before 0.6.0 each provider carried its own
#' hand-rolled `repeat` loop that fused four concerns: the transport, the event
#' parsing, the output sink and the loop control. Only the parsing is genuinely
#' provider-specific, so it is the only part that stays behind a generic
#' (`parse_stream_event()`); the rest lives here once.
#'
#' @noRd
NULL

#' The value a `parse_stream_event()` method returns
#'
#' `kind` exists because a text delta, a thinking delta and a provider error are
#' three different things that used to be conflated. `keep` decides whether the
#' event is stored; providers differ (OpenAI's Responses API keeps only the
#' terminal event, Claude keeps all of them).
#'
#' Kept events are the entire response. `assemble_stream_body()` rebuilds
#' the body out of them, and the reply, the metadata and the tool calls are all
#' read back from that body, so an event dropped here is content that will never
#' be seen. Narrowing what a provider keeps is therefore not the local memory
#' optimisation it looks like.
#'
#' @noRd
stream_event <- function(kind  = "noop",
                         text  = NULL,
                         done  = FALSE,
                         error = NULL,
                         keep  = FALSE,
                         event = NULL) {
  list(kind = kind, text = text, done = done, error = error,
       keep = keep, event = event)
}

#' Carry the response-level fields of a stream chunk into an accumulator
#'
#' Used by `assemble_stream_body()` methods, which rebuild a response body
#' out of chunks that each repeat some of its top-level fields.
#'
#' `purrr::compact()` is the point of the helper. `jsonlite` keeps a JSON `null`
#' as a named element with a NULL value, and `modifyList()` reads a NULL value as
#' "delete this key". OpenAI-compatible endpoints send `"usage": null` on every
#' chunk but the last when usage reporting is on, so merging naively lets a later
#' chunk delete a field an earlier one supplied.
#'
#' @noRd
merge_stream_envelope <- function(.envelope, .event, .keys) {
  utils::modifyList(.envelope, purrr::compact(.event[intersect(names(.event), .keys)]))
}

#' Default per-delta sink: the console, exactly as before 0.6.0.
#'
#' @noRd
stream_sink_console <- function(.text) {
  cat(.text)
  utils::flush.console()
}

#' Read one chunk in whichever transport the provider speaks.
#'
#' Returns NULL when nothing complete is available yet, which the pump treats as
#' "not an event" rather than "end of stream".
#'
#' @noRd
read_stream_chunk <- function(.api, .response) {
  switch(
    .api@stream_transport,
    sse = {
      chunk <- httr2::resp_stream_sse(.response)
      if (is.null(chunk) || is.null(chunk$data) || !nzchar(chunk$data)) NULL else chunk
    },
    lines = {
      chunk <- httr2::resp_stream_lines(.response)
      if (length(chunk) == 0 || !nzchar(chunk[[1]])) NULL else chunk
    },
    stop("Unknown stream transport '", .api@stream_transport, "' for ", .api@long_name)
  )
}

#' Parse a JSON payload from a stream, returning NULL rather than erroring.
#'
#' @noRd
parse_stream_json <- function(.data) {
  tryCatch(
    jsonlite::fromJSON(.data, simplifyVector = FALSE, simplifyDataFrame = FALSE),
    error = function(e) NULL
  )
}

#' Drive a streaming response to completion.
#'
#' Termination is three-way, which is the point of the rewrite. Before 0.6.0 the
#' loops relied solely on a provider-specific terminal event, so a stream that
#' ended without one span forever: a truncated connection, a mid-stream provider
#' error, or a `finish_reason` the loop did not recognise. Now the pump also
#' checks whether the connection has completed, and enforces an idle deadline.
#'
#' @param .api Provider object; supplies the transport and the parser.
#' @param .response An open streaming `httr2_response`.
#' @param .on_chunk Sink called with each text delta. Defaults to the console.
#' @param .idle_timeout Seconds to wait with no event before giving up. Measured
#'   between events, not from the start, so a long generation is not killed for
#'   being long.
#' @param .verbose Whether to print the "Stream finished" banner.
#'
#' @return `list(reply, raw_data)`. `raw_data` is what the response is rebuilt
#'   from. `reply` is only what the sink was fed as it arrived: it is the live
#'   view, not the record, and `perform_chat_request()` deliberately reads the
#'   final reply back out of the assembled body instead.
#' @noRd
run_stream_pump <- function(.api,
                            .response,
                            .on_chunk     = NULL,
                            .idle_timeout = 60,
                            .verbose      = TRUE) {
  state <- new_stream_state(.api, .response, .on_chunk, .idle_timeout, .verbose)
  # A blocking connection makes `read_stream_chunk()` wait for bytes, so a step
  # that reports "wait" is only possible on the non-blocking form the event-loop
  # driver opens. Looping on it costs nothing here and keeps one step function.
  while (!isTRUE(state$done)) stream_pump_step(state)
  stream_pump_result(state)
}

#' The pump's state, so that it can be stepped rather than run
#'
#' An environment rather than a list because both drivers advance the same
#' object in place: the blocking loop below and the `later`-driven one in
#' `R/async_chat.R`, which is handed control back between chunks and must find
#' the accumulated text where it left it.
#'
#' @noRd
new_stream_state <- function(.api,
                             .response,
                             .on_chunk     = NULL,
                             .idle_timeout = 60,
                             .verbose      = TRUE) {
  state <- new.env(parent = emptyenv())
  state$api          <- .api
  state$response     <- .response
  state$sink         <- .on_chunk %||% stream_sink_console
  state$idle_timeout <- .idle_timeout
  state$verbose      <- .verbose
  state$text_parts   <- list()
  state$events       <- list()
  state$done         <- FALSE
  state$last_event   <- Sys.time()
  state
}

#' Advance the pump by one read
#'
#' @return `"done"` when the provider's terminal event arrived, `"wait"` when
#'   nothing complete was available yet, `"continue"` otherwise. Raises on a
#'   provider error, a truncated connection or an exceeded idle deadline; the
#'   caller owns nothing that needs unwinding, because the response is closed
#'   before the error is thrown.
#' @noRd
stream_pump_step <- function(.state) {
  chunk <- read_stream_chunk(.state$api, .state$response)

  if (is.null(chunk)) {
    # Nothing parseable arrived. Either the connection is done, in which case
    # the provider never sent its terminal event, or we are still waiting.
    if (httr2::resp_stream_is_complete(.state$response)) {
      close(.state$response)
      .state$done <- TRUE
      stop(sprintf(
        "%s stream ended after %d events without a completion signal. The connection was closed or truncated before the response finished.",
        .state$api@long_name, length(.state$events)
      ), call. = FALSE)
    }
    if (difftime(Sys.time(), .state$last_event, units = "secs") > .state$idle_timeout) {
      close(.state$response)
      .state$done <- TRUE
      stop(sprintf(
        "%s stream produced no data for %g seconds; giving up.",
        .state$api@long_name, .state$idle_timeout
      ), call. = FALSE)
    }
    return("wait")
  }

  .state$last_event <- Sys.time()
  parsed <- parse_stream_event(.state$api, chunk)

  if (isTRUE(parsed$keep)) .state$events <- append(.state$events, list(parsed$event))

  if (!is.null(parsed$error)) {
    close(.state$response)
    .state$done <- TRUE
    stop(sprintf("%s stream error: %s", .state$api@long_name, parsed$error), call. = FALSE)
  }

  if (parsed$kind == "text" && !is.null(parsed$text) && nzchar(parsed$text)) {
    .state$text_parts <- append(.state$text_parts, list(parsed$text))
    .state$sink(parsed$text)
  }

  if (isTRUE(parsed$done)) {
    .state$done <- TRUE
    close(.state$response)
    if (isTRUE(.state$verbose)) message("\n---------\nStream finished\n---------\n")
    return("done")
  }

  "continue"
}

#' @noRd
stream_pump_result <- function(.state) {
  list(
    reply    = paste0(unlist(.state$text_parts), collapse = ""),
    raw_data = .state$events
  )
}

#' Every provider streams through the shared pump.
#'
#' Providers customise streaming by implementing `parse_stream_event()` and by
#' setting `stream_transport`, never by writing another loop.
#'
#' @noRd
method(handle_stream, list(APIProvider, new_S3_class("httr2_response"))) <-
  function(.api, .stream_response, .on_chunk = NULL, .idle_timeout = 60,
           .verbose = TRUE) {
    run_stream_pump(.api, .stream_response, .on_chunk = .on_chunk,
                    .idle_timeout = .idle_timeout, .verbose = .verbose)
  }

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
#' event is stored for `extract_metadata_stream()`; providers differ (OpenAI's
#' Responses API keeps only the terminal event, Claude keeps all of them).
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
#' @return `list(reply, raw_data)`, the same shape the per-provider
#'   `handle_stream()` methods returned before, so `extract_metadata_stream()`
#'   and `perform_chat_request()` are unaffected.
#' @noRd
run_stream_pump <- function(.api,
                            .response,
                            .on_chunk     = NULL,
                            .idle_timeout = 60,
                            .verbose      = TRUE) {
  sink        <- .on_chunk %||% stream_sink_console
  text_parts  <- list()
  events      <- list()
  finished    <- FALSE
  last_event  <- Sys.time()

  finish <- function() {
    if (isTRUE(.verbose)) message("\n---------\nStream finished\n---------\n")
  }

  repeat {
    chunk <- read_stream_chunk(.api, .response)

    if (is.null(chunk)) {
      # Nothing parseable arrived. Either the connection is done, in which case
      # the provider never sent its terminal event, or we are still waiting.
      if (httr2::resp_stream_is_complete(.response)) {
        close(.response)
        stop(sprintf(
          "%s stream ended after %d events without a completion signal. The connection was closed or truncated before the response finished.",
          .api@long_name, length(events)
        ), call. = FALSE)
      }
      if (difftime(Sys.time(), last_event, units = "secs") > .idle_timeout) {
        close(.response)
        stop(sprintf(
          "%s stream produced no data for %g seconds; giving up.",
          .api@long_name, .idle_timeout
        ), call. = FALSE)
      }
      next
    }

    last_event <- Sys.time()
    parsed     <- parse_stream_event(.api, chunk)

    if (isTRUE(parsed$keep)) events <- append(events, list(parsed$event))

    if (!is.null(parsed$error)) {
      close(.response)
      stop(sprintf("%s stream error: %s", .api@long_name, parsed$error), call. = FALSE)
    }

    if (parsed$kind == "text" && !is.null(parsed$text) && nzchar(parsed$text)) {
      text_parts <- append(text_parts, list(parsed$text))
      sink(parsed$text)
    }

    if (isTRUE(parsed$done)) {
      finished <- TRUE
      close(.response)
      finish()
      break
    }
  }

  list(
    reply    = paste0(unlist(text_parts), collapse = ""),
    raw_data = events
  )
}

#' Every provider streams through the shared pump.
#'
#' Providers customise streaming by implementing `parse_stream_event()` and by
#' setting `stream_transport`, never by writing another loop.
#'
#' @noRd
method(handle_stream, list(APIProvider, new_S3_class("httr2_response"))) <-
  function(.api, .stream_response, .on_chunk = NULL, .idle_timeout = 60) {
    run_stream_pump(.api, .stream_response,
                    .on_chunk = .on_chunk, .idle_timeout = .idle_timeout)
  }

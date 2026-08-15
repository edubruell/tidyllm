#' The three-part chat pipeline
#'
#' Every `*_chat()` function is the same three steps in the same order: build a
#' request, perform it, turn the response into an `LLMMessage`. Before 0.6.0 all
#' three were welded into one function body per provider, which meant nothing
#' but `*_chat()` itself could reach the middle: `.dry_run` handed back a bare
#' `httr2` request with no api object, no tool definitions and no `json` flag, so
#' a caller could send it but could never interpret what came back.
#'
#' The seam is the same one ellmer draws, and it is what the async and parallel
#' entry points consume:
#'
#'   build   ->  a `tidyllm_chat_request`: request plus everything finish needs
#'   perform ->  a response (blocking value or stream today)
#'   finish  ->  tool loop, reply extraction, rate limit, `add_message()`
#'
#' `.dry_run = TRUE` is now `build_chat_request(...)$request`, so the user-facing
#' contract is unchanged.
#'
#' @noRd
NULL

#' Bundle a built request with everything `finish_chat_response()` needs
#'
#' The `mode` is baked in at build time on purpose. Every provider commits to
#' streaming in the request itself; Gemini in the URL path, Claude, OpenAI and
#' the ChatCompletions family in the body. A request built for `"value"` will
#' not stream, so `perform_chat()` cannot be handed the choice.
#'
#' @param .reply_fn Optional; extracts the reply from the response. Defaults to
#'   `response$assistant_reply`. Claude re-derives it from the raw body instead.
#' @param .meta_fn Optional; post-processes metadata, given `(meta, response)`.
#'   Perplexity uses it to fold in search results.
#' @param .track_rate_limit Only 6 of the 13 chat functions track rate limits,
#'   because only those providers return the headers to track.
#' @noRd
new_chat_request <- function(.request,
                             .api,
                             .llm,
                             .body = NULL,
                             .tools_def = NULL,
                             .json = FALSE,
                             .mode = "value",
                             .timeout = 60,
                             .max_tries = 3,
                             .max_tool_rounds = 10,
                             .verbose = FALSE,
                             .track_rate_limit = FALSE,
                             .parse_logprobs = FALSE,
                             .reply_fn = NULL,
                             .meta_fn = NULL,
                             .perform_fn = NULL) {
  structure(
    list(
      request          = .request,
      api              = .api,
      llm              = .llm,
      body             = .body,
      tools_def        = .tools_def,
      json             = .json,
      mode             = .mode,
      timeout          = .timeout,
      max_tries        = .max_tries,
      max_tool_rounds  = .max_tool_rounds,
      verbose          = .verbose,
      track_rate_limit = .track_rate_limit,
      parse_logprobs   = .parse_logprobs,
      reply_fn         = .reply_fn,
      meta_fn          = .meta_fn,
      perform_fn       = .perform_fn
    ),
    class = "tidyllm_chat_request"
  )
}

#' Is this built request a streaming one?
#' @noRd
chat_request_streams <- function(.built) {
  .built$mode %in% c("stream", "async-stream")
}

#' Perform a built chat request
#'
#' Deliberately *not* named `perform_chat_request()`. That function keeps its
#' name and its exact signature because `process_tool_loop()` calls it on every
#' follow-up round; rebinding the name to new semantics would make any future
#' bisect through the tool loop miserable.
#'
#' @noRd
perform_chat <- function(.built) {
  # `openai_chat(.stateful = TRUE)` supplies its own, because a stale
  # server-side context has to be retried against a rebuilt body.
  if (!is.null(.built$perform_fn)) return(.built$perform_fn(.built))

  perform_chat_request(
    .request   = .built$request,
    .api       = .built$api,
    .stream    = chat_request_streams(.built),
    .timeout   = .built$timeout,
    .max_tries = .built$max_tries
  )
}

#' Turn a performed response into the updated LLMMessage
#'
#' This is not a pure terminal step: `process_tool_loop()` inside it performs up
#' to `.max_tool_rounds` further blocking HTTP round trips. That matters for the
#' async entry point, which must not hand back a job that blocks inside
#' `fetch_job()`.
#'
#' @noRd
finish_chat_response <- function(.built, .response) {
  api      <- .built$api
  streams  <- chat_request_streams(.built)

  if (!streams && !is.null(.built$tools_def)) {
    .response <- process_tool_loop(
      .api             = api,
      .response        = .response,
      .tools_def       = .built$tools_def,
      .request_body    = .built$body,
      .request         = .built$request,
      .timeout         = .built$timeout,
      .max_tries       = .built$max_tries,
      .max_tool_rounds = .built$max_tool_rounds
    )
  }

  assistant_reply <- if (is.null(.built$reply_fn)) {
    .response$assistant_reply
  } else {
    .built$reply_fn(.response)
  }

  logprobs <- if (isTRUE(.built$parse_logprobs)) parse_logprobs(api, .response$raw) else NULL

  meta <- .response$meta
  if (!is.null(.built$meta_fn)) meta <- .built$meta_fn(meta, .response)

  if (isTRUE(.built$track_rate_limit)) {
    track_rate_limit(api, .response$headers, .built$verbose)
  }

  add_message(
    .llm      = .built$llm,
    .role     = "assistant",
    .content  = assistant_reply,
    .json     = .built$json,
    .meta     = meta,
    .logprobs = logprobs
  )
}

#' Run all three steps
#'
#' @noRd
run_chat_pipeline <- function(.built, .dry_run = FALSE) {
  if (.dry_run) return(.built$request)
  finish_chat_response(.built, perform_chat(.built))
}

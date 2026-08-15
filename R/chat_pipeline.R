#' The three-part chat pipeline
#'
#' Every `*_chat()` function is the same three steps in the same order: build a
#' request, perform it, turn the response into an `LLMMessage`. Before 0.6.0 all
#' three were welded into one function body per provider, which meant nothing
#' but `*_chat()` itself could reach the middle: `.dry_run` handed back a bare
#' `httr2` request with no api object, no tool definitions and no `json` flag, so
#' a caller could send it but could never interpret what came back.
#'
#' The three steps, and what the async and parallel entry points consume:
#'
#'   build   ->  a `tidyllm_chat_request`: request plus everything finish needs
#'   perform ->  a response (blocking value or stream today)
#'   finish  ->  tool loop, reply extraction, rate limit, `add_message()`
#'
#' `.dry_run = TRUE` is now `<provider>_build_chat_request(...)$request`, so the
#' user-facing contract is unchanged.
#'
#' @noRd
NULL

#' Bundle a built request with everything `finish_chat_response()` needs
#'
#' The `mode` is baked in at build time on purpose. Every provider commits to
#' streaming in the request itself; Gemini in the URL path, Claude, OpenAI and
#' the ChatCompletions family in the body. A request built for `"value"` will
#' not stream, so the perform step cannot be handed the choice.
#'
#' @param .meta_fn Optional; post-processes metadata, given `(meta, response)`.
#'   Perplexity uses it to fold in search results.
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
      meta_fn          = .meta_fn,
      perform_fn       = .perform_fn
    ),
    class = "tidyllm_chat_request"
  )
}

#' Does this built request stream?
#'
#' `"async-stream"` is not produced anywhere yet. It is listed because the
#' streaming tool loop and the event-loop driver both need a mode that streams
#' without blocking, and a driver that emits it should not have to remember to
#' edit this predicate too.
#'
#' @noRd
chat_request_streams <- function(.built) {
  .built$mode %in% c("stream", "async-stream")
}

#' Perform a built chat request
#'
#' Named for the built object it takes, so that call sites cannot be confused
#' with `perform_chat_request()`, which takes an httr2 request and keeps both its
#' name and its signature because `process_tool_loop()` calls it every follow-up
#' round.
#'
#' @noRd
perform_built_request <- function(.built) {
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

  # Streaming runs the same loop as everything else. `assemble_stream_response()`
  # folds the stream's events back into the body shape the tool generics read,
  # so `has_tool_calls()` and friends are reused unchanged, and each follow-up
  # round streams too.
  if (!is.null(.built$tools_def)) {
    .response <- process_tool_loop(
      .api             = api,
      .response        = .response,
      .tools_def       = .built$tools_def,
      .request_body    = .built$body,
      .request         = .built$request,
      .timeout         = .built$timeout,
      .max_tries       = .built$max_tries,
      .max_tool_rounds = .built$max_tool_rounds,
      .stream          = streams
    )
  }

  # Both generics have an APIProvider default returning NULL, and every provider
  # that inherits a method it should not use overrides it back. So these are
  # unconditional: whether they do anything is the provider class's business.
  logprobs <- parse_logprobs(api, .response$raw)

  meta <- .response$meta
  if (!is.null(.built$meta_fn)) meta <- .built$meta_fn(meta, .response)

  track_rate_limit(api, .response$headers, .built$verbose)

  add_message(
    .llm      = .built$llm,
    .role     = "assistant",
    .content  = .response$assistant_reply,
    .json     = .built$json,
    .meta     = meta,
    .logprobs = logprobs
  )
}

#' @noRd
run_chat_pipeline <- function(.built, .dry_run = FALSE) {
  if (.dry_run) return(.built$request)
  finish_chat_response(.built, perform_built_request(.built))
}

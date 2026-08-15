#' Send many chats at once
#'
#' Performs a list of `LLMMessage`s against one provider concurrently and
#' returns their replies in the same order, with the same names. It is the
#' middle ground between `chat()` in a loop, which waits for each answer before
#' starting the next, and `send_batch()`, which is cheaper still but returns
#' hours later.
#'
#' @param .llms A list of `LLMMessage` objects. Names are preserved.
#' @param .provider A provider function call, as in [chat()].
#' @param .max_active Maximum number of requests in flight at once. Keep it
#'   modest against a rate-limited provider: `httr2` applies retries across the
#'   whole set rather than per request, so a high number is a good way to
#'   collect 429s.
#' @param .throttle Optional maximum number of requests per second, applied
#'   across the set. The straightforward defence against a rate limit.
#' @param .on_error `"continue"` (default) keeps going and puts the failure in
#'   that element's slot; `"stop"` aborts the whole set on the first failure.
#' @inheritParams chat
#'
#' @return A list the same length as `.llms`. Successful elements are
#'   `LLMMessage` objects; failed ones, under `.on_error = "continue"`, are the
#'   condition that failed, so nothing is silently dropped and a downstream
#'   `get_reply()` fails loudly on exactly the elements that have no reply.
#'
#' @details
#' Neither streaming nor tool calls are supported here, and both are refused
#' rather than quietly ignored. A tool call is a conversation, not a request:
#' its rounds would run one after another after the parallel phase, which is a
#' surprising performance cliff rather than a feature. Use [send_chat()] for
#' those, which can have several conversations in flight at once.
#'
#' @examples
#' \dontrun{
#' questions <- list(
#'   physics = llm_message("What is a photon?"),
#'   biology = llm_message("What is a ribosome?")
#' )
#' answers <- parallel_chat(questions, claude())
#' purrr::map_chr(answers, get_reply)
#' }
#'
#' @export
parallel_chat <- function(
    .llms,
    .provider = getOption("tidyllm_chat_default"),
    .max_active = 4,
    .throttle = NULL,
    .on_error = "continue",
    .temperature = NULL,
    .timeout = NULL,
    .top_p = NULL,
    .max_tries = NULL,
    .model = NULL,
    .verbose = NULL,
    .json_schema = NULL,
    .seed = NULL,
    .stop = NULL,
    .frequency_penalty = NULL,
    .presence_penalty = NULL,
    .stream = FALSE,
    .tools = NULL) {

  # Named and refused rather than left out of the signature, so that the error
  # says why instead of "unused argument".
  if (isTRUE(.stream)) {
    stop("parallel_chat() cannot stream: there is no single console to stream ",
         "several replies into. Use send_chat(.stream = TRUE) per message.",
         call. = FALSE)
  }
  if (!is.null(.tools)) {
    stop("parallel_chat() does not run tool calls: their rounds would run one ",
         "after another once the parallel phase was over. Use send_chat() for ",
         "conversations that call tools.", call. = FALSE)
  }

  if (!is.list(.llms) || length(.llms) == 0) {
    stop("Input .llms must be a non-empty list of LLMMessage objects.")
  }
  if (!all(vapply(.llms, function(m) S7_inherits(m, LLMMessage), logical(1)))) {
    stop("Every element of .llms must be an LLMMessage object.")
  }
  if (is.null(.provider)) {
    stop("You need to specify a .provider function in parallel_chat().")
  }
  .on_error <- match.arg(.on_error, c("continue", "stop"))

  if (rlang::is_function(.provider)) .provider <- .provider()
  provider_expr <- if (rlang::is_call(.provider)) {
    .provider
  } else {
    rlang::quo_get_expr(rlang::enquo(.provider))
  }

  common_args <- list(
    .model = .model, .verbose = .verbose, .max_tries = .max_tries,
    .timeout = .timeout, .temperature = .temperature, .top_p = .top_p,
    .json_schema = .json_schema, .seed = .seed, .stop = .stop,
    .frequency_penalty = .frequency_penalty, .presence_penalty = .presence_penalty
  )
  common_args <- common_args[!vapply(common_args, is.null, logical(1))]

  built <- lapply(.llms, function(llm) {
    validate_message_attachments(llm, provider_expr)
    dispatch_to_provider(provider_expr, "build", c(list(.llm = llm), common_args))
  })

  if (!all(vapply(built, inherits, logical(1), "tidyllm_chat_request"))) {
    stop("This provider does not support parallel_chat() yet.", call. = FALSE)
  }

  requests <- lapply(built, function(b) {
    request <- b$request
    if (!is.null(.throttle)) request <- httr2::req_throttle(request, rate = .throttle)
    request
  })

  responses <- httr2::req_perform_parallel(
    requests,
    on_error    = if (identical(.on_error, "stop")) "stop" else "continue",
    max_active  = .max_active,
    progress    = FALSE
  )

  # `req_perform_parallel()` returns a positionally indexed list and drops the
  # names of the one it was given, which is a nasty surprise for the `imap()`
  # style pipelines this function exists to serve. Re-attaching them here is not
  # optional.
  names(responses) <- names(.llms)

  results <- purrr::imap(responses, function(response, index) {
    if (!inherits(response, "httr2_response")) return(response)

    tryCatch({
      b <- built[[index]]
      finish_chat_response(b, interpret_chat_response(b$api, list(
        content = httr2::resp_body_json(response),
        headers = httr2::resp_headers(response),
        status  = httr2::resp_status(response)
      )))
    }, error = function(e) e)
  })

  failed <- !vapply(results, function(r) S7_inherits(r, LLMMessage), logical(1))
  if (any(failed)) {
    warning(sprintf(
      "%d of %d requests failed; those elements hold the condition rather than an LLMMessage.",
      sum(failed), length(results)
    ), call. = FALSE)
  }

  results
}

#' Non-blocking chats
#'
#' `send_chat()` is the third way to run a chat, beside `chat()` and
#' `send_batch()`. `chat()` blocks the session until the answer is complete;
#' `send_batch()` hands thousands of prompts to a provider's batch endpoint and
#' collects them later; `send_chat()` dispatches *one* request and gives back a
#' handle immediately, so the session stays usable while it runs.
#'
#' The handle wears the vocabulary the package already uses for batches:
#' `check_job()` for status, `fetch_job()` for the finished `LLMMessage`. Two
#' accessors are specific to a live chat: `get_partial()` for the text so far,
#' and `cancel_job()`.
#'
#' Nothing runs on a thread or in a second process. The request is driven from
#' R's own event loop, one chunk at a time, in the gaps between whatever else the
#' session is doing; the work happens when `later::run_now()` is reached, which
#' every one of the accessors below does on your behalf. That is why a Shiny app
#' can serve other sessions while a chat streams, and equally why a long
#' `Sys.sleep()` in your own code pauses the chat with it.
#'
#' @noRd
NULL

#' The `later` dependency, checked where it is used
#'
#' `later` is in `Suggests` under the 0.6.0 dependency budget: it is compiled
#' (`LinkingTo Rcpp`) and nothing else in tidyllm needs it. `shiny` imports it,
#' so it is always present in the environment where this driver is the point,
#' and httr2 keeps it in its own `Suggests` for the same non-blocking helpers.
#'
#' @noRd
check_later_installed <- function() {
  rlang::check_installed("later", reason = "to run chats without blocking the session.")
}

#' Dispatch a non-streaming request without waiting for it
#'
#' The connection driver cannot serve this case. `req_perform_connection()`
#' returns as soon as the *headers* arrive, which for a streamed reply is
#' immediately and for a non-streamed one is when the model has finished: the
#' whole wait would happen inside `send_chat()`, which is the one thing it must
#' not do. Measured against `openai(.stream = FALSE)`: the job was already
#' complete when `send_chat()` returned.
#'
#' `req_perform_promise()` is the httr2 entry point for this, and it keeps
#' `req_retry()` and `req_timeout()`, which a connection does not. It costs the
#' `promises` package, checked here rather than depended on: this is the only
#' path in tidyllm that needs it, and a streaming job never reaches it.
#'
#' @noRd
start_promised_request <- function(.job) {
  rlang::check_installed(
    "promises",
    reason = "to run a non-streaming chat without blocking. Streaming jobs (.stream = TRUE) do not need it."
  )
  env <- .job$env

  request <- env$built$request |>
    httr2::req_timeout(env$built$timeout) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_retry(
      max_tries = env$built$max_tries,
      retry_on_failure = TRUE,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 503)
    )

  promises::then(
    httr2::req_perform_promise(request),
    onFulfilled = function(response) {
      # A cancelled job must stay cancelled: the request is already in flight
      # and there is nothing to abort, so the answer is discarded instead.
      if (!identical(env$status, "running")) return(invisible(NULL))
      env$headers     <- httr2::resp_headers(response)
      env$http_status <- httr2::resp_status(response)
      chat_job_finish(.job, httr2::resp_body_json(response))
    },
    onRejected = function(condition) {
      if (!identical(env$status, "running")) return(invisible(NULL))
      env$status <- "error"
      env$error  <- condition
    }
  )
  invisible(NULL)
}

#' A chat running in the background of this session
#'
#' Deliberately an environment: the driver writes to it from a `later` callback
#' long after `send_chat()` returned, and every accessor has to see those writes.
#' A copied list would leave the caller holding a snapshot of a job that had
#' already finished.
#'
#' @noRd
new_chat_job <- function(.built, .on_chunk = NULL) {
  # A job nobody is watching prints nothing, so the fallback sink discards
  # rather than being the console default the pump would otherwise reach for.
  # The text is accumulated regardless, which is what `get_partial()` reads.
  #
  # Both fields go onto the built request because the tool loop performs its
  # rounds through the shared pipeline, which reads them from there. Without
  # them a job with tools would stream its first round into the caller's sink
  # and every later round onto the console, banners included.
  sink <- .on_chunk %||% function(.text) invisible(NULL)
  .built$on_chunk <- sink
  .built$quiet    <- TRUE

  job <- new.env(parent = emptyenv())
  job$built     <- .built
  job$streams   <- chat_request_streams(.built)
  job$status    <- "running"
  job$result    <- NULL
  job$error     <- NULL
  job$cancel_fn <- NULL
  job$started   <- Sys.time()
  job$on_chunk  <- .on_chunk
  job$state     <- NULL

  handle <- structure(list(env = job), class = "tidyllm_chat_job")

  if (job$streams) {
    response <- httr2::req_perform_connection(.built$request, blocking = FALSE)
    # Read while the response is open; `close()` happens inside the pump, and
    # the interpretation afterwards still needs both of these.
    job$headers     <- httr2::resp_headers(response)
    job$http_status <- httr2::resp_status(response)
    job$state <- new_stream_state(
      .api          = .built$api,
      .response     = response,
      .on_chunk     = sink,
      .idle_timeout = .built$timeout,
      .verbose      = FALSE
    )
    chat_job_schedule(handle)
  } else {
    start_promised_request(handle)
  }

  handle
}

#' Advance a job as far as it can go right now
#'
#' Bounded rather than run to exhaustion. A fast local model can deliver chunks
#' faster than the loop yields, and a tick that kept reading until the stream
#' ended would be a blocking call wearing a callback's clothes: the Shiny session
#' next in line would wait for the whole reply.
#'
#' @noRd
chat_job_tick <- function(.job, .max_steps = 32L) {
  env <- .job$env
  if (!identical(env$status, "running")) return(invisible(NULL))

  for (i in seq_len(.max_steps)) {
    outcome <- tryCatch(stream_pump_step(env$state), error = function(e) e)

    if (inherits(outcome, "condition")) {
      env$status <- "error"
      env$error  <- outcome
      return(invisible(NULL))
    }
    if (identical(outcome, "done")) {
      chat_job_finish(.job, assemble_stream_body(env$built$api, env$state$events))
      return(invisible(NULL))
    }
    if (identical(outcome, "wait")) break
  }

  chat_job_schedule(.job)
  invisible(NULL)
}

#' Ask the event loop to come back when there is something to read
#'
#' `later_fd()` waits on curl's own file descriptors rather than on a timer, so
#' a tick happens when bytes arrive instead of at some rate guessed in advance.
#' The timeout is the fallback for the case where no descriptor ever becomes
#' readable; the pump's own idle deadline is what ultimately ends a dead stream.
#'
#' @noRd
chat_job_schedule <- function(.job) {
  env <- .job$env
  fds <- tryCatch(env$state$response$body$get_fdset(), error = function(e) NULL)

  env$cancel_fn <- if (is.null(fds)) {
    # httr2 documents `get_fdset()` on its streaming body but not this use of
    # it, so a version that renames or drops it degrades to a short timer rather
    # than stopping the job.
    later::later(function() chat_job_tick(.job), delay = 0.02)
  } else {
    later::later_fd(
      function(...) chat_job_tick(.job),
      readfds   = fds$reads,
      writefds  = fds$writes,
      exceptfds = fds$exceptions,
      timeout   = 0.2
    )
  }
  invisible(NULL)
}

#' Turn a finished transfer into the LLMMessage the job hands back
#'
#' Everything here is the shared pipeline: the same two interpretation generics
#' and the same `finish_chat_response()`, given the same response body. The two
#' transports meet here and differ in nothing downstream of it.
#'
#' @param .content The parsed response body: assembled from the stream's events
#'   for a streaming job, parsed from the completed response for the other.
#' @noRd
chat_job_finish <- function(.job, .content) {
  env <- .job$env

  outcome <- tryCatch({
    response_data <- list(
      content = .content,
      headers = env$headers,
      status  = env$http_status
    )

    interpreted <- interpret_chat_response(env$built$api, response_data)
    if (env$streams) interpreted$meta$stream <- TRUE

    # The tool loop is blocking, and knowingly so in this release: it performs
    # its follow-up rounds through `chat_performer()`, which is the console
    # driver. A job with tools therefore returns to the event loop only after
    # its rounds are done. `process_tool_loop()` taking a performer is what
    # makes an async version possible later without touching the loop itself.
    finish_chat_response(env$built, interpreted)
  }, error = function(e) e)

  if (inherits(outcome, "condition")) {
    env$status <- "error"
    env$error  <- outcome
  } else {
    env$status <- "done"
    env$result <- outcome
  }
  invisible(NULL)
}

#' Give the event loop a chance to advance this job
#'
#' Every accessor calls this first. Without it a script that only ever calls
#' `check_job()` in a loop would never progress: nothing else in a plain R
#' session runs the event loop.
#'
#' The short wait is not politeness, it is what makes the loop work. With a zero
#' timeout `run_now()` returns before `later` has polled curl's descriptors, so a
#' script polling `check_job()` in a tight loop burns CPU and collects the reply
#' in a few large jumps whenever the 0.2s fallback fires. Measured on a 200-word
#' Claude reply: ~40,000 fruitless polls and no partial text at all, against
#' smooth per-chunk growth with a wait. 50ms is short enough to be invisible in a
#' Shiny observer and long enough for the descriptor to become readable.
#'
#' @noRd
chat_job_run_now <- function(.job, .timeout = 0.05) {
  if (identical(.job$env$status, "running")) later::run_now(.timeout)
  invisible(NULL)
}

#' Start a chat without waiting for it
#'
#' Sends one request and returns immediately with a job handle. The session
#' stays usable while the model works; use [check_job()] for its status,
#' [get_partial()] for the text so far, and [fetch_job()] for the finished
#' `LLMMessage`, which is exactly what [chat()] would have returned.
#'
#' The request is driven from R's event loop rather than from a thread or a
#' second process, so progress happens whenever the session yields, which the
#' accessors do on your behalf. The consequence worth knowing is the other side
#' of that: a blocking call of your own, a long `Sys.sleep()` or another
#' `chat()`, pauses the job for its duration.
#'
#' A streamed job is not retried after a transient 429 or 503 the way `chat()`
#' is, because it is read from an open connection; a non-streamed one keeps the
#' usual retries.
#'
#' Requires the `later` package, and `promises` as well when `.stream = FALSE`.
#'
#' @param .llm An `LLMMessage` object.
#' @param .provider A provider function call, as in [chat()].
#' @param .on_chunk Optional function of one argument, called with each text
#'   delta as it arrives. This is the push form of a stream; `\(d) cat(d)`
#'   echoes to the console, and writing to a `reactiveVal` is all a Shiny app
#'   needs. Only meaningful with `.stream = TRUE`.
#' @param .stream Logical; whether the provider streams the reply. Streaming is
#'   what makes `.on_chunk` and `get_partial()` show progress. A non-streaming
#'   job still runs without blocking; it simply has nothing to report until it
#'   finishes.
#' @inheritParams chat
#'
#' @return A `tidyllm_chat_job`.
#'
#' @examples
#' \dontrun{
#' job <- llm_message("Summarise the history of R in 500 words") |>
#'   send_chat(claude(), .stream = TRUE)
#'
#' while (check_job(job) == "running") {
#'   cat("\r", nchar(get_partial(job)), "characters so far")
#' }
#'
#' reply <- fetch_job(job)
#' }
#'
#' @export
send_chat <- function(
    .llm,
    .provider = getOption("tidyllm_chat_default"),
    .on_chunk = NULL,
    .stream = TRUE,
    .dry_run = NULL,
    .temperature = NULL,
    .timeout = NULL,
    .top_p = NULL,
    .max_tries = NULL,
    .model = NULL,
    .verbose = NULL,
    .json_schema = NULL,
    .tools = NULL,
    .max_tool_rounds = NULL,
    .seed = NULL,
    .stop = NULL,
    .frequency_penalty = NULL,
    .presence_penalty = NULL) {

  check_later_installed()

  if (!S7_inherits(.llm, LLMMessage)) {
    stop("Input .llm must be an LLMMessage object.")
  }
  if (is.null(.provider)) {
    stop("You need to specify a .provider function in send_chat().")
  }
  if (!is.null(.on_chunk) && !is.function(.on_chunk)) {
    stop(".on_chunk must be a function of one argument, the text delta.")
  }
  if (isTRUE(.dry_run)) {
    stop("send_chat() has no .dry_run: use chat(.dry_run = TRUE) to inspect the request.")
  }

  if (rlang::is_function(.provider)) .provider <- .provider()
  provider_expr <- if (rlang::is_call(.provider)) {
    .provider
  } else {
    rlang::quo_get_expr(rlang::enquo(.provider))
  }

  common_args <- list(
    .llm = .llm, .model = .model, .verbose = .verbose, .max_tries = .max_tries,
    .stream = .stream, .timeout = .timeout, .temperature = .temperature,
    .top_p = .top_p, .json_schema = .json_schema, .seed = .seed, .stop = .stop,
    .frequency_penalty = .frequency_penalty, .presence_penalty = .presence_penalty,
    .tools = .tools, .max_tool_rounds = .max_tool_rounds
  )
  common_args <- common_args[!vapply(common_args, is.null, logical(1))]

  validate_message_attachments(.llm, provider_expr)

  # "build" is a registered action like "chat" or "embed", so the provider's own
  # builder decides which of the common arguments it accepts. `send_chat()` needs
  # the built request rather than a finished message, which is the whole reason
  # `build_chat_request()` was split out of `*_chat()`.
  built <- dispatch_to_provider(provider_expr, "build", common_args)

  if (!inherits(built, "tidyllm_chat_request")) {
    stop("This provider does not support send_chat() yet.", call. = FALSE)
  }
  if (!is.null(.on_chunk) && !chat_request_streams(built)) {
    warning(".on_chunk was given but the request does not stream, so it will never be called.",
            call. = FALSE)
  }

  new_chat_job(built, .on_chunk)
}

#' @export
print.tidyllm_chat_job <- function(x, ...) {
  env <- x$env
  chat_job_run_now(x)

  cat("<tidyllm_chat_job>\n")
  cat("  provider: ", env$built$api@long_name, "\n", sep = "")
  cat("  streaming:", if (env$streams) "yes" else "no", "\n")
  cat("  status:   ", env$status, "\n", sep = "")

  if (identical(env$status, "running")) {
    cat("  elapsed:  ", round(as.numeric(difftime(Sys.time(), env$started, units = "secs")), 1),
        "s\n", sep = "")
    if (env$streams) {
      partial <- get_partial(x)
      cat("  received: ", nchar(partial), " characters\n", sep = "")
    }
  }
  if (identical(env$status, "error")) {
    cat("  error:    ", conditionMessage(env$error), "\n", sep = "")
  }
  invisible(x)
}

#' The text a running chat has produced so far
#'
#' Non-blocking: it advances the job as far as it can right now and returns what
#' has arrived. On a finished job it returns the complete text, so a polling loop
#' needs no special case at the end. On a non-streaming job it returns `""` until
#' the job is done, because there is nothing to report before then.
#'
#' @param .job A `tidyllm_chat_job` from [send_chat()].
#' @return A character scalar.
#' @export
get_partial <- function(.job) {
  if (!inherits(.job, "tidyllm_chat_job")) {
    stop("get_partial() expects a tidyllm_chat_job from send_chat().")
  }
  chat_job_run_now(.job)

  env <- .job$env
  if (!env$streams) {
    if (identical(env$status, "done")) return(get_reply(env$result) %||% "")
    return("")
  }
  paste0(unlist(env$state$text_parts), collapse = "")
}

#' Stop a running chat
#'
#' Closes the connection and marks the job cancelled. A cancelled job cannot be
#' fetched; `fetch_job()` on one raises rather than waiting forever.
#'
#' @param .job A `tidyllm_chat_job` from [send_chat()].
#' @return The job, invisibly.
#' @export
cancel_job <- function(.job) {
  if (!inherits(.job, "tidyllm_chat_job")) {
    stop("cancel_job() expects a tidyllm_chat_job from send_chat().")
  }
  env <- .job$env
  if (!identical(env$status, "running")) return(invisible(.job))

  # A streaming job is cancelled for real: the scheduled callback is dropped and
  # the connection closed. A non-streaming one is already in flight through
  # httr2's pool with no handle to abort, so the status is what does the work,
  # and the promise's handlers check it before writing a result nobody wants.
  if (is.function(env$cancel_fn)) env$cancel_fn()
  if (!is.null(env$state)) try(close(env$state$response), silent = TRUE)
  env$status <- "cancelled"
  invisible(.job)
}

#' @export
check_job.tidyllm_chat_job <- function(.job, ...) {
  chat_job_run_now(.job)
  .job$env$status
}

#' @export
fetch_job.tidyllm_chat_job <- function(.job, .provider = NULL, ...) {
  env <- .job$env

  # Blocks by running the event loop rather than by sleeping, so anything else
  # scheduled on it, another job, a Shiny session, keeps making progress while
  # this one waits.
  while (identical(env$status, "running")) later::run_now(0.05)

  switch(
    env$status,
    done      = env$result,
    error     = stop(env$error),
    cancelled = stop("This chat job was cancelled; there is no reply to fetch.", call. = FALSE),
    stop("Unknown job status: ", env$status)
  )
}

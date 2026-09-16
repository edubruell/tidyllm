#' @noRd
api_claude_cli <- new_class("ClaudeCLI", APIProvider)

#' Find the Claude CLI, without relying on the PATH alone
#'
#' `Sys.which()` on its own is not enough. A GUI R session does not inherit the
#' PATH from the user's shell profile: RStudio on macOS starts from the launch
#' environment, so `~/.local/bin`, where the CLI installs itself by default, is
#' frequently absent even though `claude` runs fine in the same user's terminal.
#' Reported from RStudio on 2026-09-16, with the binary sitting in
#' `~/.local/bin/claude` the whole time.
#'
#' The order is: an explicit setting, then the PATH, then the handful of places
#' the installers actually use. A caller who passes a path of their own gets that
#' path and no searching.
#'
#' @noRd
claude_cli_binary <- function(.binary = "claude") {
  # A path rather than a bare command name is taken at face value: the user has
  # said where it is, so a search would only second-guess them.
  if (grepl("/", .binary, fixed = TRUE)) {
    expanded <- path.expand(.binary)
    if (file.access(expanded, mode = 1L) == 0) return(expanded)
    stop(glue::glue("`{.binary}` is not an executable file."), call. = FALSE)
  }

  configured <- getOption("tidyllm_claude_cli_path", Sys.getenv("TIDYLLM_CLAUDE_CLI"))
  if (is.character(configured) && length(configured) == 1 && nzchar(configured)) {
    configured <- path.expand(configured)
    if (file.access(configured, mode = 1L) == 0) return(configured)
    stop(glue::glue(
      "The Claude CLI was set to `{configured}`, which is not an executable file.\n",
      "Fix the `tidyllm_claude_cli_path` option or the TIDYLLM_CLAUDE_CLI environment variable."
    ), call. = FALSE)
  }

  found <- Sys.which(.binary)[[1]]
  if (nzchar(found)) return(found)

  # Returned as found, not through `normalizePath()`: on this machine
  # `~/.local/bin/claude` is a symlink into a versioned directory, and resolving
  # it would pin the call to one build of a CLI that updates itself.
  for (candidate in claude_cli_search_paths(.binary)) {
    if (file.access(candidate, mode = 1L) == 0) return(candidate)
  }

  stop(glue::glue(
    "The `{.binary}` command was not found.\n",
    "`claude_cli()` runs your own installed Claude CLI, so it has to be installed and logged in first: ",
    "see https://docs.claude.com/en/docs/claude-code, then run `claude` once to sign in.\n\n",
    "If `claude` does work in your terminal, this R session simply has a different PATH, ",
    "which is usual in RStudio and other GUI front ends. Run `which claude` in a terminal and then either\n",
    "  options(tidyllm_claude_cli_path = \"/the/path/it/printed\")\n",
    "in your .Rprofile, or pass it directly with claude_cli(.binary = \"/the/path/it/printed\")."
  ), call. = FALSE)
}

#' Where the Claude CLI installs itself
#'
#' Checked only after the PATH has failed, so a normal session never reaches
#' them. Each entry is a real installer target: the official install script
#' writes to `~/.local/bin`, the native installer to `~/.claude/local`, and a
#' global npm install lands in whichever prefix npm is configured with.
#'
#' @noRd
claude_cli_search_paths <- function(.binary) {
  home <- path.expand("~")
  candidates <- c(
    file.path(home, ".local", "bin", .binary),
    file.path(home, ".claude", "local", .binary),
    file.path(home, "bin", .binary),
    file.path("/opt/homebrew/bin", .binary),
    file.path("/usr/local/bin", .binary)
  )
  if (.Platform$OS.type == "windows") {
    candidates <- c(
      candidates,
      file.path(Sys.getenv("APPDATA"), "npm", paste0(.binary, ".cmd")),
      file.path(Sys.getenv("LOCALAPPDATA"), "Programs", .binary, paste0(.binary, ".exe"))
    )
  }
  candidates
}

#' @noRd
check_processx_installed <- function() {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop(paste(
      "`claude_cli()` needs the processx package to run the CLI and read its output.",
      "Install it with install.packages(\"processx\")."
    ), call. = FALSE)
  }
}

#' The Claude CLI authenticates itself, so there is no key to look up
#'
#' The whole point of this provider is that it borrows the login the user
#' already has. Returning an empty string keeps the pipeline's unconditional
#' `get_api_key()` call harmless, and the check that actually matters, whether
#' the binary exists, happens in the builder.
#'
#' @noRd
method(get_api_key, api_claude_cli) <- function(.api, .dry_run = FALSE) ""

#' Flatten a conversation into the single prompt the CLI takes
#'
#' `claude -p` accepts one prompt, not a message list, so a multi-turn history
#' has to be written out as text. Turns are labelled because without labels a
#' two-turn history reads as one run-on user message and the model loses track
#' of who said what.
#'
#' With `.stateful = TRUE` only the newest user turn is sent and the CLI holds
#' the rest, which is why the builder passes `.history = FALSE` there.
#'
#' @noRd
method(to_api_format, list(LLMMessage, api_claude_cli)) <- function(.llm,
                                                                    .api,
                                                                    .history = TRUE) {
  turns <- filter_roles(.llm@message_history, c("user", "assistant"))

  if (!isTRUE(.history)) {
    user_turns <- Filter(function(m) identical(m$role, "user"), turns)
    if (length(user_turns) == 0) {
      stop("There is no user message to send to the Claude CLI.", call. = FALSE)
    }
    return(format_message(user_turns[[length(user_turns)]])$content)
  }

  if (length(turns) == 1) return(format_message(turns[[1]])$content)

  labelled <- vapply(turns, function(m) {
    label <- if (identical(m$role, "assistant")) "Assistant" else "User"
    paste0(label, ": ", format_message(m)$content)
  }, character(1))

  paste(labelled, collapse = "\n\n")
}

#' The command line a request runs, and what `.dry_run` hands back
#'
#' A `tidyllm_cli_command` rather than a bare character vector so that printing
#' it shows the command a user could paste into a terminal, which is the CLI
#' equivalent of inspecting an httr2 request. `args` stays a vector: the command
#' is never run through a shell, so nothing here is ever re-parsed and there is
#' no quoting to get wrong.
#'
#' @noRd
new_cli_command <- function(.binary, .args, .stdin = NULL) {
  structure(
    list(binary = .binary, args = .args, stdin = .stdin),
    class = "tidyllm_cli_command"
  )
}

#' @export
print.tidyllm_cli_command <- function(x, ...) {
  cat("<tidyllm CLI command>\n")
  cat(paste(c(x$binary, x$args), collapse = " "), "\n")
  if (!is.null(x$stdin)) {
    preview <- substr(x$stdin, 1, 400)
    cat("\n-- prompt on stdin ------------------------------------------\n")
    cat(preview)
    if (nchar(x$stdin) > 400) cat("\n[... ", nchar(x$stdin) - 400, " more characters]", sep = "")
    cat("\n")
  }
  invisible(x)
}

#' Run the CLI to completion and hand back its terminal event
#'
#' The output is drained while the child runs rather than read at the end. A
#' pipe holds only a fixed number of bytes, so a reply larger than the buffer
#' deadlocks a wait-then-read: the child blocks writing, the parent blocks
#' waiting, and neither moves. Long answers are exactly the case this provider
#' is for.
#'
#' @noRd
claude_cli_run_blocking <- function(.command, .timeout) {
  proc     <- claude_cli_start_process(.command)
  deadline <- Sys.time() + .timeout
  out      <- character(0)
  err      <- character(0)

  repeat {
    proc$poll_io(250)
    out <- c(out, proc$read_output_lines())
    err <- c(err, proc$read_error_lines())

    if (!proc$is_alive() && !proc$is_incomplete_output()) break

    if (Sys.time() > deadline) {
      proc$kill()
      stop(sprintf("The Claude CLI produced no result within %g seconds; giving up.", .timeout),
           call. = FALSE)
    }
  }
  out <- c(out, proc$read_all_output_lines())

  if (!length(out) || !nzchar(paste(out, collapse = ""))) {
    stop(glue::glue(
      "The Claude CLI exited with status {proc$get_exit_status()} and produced no output.\n",
      "{substr(paste(err, collapse = '\n'), 1, 500)}"
    ), call. = FALSE)
  }

  events <- jsonlite::fromJSON(paste(out, collapse = "\n"), simplifyVector = FALSE,
                               simplifyDataFrame = FALSE)
  claude_cli_result_event(events)
}

#' Pick the one event that is the response
#'
#' `--output-format json` returns an array of events: session setup, rate limit
#' reporting, the assistant turns, and last a terminal object carrying the reply
#' and every number the metadata needs. Only that last one is the response. The
#' July 2026 note describing a single flat object was wrong for CLI 2.1.273, and
#' a session that trusted it would read the reply off an init event.
#'
#' @noRd
claude_cli_result_event <- function(.events) {
  if (!is.null(.events$type) && identical(.events$type, "result")) return(.events)

  hits <- Filter(function(e) identical(e$type, "result"), .events)
  if (length(hits) == 0) {
    stop("The Claude CLI returned no result event. The output was not in the shape `--output-format json` documents.",
         call. = FALSE)
  }
  hits[[length(hits)]]
}

#' @noRd
method(parse_chat_response, list(api_claude_cli, class_list)) <- function(.api, .content) {
  if (isTRUE(.content$is_error)) {
    stop(glue::glue("Claude CLI error ({.content$subtype %||% 'unknown'}): {.content$result %||% ''}"),
         call. = FALSE)
  }
  .content$result
}

#' @noRd
method(extract_metadata, list(api_claude_cli, class_list)) <- function(.api, .response) {
  usage <- .response$usage %||% list()
  model <- names(.response$modelUsage %||% list())

  list(
    model             = if (length(model)) model[[1]] else NA_character_,
    timestamp         = lubridate::as_datetime(lubridate::now()),
    prompt_tokens     = usage$input_tokens %||% NA_integer_,
    completion_tokens = usage$output_tokens %||% NA_integer_,
    total_tokens      = (usage$input_tokens %||% 0) + (usage$output_tokens %||% 0),
    cached_tokens         = usage$cache_read_input_tokens %||% NA_integer_,
    cache_creation_tokens = usage$cache_creation_input_tokens %||% NA_integer_,
    stream            = FALSE,
    specific_metadata = list(
      session_id        = .response$session_id %||% NA_character_,
      stop_reason       = .response$stop_reason %||% NA_character_,
      total_cost_usd    = .response$total_cost_usd %||% NA_real_,
      num_turns         = .response$num_turns %||% NA_integer_,
      duration_ms       = .response$duration_ms %||% NA_integer_,
      structured_output = .response$structured_output
    )
  )
}

#' The CLI streams the Anthropic events wrapped one per line
#'
#' `--output-format stream-json` emits newline-delimited JSON. Most lines are
#' the API's own streaming events under `$event`; the terminal line is the same
#' result object the blocking call returns, which is why it is the only one kept
#' and why `assemble_stream_body()` below has nothing to assemble.
#'
#' @noRd
method(parse_stream_event, api_claude_cli) <- function(.api, .chunk) {
  event <- parse_stream_json(.chunk[[1]])
  if (is.null(event)) return(stream_event())

  if (identical(event$type, "result")) {
    if (isTRUE(event$is_error)) {
      return(stream_event(kind = "error",
                          error = event$result %||% event$subtype %||% "unknown error"))
    }
    return(stream_event(kind = "done", done = TRUE, keep = TRUE, event = event))
  }

  if (!identical(event$type, "stream_event")) return(stream_event())

  inner <- event$event
  if (!identical(inner$type, "content_block_delta")) return(stream_event())

  delta <- inner$delta
  switch(
    delta$type %||% "",
    text_delta     = stream_event(kind = "text",     text = delta$text),
    thinking_delta = stream_event(kind = "thinking", text = NULL),
    stream_event()
  )
}

#' The stream's terminal event is already the response body
#'
#' Every other provider has to fold its deltas back together here. The CLI sends
#' the finished result object as its last line, identical to what the blocking
#' call returns, so the two paths meet with nothing to reconcile.
#'
#' @noRd
method(assemble_stream_body, list(api_claude_cli, class_list)) <- function(.api, .events) {
  if (length(.events) == 0) return(NULL)
  claude_cli_result_event(.events)
}

#' Start the CLI as a child process for `send_chat()`
#'
#' The process is the stream. There are no headers and no HTTP status, so both
#' come back empty; `track_rate_limit()` reads headers through
#' `ratelimit_from_header()`, whose `APIProvider` default returns NULL, so an
#' empty list is the honest answer rather than a gap.
#'
#' @noRd
method(open_chat_stream, api_claude_cli) <- function(.api, .built) {
  list(
    response = claude_cli_start_process(.built$request),
    headers  = list(),
    status   = 200L
  )
}

#' @noRd
claude_cli_start_process <- function(.command) {
  check_processx_installed()
  proc <- processx::process$new(
    command = .command$binary,
    args    = .command$args,
    stdin   = if (is.null(.command$stdin)) NULL else "|",
    stdout  = "|",
    stderr  = "|"
  )
  if (!is.null(.command$stdin)) {
    proc$write_input(paste0(.command$stdin, "\n"))
    proc$get_input_connection() |> close()
  }
  proc
}

#' Run a non-streaming CLI call in the background of the session
#'
#' The httr2 default cannot serve this: there is no request to promise. A child
#' process is already non-blocking, so the driver only has to look in on it. The
#' poll interval is a compromise: short enough that a quick answer is not left
#' sitting, long enough that a two-minute agent run costs a few hundred wake-ups
#' rather than a few thousand.
#'
#' @noRd
method(start_async_request, api_claude_cli) <- function(.api, .job) {
  check_later_installed()
  env <- .job$env
  proc <- claude_cli_start_process(env$built$request)

  env$headers     <- list()
  env$http_status <- 200L
  env$cancel_fn   <- function() if (proc$is_alive()) proc$kill()

  poll <- function() {
    if (!identical(env$status, "running")) {
      if (proc$is_alive()) proc$kill()
      return(invisible(NULL))
    }
    if (proc$is_alive()) {
      later::later(poll, delay = 0.1)
      return(invisible(NULL))
    }

    outcome <- tryCatch({
      out <- proc$read_all_output_lines()
      if (!length(out) || !nzchar(paste(out, collapse = ""))) {
        stop(sprintf("The Claude CLI exited with status %s and produced no output.",
                     proc$get_exit_status()), call. = FALSE)
      }
      events <- jsonlite::fromJSON(paste(out, collapse = "\n"),
                                   simplifyVector = FALSE, simplifyDataFrame = FALSE)
      claude_cli_result_event(events)
    }, error = function(e) e)

    if (inherits(outcome, "condition")) {
      env$status <- "error"
      env$error  <- outcome
      return(invisible(NULL))
    }
    chat_job_finish(.job, outcome)
  }

  later::later(poll, delay = 0.05)
  invisible(NULL)
}

#' Chat with Claude through your own installed Claude CLI
#'
#' `claude_cli()` is the odd one out among tidyllm's providers: it sends nothing
#' over the network itself. It runs the `claude` command line tool that is
#' already installed and signed in on your machine, and reads its JSON output
#' back. There is no API key to set, and usage counts against whatever plan the
#' CLI is logged in to rather than against an Anthropic API key.
#'
#' `claude_cli()` finds the CLI on the PATH, and, failing that, in the directories
#' the installers write to. RStudio and other GUI front ends do not inherit the
#' PATH from your shell profile, so the CLI can be perfectly installed and still
#' invisible to `Sys.which()`; the fallback is what covers that. To point at it
#' explicitly, set `options(tidyllm_claude_cli_path = "/path/to/claude")`.
#'
#' The CLI is an agent, not a plain completion endpoint. By default it can read
#' files, edit them and run shell commands. tidyllm turns all of that off unless
#' you ask for it, because a call to `chat()` that quietly edits files in the
#' working directory is not what the rest of this package does. Pass
#' `.cli_tools` to allow specific tools back.
#'
#' @param .llm An `LLMMessage` object.
#' @param .model Character; the model the CLI should use, for example
#'   "claude-sonnet-5" or "claude-haiku-4-5-20251001". Default NULL uses whatever
#'   the CLI is configured to use.
#' @param .system_prompt_mode One of "append" or "ignore". With "append" the
#'   message's system prompt is added to the CLI's own system prompt, which is
#'   the only way the CLI accepts one. With "ignore" it is dropped.
#' @param .json_schema A schema to enforce an output structure; a list, or an
#'   ellmer type object. Passed to the CLI's own `--json-schema` flag.
#' @param .cli_tools Controls the CLI's built-in tools. FALSE, the default,
#'   disables all of them, so the call behaves like an ordinary completion.
#'   A character vector allows exactly those tools, for example
#'   `c("Read", "WebSearch")`. TRUE hands over to the CLI's own configuration,
#'   which on a default install includes Bash, Write and Edit.
#' @param .stateful Logical; if TRUE the CLI keeps the conversation on its side.
#'   The first call sends only the newest user message and records the session
#'   id in the metadata; later calls resume that session instead of replaying
#'   the history. Default FALSE sends the whole conversation every time, the way
#'   every other tidyllm provider does.
#' @param .session_id Character; resume this CLI session explicitly. Normally
#'   left NULL, because `.stateful = TRUE` picks the id up from the message
#'   history on its own.
#' @param .max_budget_usd Numeric; hand the CLI a spending ceiling for this call.
#' @param .append_system_prompt Character; extra system prompt text, added after
#'   the message's own system prompt.
#' @param .binary Character; the command to run, or a full path to it. Defaults
#'   to "claude", which is looked for on the PATH and then in the places the
#'   installers use. A GUI R session often has a shorter PATH than your terminal,
#'   so if the CLI is somewhere unusual, set it once with
#'   `options(tidyllm_claude_cli_path = "/path/to/claude")` in your `.Rprofile`,
#'   or the `TIDYLLM_CLAUDE_CLI` environment variable, rather than passing it at
#'   every call.
#' @param .verbose Logical; if TRUE, prints rate limit information after the
#'   response.
#' @param .timeout Integer; seconds to wait. On a stream this is the idle
#'   deadline between events rather than a total, so a long answer is not cut
#'   off for being long.
#' @param .stream Logical; if TRUE, prints the reply as it arrives.
#' @param .dry_run Logical; if TRUE, returns the command that would be run
#'   instead of running it.
#'
#' @return A new `LLMMessage` object containing the original messages plus the
#'   CLI's response.
#' @examples
#' \dontrun{
#' llm_message("What is R's S7 class system?") |>
#'   chat(claude_cli())
#'
#' # Let the CLI read files, but not write or run anything
#' llm_message("Summarise the DESCRIPTION file in this folder.") |>
#'   chat(claude_cli(.cli_tools = c("Read", "Glob")))
#'
#' # Keep the conversation on the CLI's side
#' first  <- llm_message("Start a review of my package.") |>
#'   chat(claude_cli(.stateful = TRUE))
#' second <- first |>
#'   llm_message("Now look at the tests.") |>
#'   chat(claude_cli(.stateful = TRUE))
#' }
#'
#' @export
claude_cli_chat <- function(.llm,
                            .model = NULL,
                            .system_prompt_mode = "append",
                            .json_schema = NULL,
                            .cli_tools = FALSE,
                            .stateful = FALSE,
                            .session_id = NULL,
                            .max_budget_usd = NULL,
                            .append_system_prompt = NULL,
                            .binary = "claude",
                            .verbose = FALSE,
                            .timeout = 300,
                            .stream = FALSE,
                            .dry_run = FALSE) {
  built <- do.call(claude_cli_build_chat_request, mget(names(formals())), quote = TRUE)
  run_chat_pipeline(built, .dry_run)
}

#' Build a Claude CLI chat request without running it
#'
#' @noRd
claude_cli_build_chat_request <- function(.llm,
                            .model = NULL,
                            .system_prompt_mode = "append",
                            .json_schema = NULL,
                            .cli_tools = FALSE,
                            .stateful = FALSE,
                            .session_id = NULL,
                            .max_budget_usd = NULL,
                            .append_system_prompt = NULL,
                            .binary = "claude",
                            .verbose = FALSE,
                            .timeout = 300,
                            .stream = FALSE,
                            .dry_run = FALSE) {
  c(
    ".llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
    ".model must be a single string if provided" =
      is.null(.model) || (is.character(.model) && length(.model) == 1),
    ".system_prompt_mode must be \"append\" or \"ignore\"" =
      is.character(.system_prompt_mode) && length(.system_prompt_mode) == 1 &&
      .system_prompt_mode %in% c("append", "ignore"),
    ".json_schema must be NULL or a list or an ellmer type object" =
      is.null(.json_schema) | is.list(.json_schema) | is_ellmer_type(.json_schema),
    ".cli_tools must be TRUE, FALSE, or a character vector of tool names" =
      (is.logical(.cli_tools) && length(.cli_tools) == 1) || is.character(.cli_tools),
    ".stateful must be logical" = is.logical(.stateful) && length(.stateful) == 1,
    ".session_id must be a single string if provided" =
      is.null(.session_id) || (is.character(.session_id) && length(.session_id) == 1),
    ".max_budget_usd must be a positive number if provided" =
      is.null(.max_budget_usd) || (is.numeric(.max_budget_usd) && .max_budget_usd > 0),
    ".append_system_prompt must be a single string if provided" =
      is.null(.append_system_prompt) || (is.character(.append_system_prompt) && length(.append_system_prompt) == 1),
    ".binary must be a single string" = is.character(.binary) && length(.binary) == 1,
    ".verbose must be logical" = is.logical(.verbose),
    ".timeout must be an integer-valued numeric (seconds till timeout)" = is_integer_valued(.timeout),
    ".stream must be logical" = is.logical(.stream),
    ".dry_run must be logical" = is.logical(.dry_run)
  ) |>
    validate_inputs()

  api_obj <- api_claude_cli(
    short_name       = "claude_cli",
    long_name        = "Claude CLI",
    api_key_env_var  = "",
    stream_transport = "process"
  )

  json <- FALSE
  schema_arg <- NULL
  if (!is.null(.json_schema)) {
    json <- TRUE
    if (requireNamespace("ellmer", quietly = TRUE)) {
      if (S7_inherits(.json_schema, ellmer::TypeObject)) .json_schema <- to_schema(.json_schema)
    }
    schema_arg <- jsonlite::toJSON(.json_schema, auto_unbox = TRUE, null = "null")
  }

  # A session id already in the history is what makes the second `.stateful`
  # call a continuation rather than a fresh conversation.
  resume_id <- .session_id
  if (isTRUE(.stateful) && is.null(resume_id)) {
    resume_id <- claude_cli_last_session_id(.llm)
  }

  # Only a resumed session may drop the history: without one the CLI has no
  # record of the conversation, and sending the newest turn alone would silently
  # lose everything said before it.
  send_history <- !(isTRUE(.stateful) && !is.null(resume_id))
  prompt <- to_api_format(.llm, api_obj, .history = send_history)

  system_prompt <- NULL
  if (identical(.system_prompt_mode, "append")) {
    parts <- c(.llm@system_prompt, .append_system_prompt)
    parts <- parts[!is.na(parts) & nzchar(parts)]
    # tidyllm's default system prompt says the assistant is helpful, which the
    # CLI's own prompt already covers. Passing it would only dilute it.
    parts <- setdiff(parts, "You are a helpful assistant")
    if (length(parts)) system_prompt <- paste(parts, collapse = "\n\n")
  }

  args <- c("-p", "--output-format", if (isTRUE(.stream)) "stream-json" else "json")
  if (isTRUE(.stream)) args <- c(args, "--include-partial-messages", "--verbose")
  if (!is.null(.model))          args <- c(args, "--model", .model)
  if (!is.null(system_prompt))   args <- c(args, "--append-system-prompt", system_prompt)
  if (!is.null(schema_arg))      args <- c(args, "--json-schema", schema_arg)
  if (!is.null(.max_budget_usd)) args <- c(args, "--max-budget-usd", format(.max_budget_usd))
  if (!is.null(resume_id))       args <- c(args, "--resume", resume_id)
  args <- c(args, claude_cli_tool_args(.cli_tools))

  # The prompt goes on stdin rather than in the argument vector. A long
  # conversation flattened into one prompt can run to tens of thousands of
  # characters, and an argument vector has an operating-system size limit that a
  # long chat history would eventually hit.
  command <- new_cli_command(claude_cli_binary(.binary), args, .stdin = prompt)

  new_chat_request(
    .request    = command,
    .api        = api_obj,
    .llm        = .llm,
    .body       = NULL,
    .tools_def  = NULL,
    .json       = json,
    .mode       = if (isTRUE(.stream)) "stream" else "value",
    .timeout    = .timeout,
    .max_tries  = 1,
    .verbose    = .verbose,
    .perform_fn = claude_cli_performer()
  )
}

#' Turn `.cli_tools` into the flags that allow or forbid the CLI's own tools
#'
#' @noRd
claude_cli_tool_args <- function(.cli_tools) {
  if (isTRUE(.cli_tools)) return(character(0))
  if (is.character(.cli_tools) && length(.cli_tools)) {
    return(c("--allowed-tools", paste(.cli_tools, collapse = ",")))
  }
  # An empty allow-list is what the CLI reads as "nothing is permitted"; the
  # write-capable tools are named as well so that a future CLI default cannot
  # quietly widen what a plain `chat()` call may do to the filesystem.
  c("--allowed-tools", "",
    "--disallowed-tools", "Bash,Write,Edit,NotebookEdit,WebFetch,WebSearch,Task")
}

#' The most recent CLI session id in a conversation's metadata
#'
#' @noRd
claude_cli_last_session_id <- function(.llm) {
  history <- .llm@message_history
  for (i in rev(seq_along(history))) {
    meta <- history[[i]]$meta
    sid  <- meta$specific_metadata$session_id
    if (!is.null(sid) && !is.na(sid) && nzchar(sid)) return(sid)
  }
  NULL
}

#' Run one round, blocking or streaming, and interpret it
#'
#' A closure on the built request rather than a branch inside
#' `perform_chat_request()`, because that function is written against httr2 from
#' its first line. `new_chat_request(.perform_fn =)` is the seam the pipeline
#' already provides for a provider that performs its own requests.
#'
#' @noRd
claude_cli_performer <- function() {
  function(.built) {
    if (chat_request_streams(.built)) {
      if (!isTRUE(.built$quiet)) {
        message("\n---------\nStart ", .built$api@long_name, " streaming: \n---------\n")
      }
      proc <- claude_cli_start_process(.built$request)
      stream_response <- handle_stream(.built$api, proc,
                                       .on_chunk     = .built$on_chunk,
                                       .idle_timeout = .built$timeout,
                                       .verbose      = !isTRUE(.built$quiet))
      content <- assemble_stream_body(.built$api, stream_response$raw_data)
      interpreted <- interpret_chat_response(
        .built$api,
        list(content = content, headers = list(), status = 200L)
      )
      interpreted$meta$stream <- TRUE
      return(interpreted)
    }

    interpret_chat_response(
      .built$api,
      list(
        content = claude_cli_run_blocking(.built$request, .built$timeout),
        headers = list(),
        status  = 200L
      )
    )
  }
}

#' Chat through a locally installed Claude CLI
#'
#' `claude_cli()` routes a chat to the `claude` command line tool installed on
#' your own machine, using the login it already has. It takes no API key. See
#' [claude_cli_chat()] for the arguments and for what happens to the CLI's own
#' file and shell tools.
#'
#' @param ... Arguments passed to the CLI chat function.
#' @param .called_from Internal; the verb that dispatched here.
#'
#' @return The result of the requested action; for `chat()`, an updated
#'   `LLMMessage`.
#'
#' @examples
#' \dontrun{
#' llm_message("Explain R's S7 classes in three sentences.") |>
#'   chat(claude_cli())
#' }
#'
#' @export
claude_cli <- create_provider_function(
  .name = "claude_cli",
  chat  = claude_cli_chat,
  build = claude_cli_build_chat_request
)

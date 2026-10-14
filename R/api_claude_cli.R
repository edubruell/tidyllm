#' @noRd
api_claude_cli <- new_class("ClaudeCLI", APIProvider)

#' @noRd
claude_cli_binary <- function(.binary = "claude") {
  cli_binary(
    .binary,
    .option       = "tidyllm_claude_cli_path",
    .env_var      = "TIDYLLM_CLAUDE_CLI",
    .fn           = "claude_cli()",
    .install_hint = "see https://docs.claude.com/en/docs/claude-code, then run `claude` once to sign in.",
    .extra_dirs   = file.path(path.expand("~"), ".claude", "local")
  )
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

#' @noRd
method(to_api_format, list(LLMMessage, api_claude_cli)) <- function(.llm,
                                                                    .api,
                                                                    .history = TRUE) {
  cli_prompt_text(.llm, .history, .label = "the Claude CLI")
}

#' @noRd
claude_cli_read_output <- function(.lines) {
  events <- jsonlite::fromJSON(paste(.lines, collapse = "\n"), simplifyVector = FALSE,
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
    response = cli_start_process(.built$request, "claude_cli()"),
    headers  = list(),
    status   = 200L
  )
}

#' @noRd
method(start_async_request, api_claude_cli) <- function(.api, .job) {
  cli_start_async(.api, .job, claude_cli_read_output)
}

#' Chat with Claude through your own installed Claude CLI
#'
#' `claude_cli()`, like `codex_cli()` and `vibe_cli()`, sends nothing over the
#' network itself. It runs the `claude` command line tool that is
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
#'   "claude-sonnet-5-5" or "claude-haiku-4-5-20251001". Default NULL uses whatever
#'   the CLI is configured to use.
#' @param .system_prompt_mode One of "append" or "ignore". With "append" the
#'   message's system prompt is added to the CLI's own system prompt, which is
#'   the only way the CLI accepts one. With "ignore" it is dropped.
#' @param .json_schema A schema to enforce an output structure; a list, or an
#'   ellmer type object. Passed to the CLI's own `--json-schema` flag.
#' @param .cli_tools Controls the CLI's own tools. FALSE, the default,
#'   removes all of them and leaves out your MCP servers, so the call behaves
#'   like an ordinary completion and cannot read files. A character vector
#'   makes exactly those built-in tools available, for example
#'   `c("Read", "Glob")`; MCP tools named there still work. TRUE hands over to
#'   the CLI's own configuration, which on a default install includes Bash,
#'   Write and Edit.
#' @param .stateful Logical; if TRUE the CLI keeps the conversation on its side.
#'   The first call sends the conversation and records the session id in the
#'   metadata; later calls resume that session and send only the newest user
#'   message instead of replaying the history. Default FALSE sends the whole conversation every time, the way
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
    resume_id <- cli_last_session_id(.llm)
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
  if (!is.null(.max_budget_usd)) args <- c(args, "--max-budget-usd", format(.max_budget_usd, scientific = FALSE))
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
    .perform_fn = cli_performer(claude_cli_read_output)
  )
}

#' Turn `.cli_tools` into the flags that allow or forbid the CLI's own tools
#'
#' @noRd
claude_cli_tool_args <- function(.cli_tools) {
  if (isTRUE(.cli_tools)) return(character(0))
  if (is.character(.cli_tools) && length(.cli_tools)) {
    tools <- paste(.cli_tools, collapse = ",")
    return(c("--tools", tools, "--allowed-tools", tools))
  }
  c("--tools", "", "--strict-mcp-config",
    "--disallowed-tools", "Bash,Write,Edit,NotebookEdit,WebFetch,WebSearch,Task")
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
  build = claude_cli_build_chat_request,
  .media = character()
)

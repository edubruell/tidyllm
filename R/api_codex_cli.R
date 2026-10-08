#' @noRd
api_codex_cli <- new_class("CodexCLI", APIProvider)

#' Features that give Codex tools; turned off unless `.cli_tools = TRUE`
#'
#' Set through `-c features.<name>=false` rather than `--disable`, because
#' `--disable` rejects a feature name the installed version does not know.
#'
#' @noRd
codex_cli_tool_features <- c(
  "shell_tool", "unified_exec", "browser_use", "browser_use_external",
  "computer_use", "in_app_browser", "plugins", "apps", "skill_search",
  "tool_suggest", "sleep_tool", "goals"
)

#' @noRd
codex_cli_reasoning_efforts <- c("minimal", "low", "medium", "high", "xhigh")

#' @noRd
codex_cli_binary <- function(.binary = "codex") {
  cli_binary(
    .binary,
    .option       = "tidyllm_codex_cli_path",
    .env_var      = "TIDYLLM_CODEX_CLI",
    .fn           = "codex_cli()",
    .install_hint = "run `npm install -g @openai/codex`, then `codex login` or use `.use_api_key = TRUE`."
  )
}

#' @noRd
method(get_api_key, api_codex_cli) <- function(.api, .dry_run = FALSE) ""

#' @noRd
method(to_api_format, list(LLMMessage, api_codex_cli)) <- function(.llm,
                                                                   .api,
                                                                   .history = TRUE) {
  cli_prompt_text(.llm, .history, .label = "the Codex CLI")
}

#' Fold the JSON Lines events of one `codex exec` run into a response body
#'
#' Codex can send several agent messages in one turn, for example a short
#' preamble before the answer. The reply is the last one. Events of type
#' `error` are retries Codex reports while it reconnects; only `turn.failed`
#' ends the run with an error.
#'
#' @noRd
codex_cli_body <- function(.events) {
  body <- list(session_id = NA_character_, reply = NULL, usage = NULL, error = NULL)
  for (event in .events) {
    type <- event[["type"]] %||% ""
    if (identical(type, "thread.started")) body$session_id <- event[["thread_id"]] %||% NA_character_
    if (identical(type, "item.completed") &&
        identical(event[["item"]][["type"]], "agent_message")) {
      body$reply <- event[["item"]][["text"]]
    }
    if (identical(type, "turn.completed")) body$usage <- event[["usage"]]
    if (identical(type, "turn.failed")) {
      body$error <- event[["error"]][["message"]] %||% "unknown error"
    }
  }
  body
}

#' @noRd
codex_cli_read_output <- function(.lines) {
  .lines <- .lines[nzchar(.lines)]
  events <- lapply(.lines, parse_stream_json)
  codex_cli_body(Filter(Negate(is.null), events))
}

#' @noRd
codex_cli_error_message <- function(.error) {
  hint <- ""
  if (grepl("401|unauthorized", .error, ignore.case = TRUE)) {
    hint <- paste0(
      "\nCodex is not signed in, or its login has expired. Run `codex login` in a terminal, ",
      "or use codex_cli(.use_api_key = TRUE) to pay with your OPENAI_API_KEY instead."
    )
  }
  paste0("Codex CLI error: ", .error, hint)
}

#' @noRd
method(parse_chat_response, list(api_codex_cli, class_list)) <- function(.api, .content) {
  if (!is.null(.content$error)) stop(codex_cli_error_message(.content$error), call. = FALSE)
  if (is.null(.content$reply)) {
    stop("The Codex CLI finished without a reply.", call. = FALSE)
  }
  .content$reply
}

#' @noRd
method(extract_metadata, list(api_codex_cli, class_list)) <- function(.api, .response) {
  usage <- .response$usage %||% list()
  list(
    model                 = .response$model %||% NA_character_,
    timestamp             = lubridate::as_datetime(lubridate::now()),
    prompt_tokens         = usage$input_tokens %||% NA_integer_,
    completion_tokens     = usage$output_tokens %||% NA_integer_,
    total_tokens          = (usage$input_tokens %||% 0) + (usage$output_tokens %||% 0),
    cached_tokens         = usage$cached_input_tokens %||% NA_integer_,
    cache_creation_tokens = usage$cache_write_input_tokens %||% NA_integer_,
    stream                = FALSE,
    specific_metadata = list(
      session_id       = .response$session_id %||% NA_character_,
      reasoning_tokens = usage$reasoning_output_tokens %||% NA_integer_
    )
  )
}

#' @noRd
method(parse_stream_event, api_codex_cli) <- function(.api, .chunk) {
  event <- parse_stream_json(.chunk[[1]])
  if (is.null(event)) return(stream_event())
  type <- event[["type"]] %||% ""

  if (identical(type, "turn.failed")) {
    return(stream_event(kind = "error",
                        error = codex_cli_error_message(event[["error"]][["message"]] %||% "unknown error")))
  }
  if (identical(type, "turn.completed")) {
    return(stream_event(kind = "done", done = TRUE, keep = TRUE, event = event))
  }
  if (identical(type, "thread.started")) return(stream_event(keep = TRUE, event = event))
  if (identical(type, "item.completed") &&
      identical(event[["item"]][["type"]], "agent_message")) {
    return(stream_event(kind = "text", text = event[["item"]][["text"]], keep = TRUE, event = event))
  }
  stream_event()
}

#' @noRd
method(assemble_stream_body, list(api_codex_cli, class_list)) <- function(.api, .events) {
  if (length(.events) == 0) return(NULL)
  codex_cli_body(.events)
}

#' @noRd
method(open_chat_stream, api_codex_cli) <- function(.api, .built) {
  list(response = cli_start_process(.built$request, "codex_cli()"), headers = list(), status = 200L)
}

#' @noRd
method(start_async_request, api_codex_cli) <- function(.api, .job) {
  cli_start_async(.api, .job, codex_cli_read_output)
}

#' Chat with OpenAI models through your own installed Codex CLI
#'
#' `codex_cli()` runs the `codex` command line tool installed on your machine and
#' reads its JSON output back. It uses the login Codex already has, for example
#' a ChatGPT plan, or your OpenAI API key if you set `.use_api_key = TRUE`.
#'
#' Codex is an agent, not a plain completion endpoint. By default it can run
#' shell commands and use a browser. tidyllm turns those tools off unless you set
#' `.cli_tools = TRUE`, and even then Codex runs in its read-only sandbox unless
#' you choose another with `.sandbox`.
#'
#' Codex adds its own instructions to every call, so even a one-line prompt sends
#' about 12,000 input tokens. The token counts in the metadata include them.
#'
#' @param .llm An `LLMMessage` object.
#' @param .model Character; the model Codex should use. Default NULL uses the
#'   model Codex is configured with. The metadata reports the model only when you
#'   set one here, because Codex does not include it in its output.
#' @param .reasoning_effort Character; how much the model reasons before it
#'   answers: "minimal", "low", "medium", "high" or "xhigh". Default NULL uses
#'   Codex's own setting.
#' @param .system_prompt_mode One of "append" or "ignore". With "append" the
#'   message's system prompt is passed to Codex as developer instructions, next
#'   to Codex's own instructions. With "ignore" it is dropped.
#' @param .json_schema A schema to enforce an output structure; a list, or an
#'   ellmer type object. Passed to Codex's `--output-schema` flag.
#' @param .cli_tools Logical; FALSE, the default, turns off Codex's shell,
#'   browser, plugin and web search tools, so the call behaves like an ordinary
#'   completion. TRUE hands over to Codex's own configuration.
#' @param .sandbox Character; what commands Codex runs may do when `.cli_tools =
#'   TRUE`. "read-only" (the default) or "workspace-write", which lets Codex
#'   change files in the working directory.
#' @param .stateful Logical; if TRUE, Codex keeps the conversation on its side.
#'   The first call sends the conversation and records the session id in the
#'   metadata; later calls resume that session and send only the newest user
#'   message instead of replaying the history. Default FALSE sends the whole conversation every time and
#'   saves no session files.
#' @param .session_id Character; resume this Codex session explicitly. Normally
#'   left NULL, because `.stateful = TRUE` picks the id up from the message
#'   history on its own.
#' @param .use_api_key Logical; if TRUE, passes your `OPENAI_API_KEY` to Codex,
#'   so the call is billed to your API key instead of the login Codex has.
#'   Default FALSE.
#' @param .binary Character; the command to run, or a full path to it. Defaults
#'   to "codex", which is looked for on the PATH and then in the places the
#'   installers use. If R does not find it, set
#'   `options(tidyllm_codex_cli_path = "/path/to/codex")` in your `.Rprofile`, or
#'   the `TIDYLLM_CODEX_CLI` environment variable.
#' @param .verbose Logical; if TRUE, prints rate limit information after the
#'   response.
#' @param .timeout Integer; seconds to wait. On a stream this is the idle
#'   deadline between events rather than a total.
#' @param .stream Logical; if TRUE, prints each message as Codex finishes it.
#'   Codex sends whole messages, not word by word.
#' @param .dry_run Logical; if TRUE, returns the command that would be run
#'   instead of running it.
#'
#' @return A new `LLMMessage` object containing the original messages plus the
#'   reply from Codex.
#' @examples
#' \dontrun{
#' llm_message("What is R's S7 class system?") |>
#'   chat(codex_cli())
#'
#' # Pay with your OpenAI API key instead of a ChatGPT login
#' llm_message("Explain vapply() in two sentences.") |>
#'   chat(codex_cli(.use_api_key = TRUE, .reasoning_effort = "low"))
#' }
#'
#' @export
codex_cli_chat <- function(.llm,
                           .model = NULL,
                           .reasoning_effort = NULL,
                           .system_prompt_mode = "append",
                           .json_schema = NULL,
                           .cli_tools = FALSE,
                           .sandbox = "read-only",
                           .stateful = FALSE,
                           .session_id = NULL,
                           .use_api_key = FALSE,
                           .binary = "codex",
                           .verbose = FALSE,
                           .timeout = 300,
                           .stream = FALSE,
                           .dry_run = FALSE) {
  built <- do.call(codex_cli_build_chat_request, mget(names(formals())), quote = TRUE)
  run_chat_pipeline(built, .dry_run)
}

#' Build a Codex CLI chat request without running it
#'
#' @noRd
codex_cli_build_chat_request <- function(.llm,
                                         .model = NULL,
                                         .reasoning_effort = NULL,
                                         .system_prompt_mode = "append",
                                         .json_schema = NULL,
                                         .cli_tools = FALSE,
                                         .sandbox = "read-only",
                                         .stateful = FALSE,
                                         .session_id = NULL,
                                         .use_api_key = FALSE,
                                         .binary = "codex",
                                         .verbose = FALSE,
                                         .timeout = 300,
                                         .stream = FALSE,
                                         .dry_run = FALSE) {
  c(
    ".llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
    ".model must be a single string if provided" =
      is.null(.model) || (is.character(.model) && length(.model) == 1),
    ".reasoning_effort must be NULL or one of \"minimal\", \"low\", \"medium\", \"high\", \"xhigh\"" =
      is.null(.reasoning_effort) ||
      (is.character(.reasoning_effort) && length(.reasoning_effort) == 1 &&
         .reasoning_effort %in% codex_cli_reasoning_efforts),
    ".system_prompt_mode must be \"append\" or \"ignore\"" =
      is.character(.system_prompt_mode) && length(.system_prompt_mode) == 1 &&
      .system_prompt_mode %in% c("append", "ignore"),
    ".json_schema must be NULL or a list or an ellmer type object" =
      is.null(.json_schema) | is.list(.json_schema) | is_ellmer_type(.json_schema),
    ".cli_tools must be TRUE or FALSE" = is.logical(.cli_tools) && length(.cli_tools) == 1,
    ".sandbox must be \"read-only\" or \"workspace-write\"" =
      is.character(.sandbox) && length(.sandbox) == 1 &&
      .sandbox %in% c("read-only", "workspace-write"),
    ".stateful must be logical" = is.logical(.stateful) && length(.stateful) == 1,
    ".session_id must be a single string if provided" =
      is.null(.session_id) || (is.character(.session_id) && length(.session_id) == 1),
    ".use_api_key must be TRUE or FALSE" = is.logical(.use_api_key) && length(.use_api_key) == 1,
    ".binary must be a single string" = is.character(.binary) && length(.binary) == 1,
    ".verbose must be logical" = is.logical(.verbose),
    ".timeout must be an integer-valued numeric (seconds till timeout)" = is_integer_valued(.timeout),
    ".stream must be logical" = is.logical(.stream),
    ".dry_run must be logical" = is.logical(.dry_run)
  ) |>
    validate_inputs()

  api_obj <- api_codex_cli(
    short_name       = "codex_cli",
    long_name        = "Codex CLI",
    api_key_env_var  = "",
    stream_transport = "process"
  )

  env_from <- NULL
  if (isTRUE(.use_api_key)) {
    if (!nzchar(Sys.getenv("OPENAI_API_KEY")) && !isTRUE(.dry_run)) {
      stop("codex_cli(.use_api_key = TRUE) needs the OPENAI_API_KEY environment variable to be set.",
           call. = FALSE)
    }
    env_from <- c(CODEX_API_KEY = "OPENAI_API_KEY")
  }

  resume_id <- .session_id
  if (isTRUE(.stateful) && is.null(resume_id)) resume_id <- cli_last_session_id(.llm)
  prompt <- to_api_format(.llm, api_obj, .history = !(isTRUE(.stateful) && !is.null(resume_id)))

  instructions <- NULL
  if (identical(.system_prompt_mode, "append")) {
    parts <- .llm@system_prompt
    parts <- parts[!is.na(parts) & nzchar(parts) & parts != "You are a helpful assistant"]
    if (length(parts)) instructions <- paste(parts, collapse = "\n\n")
  }

  json <- !is.null(.json_schema)
  schema_file <- NULL
  if (json) {
    schema_file <- tempfile("tidyllm_codex_schema_", fileext = ".json")
    jsonlite::write_json(add_no_extra_fields(cli_schema(.json_schema)), schema_file,
                         auto_unbox = TRUE, null = "null")
  }

  args <- c("exec", "--json", "--skip-git-repo-check", "--sandbox", .sandbox)
  if (!isTRUE(.stateful))          args <- c(args, "--ephemeral")
  if (!is.null(.model))            args <- c(args, "--model", .model)
  if (!is.null(.reasoning_effort)) args <- c(args, "-c", paste0("model_reasoning_effort=", .reasoning_effort))
  if (!is.null(instructions)) {
    args <- c(args, "-c", paste0("developer_instructions=",
                                 jsonlite::toJSON(instructions, auto_unbox = TRUE)))
  }
  if (!is.null(schema_file))       args <- c(args, "--output-schema", schema_file)
  if (!isTRUE(.cli_tools)) {
    args <- c(args, "-c", "web_search=\"disabled\"",
              as.vector(rbind("-c", paste0("features.", codex_cli_tool_features, "=false"))))
  }
  if (!is.null(resume_id))         args <- c(args, "resume", resume_id)
  args <- c(args, "-")

  command <- new_cli_command(codex_cli_binary(.binary), args, .stdin = prompt, .env_from = env_from)

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
    .perform_fn = function(.built) {
      if (!is.null(schema_file)) on.exit(unlink(schema_file), add = TRUE)
      cli_performer(codex_cli_read_output)(.built)
    },
    .meta_fn    = function(meta, response) {
      if (!is.null(.model)) meta$model <- .model
      meta
    }
  )
}

#' Chat through a locally installed Codex CLI
#'
#' `codex_cli()` routes a chat to the `codex` command line tool installed on your
#' own machine. It uses the login Codex already has, or your OpenAI API key with
#' `.use_api_key = TRUE`. See [codex_cli_chat()] for the arguments and for what
#' happens to Codex's own shell and browser tools.
#'
#' @param ... Arguments passed to the Codex CLI chat function.
#' @param .called_from Internal; the verb that dispatched here.
#'
#' @return The result of the requested action; for `chat()`, an updated
#'   `LLMMessage`.
#'
#' @examples
#' \dontrun{
#' llm_message("Explain R's S7 classes in three sentences.") |>
#'   chat(codex_cli())
#' }
#'
#' @export
codex_cli <- create_provider_function(
  .name = "codex_cli",
  chat  = codex_cli_chat,
  build = codex_cli_build_chat_request,
  .media = character()
)

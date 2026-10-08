#' @noRd
api_vibe_cli <- new_class("VibeCLI", APIProvider)

#' @noRd
vibe_cli_binary <- function(.binary = "vibe") {
  cli_binary(
    .binary,
    .option       = "tidyllm_vibe_cli_path",
    .env_var      = "TIDYLLM_VIBE_CLI",
    .fn           = "vibe_cli()",
    .install_hint = "run `uv tool install mistral-vibe` and set MISTRAL_API_KEY."
  )
}

#' @noRd
method(get_api_key, api_vibe_cli) <- function(.api, .dry_run = FALSE) ""

#' @noRd
method(to_api_format, list(LLMMessage, api_vibe_cli)) <- function(.llm,
                                                                  .api,
                                                                  .history = TRUE) {
  cli_prompt_text(.llm, .history, .label = "Mistral Vibe")
}

#' Read the history Vibe prints with `--output json`
#'
#' The output is one JSON array of history entries: user and assistant
#' messages, reasoning, and tool effects. The reply is the text of the last
#' assistant message.
#'
#' @noRd
vibe_cli_read_output <- function(.lines) {
  start <- which(startsWith(trimws(.lines), "["))
  if (!length(start)) {
    stop("Mistral Vibe returned no JSON history. The output was not in the shape `--output json` documents.",
         call. = FALSE)
  }
  entries <- jsonlite::fromJSON(paste(.lines[start[[1]]:length(.lines)], collapse = "\n"),
                                simplifyVector = FALSE, simplifyDataFrame = FALSE)
  assistant <- Filter(function(e) identical(e[["type"]], "message") &&
                        identical(e[["role"]], "assistant"), entries)
  reply <- NULL
  if (length(assistant)) {
    parts <- Filter(function(p) identical(p[["type"]], "text"),
                    assistant[[length(assistant)]][["content"]])
    reply <- paste(vapply(parts, function(p) p[["text"]] %||% "", character(1)), collapse = "")
  }
  list(
    session_id = if (length(entries)) entries[[1]][["sessionId"]] %||% NA_character_ else NA_character_,
    reply      = reply,
    tool_calls = sum(vapply(entries, function(e) identical(e[["type"]], "effect"), logical(1)))
  )
}

#' @noRd
method(parse_chat_response, list(api_vibe_cli, class_list)) <- function(.api, .content) {
  if (is.null(.content$reply)) {
    stop("Mistral Vibe finished without a reply.", call. = FALSE)
  }
  .content$reply
}

#' @noRd
method(extract_metadata, list(api_vibe_cli, class_list)) <- function(.api, .response) {
  list(
    model             = NA_character_,
    timestamp         = lubridate::as_datetime(lubridate::now()),
    prompt_tokens     = NA_integer_,
    completion_tokens = NA_integer_,
    total_tokens      = NA_integer_,
    stream            = FALSE,
    specific_metadata = list(
      session_id = .response$session_id %||% NA_character_,
      tool_calls = .response$tool_calls %||% NA_integer_
    )
  )
}

#' @noRd
method(start_async_request, api_vibe_cli) <- function(.api, .job) {
  cli_start_async(.api, .job, vibe_cli_read_output)
}

#' Chat with Mistral models through your own installed Mistral Vibe CLI
#'
#' `vibe_cli()` runs Mistral's `vibe` command line tool installed on your machine
#' and reads its JSON output back. Vibe uses your `MISTRAL_API_KEY`, or the key
#' it saved during its own setup.
#'
#' Vibe is a coding agent, not a plain completion endpoint. By default tidyllm
#' runs it in Vibe's ask-first mode, in which every tool call needs an approval
#' that a non-interactive run cannot give, so Vibe can neither read nor change
#' files. If the model still tries a tool, the refusal costs a turn; `.max_turns`
#' caps how many it may spend. Set `.cli_tools = TRUE` to use Vibe's own
#' configuration instead.
#'
#' Vibe has no system prompt option, so a message's system prompt is put in
#' front of the prompt text. Vibe does not report token counts, cost or the model
#' in its output, so those metadata fields are `NA`, except the model when you
#' set `.model`. Replies arrive in one piece; there is no streaming.
#'
#' @param .llm An `LLMMessage` object.
#' @param .model Character; a Mistral model id such as "mistral-large-4". Default
#'   NULL uses the model Vibe is configured with.
#' @param .system_prompt_mode One of "prepend" or "ignore". With "prepend" the
#'   message's system prompt is put before the conversation in the prompt text.
#'   With "ignore" it is dropped.
#' @param .cli_tools Logical; FALSE, the default, refuses every tool call. TRUE
#'   hands over to Vibe's own configuration, which may let it run commands and
#'   edit files.
#' @param .max_turns Integer; the most assistant turns Vibe may take. Default
#'   NULL leaves it to Vibe.
#' @param .max_budget_usd Numeric; stop the run once it has cost this many US
#'   dollars.
#' @param .stateful Logical; if TRUE, Vibe keeps the conversation on its side.
#'   The first call sends the conversation and records the session id in the
#'   metadata; later calls resume that session and send only the newest user
#'   message instead of replaying the history. Default FALSE sends the whole conversation every time.
#' @param .session_id Character; resume this Vibe session explicitly. Normally
#'   left NULL, because `.stateful = TRUE` picks the id up from the message
#'   history on its own.
#' @param .binary Character; the command to run, or a full path to it. Defaults
#'   to "vibe", which is looked for on the PATH and then in the places the
#'   installers use. If R does not find it, set
#'   `options(tidyllm_vibe_cli_path = "/path/to/vibe")` in your `.Rprofile`, or
#'   the `TIDYLLM_VIBE_CLI` environment variable.
#' @param .verbose Logical; if TRUE, prints rate limit information after the
#'   response.
#' @param .timeout Integer; seconds to wait for the whole run.
#' @param .stream Logical; accepted so that `send_chat()` works, but the reply
#'   always arrives in one piece, because Vibe does not stream it.
#' @param .dry_run Logical; if TRUE, returns the command that would be run
#'   instead of running it.
#'
#' @return A new `LLMMessage` object containing the original messages plus the
#'   reply from Vibe.
#' @examples
#' \dontrun{
#' llm_message("What is R's S7 class system?") |>
#'   chat(vibe_cli())
#'
#' llm_message("Explain vapply() in two sentences.") |>
#'   chat(vibe_cli(.model = "mistral-large-4"))
#' }
#'
#' @export
vibe_cli_chat <- function(.llm,
                          .model = NULL,
                          .system_prompt_mode = "prepend",
                          .cli_tools = FALSE,
                          .max_turns = NULL,
                          .max_budget_usd = NULL,
                          .stateful = FALSE,
                          .session_id = NULL,
                          .binary = "vibe",
                          .verbose = FALSE,
                          .timeout = 300,
                          .stream = FALSE,
                          .dry_run = FALSE) {
  built <- do.call(vibe_cli_build_chat_request, mget(names(formals())), quote = TRUE)
  run_chat_pipeline(built, .dry_run)
}

#' Build a Mistral Vibe chat request without running it
#'
#' @noRd
vibe_cli_build_chat_request <- function(.llm,
                                        .model = NULL,
                                        .system_prompt_mode = "prepend",
                                        .cli_tools = FALSE,
                                        .max_turns = NULL,
                                        .max_budget_usd = NULL,
                                        .stateful = FALSE,
                                        .session_id = NULL,
                                        .binary = "vibe",
                                        .verbose = FALSE,
                                        .timeout = 300,
                                        .stream = FALSE,
                                        .dry_run = FALSE) {
  c(
    ".llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
    ".model must be a single string if provided" =
      is.null(.model) || (is.character(.model) && length(.model) == 1),
    ".system_prompt_mode must be \"prepend\" or \"ignore\"" =
      is.character(.system_prompt_mode) && length(.system_prompt_mode) == 1 &&
      .system_prompt_mode %in% c("prepend", "ignore"),
    ".cli_tools must be TRUE or FALSE" = is.logical(.cli_tools) && length(.cli_tools) == 1,
    ".max_turns must be a positive whole number if provided" =
      is.null(.max_turns) || (is_integer_valued(.max_turns) && .max_turns > 0),
    ".max_budget_usd must be a positive number if provided" =
      is.null(.max_budget_usd) || (is.numeric(.max_budget_usd) && .max_budget_usd > 0),
    ".stateful must be logical" = is.logical(.stateful) && length(.stateful) == 1,
    ".session_id must be a single string if provided" =
      is.null(.session_id) || (is.character(.session_id) && length(.session_id) == 1),
    ".binary must be a single string" = is.character(.binary) && length(.binary) == 1,
    ".verbose must be logical" = is.logical(.verbose),
    ".timeout must be an integer-valued numeric (seconds till timeout)" = is_integer_valued(.timeout),
    ".stream must be logical" = is.logical(.stream),
    ".dry_run must be logical" = is.logical(.dry_run)
  ) |>
    validate_inputs()

  api_obj <- api_vibe_cli(
    short_name       = "vibe_cli",
    long_name        = "Mistral Vibe CLI",
    api_key_env_var  = "",
    stream_transport = "process"
  )

  resume_id <- .session_id
  if (isTRUE(.stateful) && is.null(resume_id)) resume_id <- cli_last_session_id(.llm)
  send_history <- !(isTRUE(.stateful) && !is.null(resume_id))
  prompt <- to_api_format(.llm, api_obj, .history = send_history)

  if (identical(.system_prompt_mode, "prepend") && send_history) {
    parts <- .llm@system_prompt
    parts <- parts[!is.na(parts) & nzchar(parts) & parts != "You are a helpful assistant"]
    if (length(parts)) prompt <- paste0(paste(parts, collapse = "\n\n"), "\n\n", prompt)
  }

  env <- NULL
  if (!is.null(.model)) {
    env <- c(
      VIBE_MODELS       = jsonlite::toJSON(list(list(name = .model, provider = "mistral", alias = .model)),
                                           auto_unbox = TRUE),
      VIBE_ACTIVE_MODEL = .model
    )
  }

  args <- c("-p", "--output", "json", "--trust")
  if (!isTRUE(.cli_tools))       args <- c(args, "--agent", "ask", "--disabled-tools", "re:.*")
  if (!is.null(.max_turns))      args <- c(args, "--max-turns", format(.max_turns, scientific = FALSE))
  if (!is.null(.max_budget_usd)) args <- c(args, "--max-price", format(.max_budget_usd, scientific = FALSE))
  if (!is.null(resume_id))       args <- c(args, "--resume", resume_id)

  command <- new_cli_command(vibe_cli_binary(.binary), args, .stdin = prompt, .env = env)

  new_chat_request(
    .request    = command,
    .api        = api_obj,
    .llm        = .llm,
    .body       = NULL,
    .tools_def  = NULL,
    .json       = FALSE,
    .mode       = "value",
    .timeout    = .timeout,
    .max_tries  = 1,
    .verbose    = .verbose,
    .perform_fn = cli_performer(vibe_cli_read_output),
    .meta_fn    = function(meta, response) {
      if (!is.null(.model)) meta$model <- .model
      meta
    }
  )
}

#' Chat through a locally installed Mistral Vibe CLI
#'
#' `vibe_cli()` routes a chat to Mistral's `vibe` command line tool installed on
#' your own machine. See [vibe_cli_chat()] for the arguments and for what happens
#' to Vibe's own tools.
#'
#' @param ... Arguments passed to the Vibe CLI chat function.
#' @param .called_from Internal; the verb that dispatched here.
#'
#' @return The result of the requested action; for `chat()`, an updated
#'   `LLMMessage`.
#'
#' @examples
#' \dontrun{
#' llm_message("Explain R's S7 classes in three sentences.") |>
#'   chat(vibe_cli())
#' }
#'
#' @export
vibe_cli <- create_provider_function(
  .name = "vibe_cli",
  chat  = vibe_cli_chat,
  build = vibe_cli_build_chat_request,
  .media = character()
)

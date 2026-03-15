
#' The OpenRouter API provider class (inherits from OpenAI)
#'
#' @noRd
api_openrouter <- new_class("OpenRouter", api_openai)


#' Extract metadata from OpenRouter chat responses
#'
#' @noRd
method(extract_metadata, list(api_openrouter, class_list)) <- function(.api, .response) {
  list(
    model             = .response$model,
    timestamp         = lubridate::as_datetime(.response$created),
    prompt_tokens     = .response$usage$prompt_tokens,
    completion_tokens = .response$usage$completion_tokens,
    total_tokens      = .response$usage$total_tokens,
    stream            = FALSE,
    specific_metadata = list(
      id               = .response$id,
      native_tokens_prompt     = .response$usage$native_tokens_prompt,
      native_tokens_completion = .response$usage$native_tokens_completion,
      cost             = .response$usage$cost
    )
  )
}


#' Send LLM Messages to the OpenRouter Chat API
#'
#' @description
#' Sends a message history to the OpenRouter API, which provides access to hundreds
#' of models from different providers through a single OpenAI-compatible endpoint.
#'
#' @param .llm An `LLMMessage` object containing the conversation history.
#' @param .model The model identifier to use (default: `"google/gemini-2.5-flash"`).
#'   Any model available on OpenRouter can be used.
#' @param .max_tokens Maximum number of tokens in the response (default: 2048).
#' @param .temperature Controls randomness (0–2, optional).
#' @param .top_p Nucleus sampling parameter (0–1, optional).
#' @param .frequency_penalty Penalizes repeated tokens (-2 to 2, optional).
#' @param .presence_penalty Encourages new topics (-2 to 2, optional).
#' @param .stop One or more stop sequences (optional).
#' @param .stream Logical; if TRUE, streams the response (default: FALSE).
#' @param .tools Either a single TOOL object or a list of TOOL objects for tool calls.
#' @param .tool_choice Tool-calling behavior: `"none"`, `"auto"`, or `"required"` (optional).
#' @param .provider A named list of OpenRouter provider preferences, e.g.
#'   `list(order = c("Anthropic", "AWS Bedrock"), allow_fallbacks = TRUE)` (optional).
#' @param .route OpenRouter routing strategy; `"fallback"` routes to the next model if
#'   the primary is unavailable (optional).
#' @param .models A character vector of model IDs to use as fallbacks when the primary
#'   model is unavailable (optional). Used together with `.route = "fallback"`.
#' @param .api_url Base URL for the OpenRouter API (default: `"https://openrouter.ai"`).
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .verbose If TRUE, displays additional information after the API call (default: FALSE).
#' @param .dry_run If TRUE, returns the request object without executing it (default: FALSE).
#' @param .max_tries Maximum retries (default: 3).
#' @param .max_tool_rounds Maximum number of tool use iterations (default: 10).
#'
#' @return A new `LLMMessage` object containing the original messages plus the assistant's response.
#'
#' @export
openrouter_chat <- function(.llm,
                            .model = "anthropic/claude-sonnet-4-6",
                            .max_tokens = 2048,
                            .temperature = NULL,
                            .top_p = NULL,
                            .frequency_penalty = NULL,
                            .presence_penalty = NULL,
                            .stop = NULL,
                            .stream = FALSE,
                            .tools = NULL,
                            .tool_choice = NULL,
                            .provider = NULL,
                            .route = NULL,
                            .models = NULL,
                            .api_url = "https://openrouter.ai",
                            .timeout = 60,
                            .verbose = FALSE,
                            .dry_run = FALSE,
                            .max_tries = 3,
                            .max_tool_rounds = 10) {

  c(
    "Input .llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
    "Input .model must be a non-empty string" = is.character(.model) & nzchar(.model),
    "Input .max_tokens must be a positive integer" = is_integer_valued(.max_tokens) & .max_tokens > 0,
    "Input .temperature must be between 0 and 2 if provided" = is.null(.temperature) | (.temperature >= 0 & .temperature <= 2),
    "Input .top_p must be between 0 and 1 if provided" = is.null(.top_p) | (.top_p >= 0 & .top_p <= 1),
    "Input .frequency_penalty must be between -2 and 2 if provided" = is.null(.frequency_penalty) | (.frequency_penalty >= -2 & .frequency_penalty <= 2),
    "Input .presence_penalty must be between -2 and 2 if provided" = is.null(.presence_penalty) | (.presence_penalty >= -2 & .presence_penalty <= 2),
    "Input .stop must be NULL, a string, or a list of strings" = is.null(.stop) | is.character(.stop) | is.list(.stop),
    "Input .stream must be logical" = is.logical(.stream),
    "Input .tools must be NULL, a TOOL object, or a list of TOOL objects" = is.null(.tools) || S7_inherits(.tools, TOOL) || (is.list(.tools) && all(purrr::map_lgl(.tools, ~ S7_inherits(.x, TOOL)))),
    "Input .tool_choice must be NULL or one of 'none', 'auto', 'required'" = is.null(.tool_choice) | (.tool_choice %in% c("none", "auto", "required")),
    "Streaming is not supported for requests with tool calls" = is.null(.tools) || !isTRUE(.stream),
    "Input .provider must be NULL or a named list" = is.null(.provider) | is.list(.provider),
    "Input .route must be NULL or a character string" = is.null(.route) | is.character(.route),
    "Input .models must be NULL or a character vector" = is.null(.models) | is.character(.models),
    "Input .api_url must be a valid URL" = is.character(.api_url) & nzchar(.api_url),
    "Input .timeout must be a positive integer" = is_integer_valued(.timeout) & .timeout > 0,
    "Input .verbose must be logical" = is.logical(.verbose),
    "Input .dry_run must be logical" = is.logical(.dry_run),
    "Input .max_tries must be integer-valued numeric" = is_integer_valued(.max_tries),
    ".max_tool_rounds must be a positive integer" = is_integer_valued(.max_tool_rounds) && .max_tool_rounds >= 1
  ) |> validate_inputs()

  api_obj <- api_openrouter(short_name = "openrouter",
                            long_name  = "OpenRouter",
                            api_key_env_var = "OPENROUTER_API_KEY")

  messages <- to_api_format(.llm, api_obj, FALSE)

  api_key <- get_api_key(api_obj, .dry_run)

  tools_def <- if (!is.null(.tools)) {
    if (S7_inherits(.tools, TOOL)) list(.tools) else .tools
  } else {
    NULL
  }

  request_body <- list(
    model = .model,
    messages = messages,
    max_tokens = .max_tokens,
    temperature = .temperature,
    top_p = .top_p,
    frequency_penalty = .frequency_penalty,
    presence_penalty = .presence_penalty,
    stop = .stop,
    stream = .stream,
    tools = if (!is.null(tools_def)) tools_to_api(api_obj, tools_def) else NULL,
    tool_choice = .tool_choice,
    provider = .provider,
    route = .route,
    models = if (!is.null(.models)) as.list(.models) else NULL
  ) |> purrr::compact()

  request <- httr2::request(.api_url) |>
    httr2::req_url_path("/api/v1/chat/completions") |>
    httr2::req_headers(
      `Authorization` = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(data = request_body)

  if (.dry_run) {
    return(request)
  }

  response <- perform_chat_request(request, api_obj, .stream, .timeout, .max_tries)

  if (.stream == FALSE && !is.null(tools_def)) {
    response <- process_tool_loop(
      .api = api_obj,
      .response = response,
      .tools_def = tools_def,
      .request_body = request_body,
      .request = request,
      .timeout = .timeout,
      .max_tries = .max_tries,
      .max_tool_rounds = .max_tool_rounds
    )
  }

  add_message(.llm     = .llm,
              .role    = "assistant",
              .content = response$assistant_reply,
              .json    = FALSE,
              .meta    = response$meta)
}


#' List Available Models on OpenRouter
#'
#' @description
#' Retrieves the list of models available through the OpenRouter API, including
#' pricing and context window information.
#'
#' @param .api_url Base URL for the OpenRouter API (default: `"https://openrouter.ai"`).
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .max_tries Maximum retries (default: 3).
#' @param .dry_run If TRUE, returns the request object without executing it (default: FALSE).
#' @param .verbose If TRUE, displays additional information (default: FALSE).
#'
#' @return A tibble with columns `id`, `name`, `context_length`,
#'   `prompt_price_per_million`, and `completion_price_per_million`,
#'   or NULL if no models are found.
#'
#' @export
openrouter_list_models <- function(.api_url = "https://openrouter.ai",
                                   .timeout = 60,
                                   .max_tries = 3,
                                   .dry_run = FALSE,
                                   .verbose = FALSE) {
  api_obj <- api_openrouter(short_name = "openrouter",
                            long_name  = "OpenRouter",
                            api_key_env_var = "OPENROUTER_API_KEY")

  api_key <- get_api_key(api_obj, .dry_run)

  request <- httr2::request(.api_url) |>
    httr2::req_url_path("/api/v1/models") |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    )

  if (.dry_run) {
    return(request)
  }

  response <- request |>
    httr2::req_timeout(.timeout) |>
    httr2::req_retry(max_tries = .max_tries) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (.verbose) {
    message("Retrieved ", length(response$data), " models from OpenRouter.")
  }

  if (is.null(response$data)) {
    return(NULL)
  }

  models <- response$data

  tibble::tibble(
    id = vapply(models, function(m) m$id %||% NA_character_, character(1)),
    name = vapply(models, function(m) m$name %||% NA_character_, character(1)),
    context_length = vapply(models, function(m) {
      v <- m$context_length
      if (is.null(v)) NA_real_ else as.numeric(v)
    }, numeric(1)),
    prompt_price_per_million = vapply(models, function(m) {
      v <- m$pricing$prompt
      if (is.null(v)) NA_real_ else as.numeric(v) * 1e6
    }, numeric(1)),
    completion_price_per_million = vapply(models, function(m) {
      v <- m$pricing$completion
      if (is.null(v)) NA_real_ else as.numeric(v) * 1e6
    }, numeric(1))
  )
}


#' OpenRouter Provider Function
#'
#' @description
#' The `openrouter()` function provides access to hundreds of AI models from
#' different providers through the OpenRouter API, using a single
#' OpenAI-compatible interface.
#'
#' @param ... Parameters passed to the appropriate OpenRouter-specific function.
#' @param .called_from An internal argument specifying which verb invoked this function.
#'   Managed automatically by tidyllm verbs; do not set manually.
#'
#' @return The result of the requested action (e.g., an updated `LLMMessage` for `chat()`).
#'
#' @export
openrouter <- create_provider_function(
  .name = "openrouter",
  chat = openrouter_chat,
  list_models = openrouter_list_models
)

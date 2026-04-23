
#' The OpenRouter API provider class (inherits from OpenAI)
#'
#' @noRd
api_openrouter <- new_class("OpenRouter", api_chat_completions)


#' Extract metadata from OpenRouter chat responses
#'
#' @noRd
method(extract_metadata, list(api_openrouter, class_list)) <- function(.api, .response) {
  msg <- .response$choices[[1]]$message
  list(
    model             = .response$model,
    timestamp         = lubridate::as_datetime(.response$created),
    prompt_tokens     = .response$usage$prompt_tokens,
    completion_tokens = .response$usage$completion_tokens,
    total_tokens      = .response$usage$total_tokens,
    stream            = FALSE,
    specific_metadata = list(
      id                       = .response$id,
      native_tokens_prompt     = .response$usage$native_tokens_prompt,
      native_tokens_completion = .response$usage$native_tokens_completion,
      reasoning_tokens         = .response$usage$reasoning_tokens,
      cost                     = .response$usage$cost,
      reasoning                = msg$reasoning,
      reasoning_details        = msg$reasoning_details
    ) |> purrr::compact()
  )
}


#' Convert LLMMessage to OpenRouter API format (with reasoning pass-through)
#'
#' @noRd
method(to_api_format, list(LLMMessage, api_openrouter)) <- function(.llm,
                                                                    .api,
                                                                    .no_system = FALSE) {
  history <- if (.no_system) filter_roles(.llm@message_history, c("user", "assistant")) else .llm@message_history
  lapply(history, function(m) {
    formatted_message <- format_message(m)
    audio_media <- extract_media(m$media, "audio")
    video_media <- extract_media(m$media, "video")
    has_images        <- length(formatted_message$images) > 0
    has_audio_or_video <- length(audio_media) > 0 || length(video_media) > 0
    msg <- if (has_images || has_audio_or_video) {
      content_parts <- list(list(type = "text", text = formatted_message$content))
      for (img_struct in formatted_message$images) {
        content_parts <- c(content_parts, list(list(
          type = "image_url",
          image_url = list(url = glue::glue("data:{img_struct$media_type};base64,{img_struct$data}"))
        )))
      }
      for (med in audio_media) {
        raw_bytes  <- readBin(med@audiopath, what = "raw", n = file.size(med@audiopath))
        b64        <- base64enc::base64encode(raw_bytes)
        audio_fmt  <- sub("audio/", "", med@audiomime)
        if (audio_fmt == "mpeg") audio_fmt <- "mp3"
        content_parts <- c(content_parts, list(list(
          type = "input_audio",
          input_audio = list(data = b64, format = audio_fmt)
        )))
      }
      for (med in video_media) {
        raw_bytes <- readBin(med@videopath, what = "raw", n = file.size(med@videopath))
        b64       <- base64enc::base64encode(raw_bytes)
        content_parts <- c(content_parts, list(list(
          type = "video_url",
          video_url = list(url = glue::glue("data:{med@videomime};base64,{b64}"))
        )))
      }
      list(role = m$role, content = content_parts)
    } else {
      list(role = m$role, content = formatted_message$content)
    }
    if (m$role == "assistant") {
      reasoning         <- m$meta$specific_metadata$reasoning
      reasoning_details <- m$meta$specific_metadata$reasoning_details
      if (!is.null(reasoning_details)) {
        msg$reasoning_details <- reasoning_details
      } else if (!is.null(reasoning)) {
        msg$reasoning <- reasoning
      }
    }
    msg
  })
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
#' @param .reasoning A named list controlling reasoning token behavior (optional). Supported
#'   fields vary by model family:
#'   - `effort`: one of `"xhigh"`, `"high"`, `"medium"`, `"low"`, `"minimal"`, `"none"` (OpenAI/Grok)
#'   - `max_tokens`: integer specifying the reasoning token budget (Anthropic/Gemini/Alibaba)
#'   - `exclude`: logical; if `TRUE`, reasoning is used internally but not returned in the response
#'   - `enabled`: logical; if `TRUE`, activates reasoning at default settings
#'
#'   Example: `list(effort = "high")` or `list(max_tokens = 4000, exclude = FALSE)`.
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
#' @param .json_schema A JSON schema object for structured output (default: NULL).
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
                            .json_schema = NULL,
                            .tools = NULL,
                            .tool_choice = NULL,
                            .reasoning = NULL,
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
    "Input .json_schema must be NULL or a list or an ellmer type object" = is.null(.json_schema) | is.list(.json_schema) | is_ellmer_type(.json_schema),
    "Streaming is not supported for requests with structured outputs" = is.null(.json_schema) || !isTRUE(.stream),
    "Input .tools must be NULL, a TOOL object, or a list of TOOL objects" = is.null(.tools) || S7_inherits(.tools, TOOL) || (is.list(.tools) && all(purrr::map_lgl(.tools, ~ S7_inherits(.x, TOOL)))),
    "Input .tool_choice must be NULL or one of 'none', 'auto', 'required'" = is.null(.tool_choice) | (.tool_choice %in% c("none", "auto", "required")),
    "Streaming is not supported for requests with tool calls" = is.null(.tools) || !isTRUE(.stream),
    "Input .reasoning must be NULL or a named list" = is.null(.reasoning) | is.list(.reasoning),
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

  json <- FALSE
  response_format <- NULL
  if (!is.null(.json_schema)) {
    json <- TRUE
    if (requireNamespace("ellmer", quietly = TRUE) && S7_inherits(.json_schema, ellmer::TypeObject)) {
      .json_schema <- to_schema(.json_schema)
    }
    schema_name <- attr(.json_schema, "name") %||% "tidyllm_schema"
    response_format <- list(
      type = "json_schema",
      json_schema = list(name = schema_name, schema = .json_schema)
    )
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
    response_format = response_format,
    tools = if (!is.null(tools_def)) tools_to_api(api_obj, tools_def) else NULL,
    tool_choice = .tool_choice,
    reasoning = .reasoning,
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
              .json    = json,
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


#' Get OpenRouter Credit Balance
#'
#' @description
#' Returns the total credits purchased and total usage so far for the current
#' API key.
#'
#' @param .api_url Base URL for the OpenRouter API (default: `"https://openrouter.ai"`).
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .max_tries Maximum retries (default: 3).
#'
#' @return A tibble with columns `total_credits` (USD purchased), `total_usage`
#'   (USD consumed so far), and `remaining` (USD remaining).
#'
#' @export
openrouter_credits <- function(.api_url = "https://openrouter.ai",
                               .timeout = 60,
                               .max_tries = 3) {
  api_obj <- api_openrouter(short_name = "openrouter",
                            long_name  = "OpenRouter",
                            api_key_env_var = "OPENROUTER_API_KEY")
  api_key <- get_api_key(api_obj, FALSE)

  response <- httr2::request(.api_url) |>
    httr2::req_url_path("/api/v1/credits") |>
    httr2::req_headers(Authorization = sprintf("Bearer %s", api_key)) |>
    httr2::req_timeout(.timeout) |>
    httr2::req_retry(max_tries = .max_tries) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  tibble::tibble(
    total_credits = response$data$total_credits,
    total_usage   = response$data$total_usage,
    remaining     = response$data$total_credits - response$data$total_usage
  )
}


#' Get Details for an OpenRouter Generation
#'
#' @description
#' Fetches cost, latency, and token details for a past generation using its ID.
#' The generation ID is available in `get_metadata(result)$api_specific$id`
#' after a `chat()` call.
#'
#' @param .id The generation ID string (e.g. `"gen-..."`) returned by OpenRouter.
#' @param .api_url Base URL for the OpenRouter API (default: `"https://openrouter.ai"`).
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .max_tries Maximum retries (default: 3).
#'
#' @return A named list with generation details including `id`, `model`,
#'   `total_cost`, `tokens_prompt`, `tokens_completion`, and `latency`.
#'
#' @export
openrouter_generation <- function(.id,
                                  .api_url = "https://openrouter.ai",
                                  .timeout = 60,
                                  .max_tries = 3) {
  c(
    "Input .id must be a non-empty string" = is.character(.id) && nzchar(.id)
  ) |> validate_inputs()

  api_obj <- api_openrouter(short_name = "openrouter",
                            long_name  = "OpenRouter",
                            api_key_env_var = "OPENROUTER_API_KEY")
  api_key <- get_api_key(api_obj, FALSE)

  response <- httr2::request(.api_url) |>
    httr2::req_url_path("/api/v1/generation") |>
    httr2::req_url_query(id = .id) |>
    httr2::req_headers(Authorization = sprintf("Bearer %s", api_key)) |>
    httr2::req_timeout(.timeout) |>
    httr2::req_retry(max_tries = .max_tries) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  d <- response$data
  list(
    id                = d$id,
    model             = d$model,
    total_cost        = d$total_cost,
    tokens_prompt     = d$tokens_prompt,
    tokens_completion = d$tokens_completion,
    latency           = d$latency,
    provider          = d$provider_name,
    origin            = d$origin
  )
}


#' Generate Embeddings Using the OpenRouter API
#'
#' @description
#' Sends text to an embedding model accessible via OpenRouter and returns
#' embedding vectors. Note that embedding models are not listed in
#' `list_models(openrouter())` — specify the model ID directly.
#' Known supported models include `"openai/text-embedding-3-small"`,
#' `"openai/text-embedding-3-large"`, and `"mistralai/mistral-embed"`.
#'
#' @param .input An `LLMMessage` object or a character vector of texts to embed.
#' @param .model The embedding model ID (default: `"openai/text-embedding-3-small"`).
#' @param .timeout Request timeout in seconds (default: 120).
#' @param .dry_run If TRUE, returns the request object without executing it (default: FALSE).
#' @param .max_tries Maximum retries (default: 3).
#'
#' @return A tibble with columns `input` (text) and `embeddings` (list of numeric vectors).
#'
#' @export
openrouter_embedding <- function(.input,
                                 .model = "openai/text-embedding-3-small",
                                 .timeout = 120,
                                 .dry_run = FALSE,
                                 .max_tries = 3) {
  c(
    "Input .input must be an LLMMessage object or a character vector" = S7_inherits(.input, LLMMessage) | is.character(.input),
    "Input .model must be a non-empty string" = is.character(.model) && nzchar(.model),
    "Input .timeout must be a positive number" = is.numeric(.timeout) && .timeout > 0,
    "Input .dry_run must be logical" = is.logical(.dry_run)
  ) |> validate_inputs()

  api_obj <- api_openrouter(short_name = "openrouter",
                            long_name  = "OpenRouter",
                            api_key_env_var = "OPENROUTER_API_KEY")
  api_key <- get_api_key(api_obj, .dry_run)

  input_texts <- parse_embedding_input(.input)

  request <- httr2::request("https://openrouter.ai") |>
    httr2::req_url_path("/api/v1/embeddings") |>
    httr2::req_headers(
      Authorization  = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(list(model = .model, input = input_texts))

  if (.dry_run) {
    return(request)
  }

  extract_embeddings_fn <- function(response_content, error, response_headers) {
    if (error) {
      paste0("OpenRouter embedding error: ", response_content$error$message) |> stop()
    }
    response_content$data |>
      purrr::map("embedding") |>
      purrr::map(unlist)
  }

  perform_embedding_request(
    .request             = request,
    .timeout             = .timeout,
    .max_tries           = .max_tries,
    .input_texts         = input_texts,
    .fn_extract_embeddings = extract_embeddings_fn
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
  embed = openrouter_embedding,
  list_models = openrouter_list_models
)

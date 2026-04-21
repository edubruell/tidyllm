
#' The OpenAI Responses API provider class
#'
#' @noRd
api_openai <- new_class("OpenAI", APIProvider)

#' Convert LLMMessage to OpenAI Responses API format
#'
#' System messages go to top-level `instructions`; all other messages go to `input`.
#'
#' @noRd
method(to_api_format, list(LLMMessage, api_openai)) <- function(.llm, .api, .no_system = FALSE) {
  history <- .llm@message_history

  # Collect system content into instructions string
  system_messages <- Filter(function(m) m$role == "system", history)
  instructions <- if (length(system_messages) > 0 && !.no_system) {
    paste(sapply(system_messages, function(m) format_message(m)$content), collapse = "\n")
  } else {
    NULL
  }

  non_system <- Filter(function(m) m$role != "system", history)

  input <- lapply(non_system, function(m) {
    formatted <- format_message(m)
    if (!is.null(formatted$image)) {
      list(
        role = m$role,
        content = list(
          list(type = "input_text", text = formatted$content),
          list(type = "input_image", image_url = list(
            url = glue::glue("data:{formatted$image$media_type};base64,{formatted$image$data}")
          ))
        )
      )
    } else {
      list(role = m$role, content = formatted$content)
    }
  })

  list(instructions = instructions, input = input)
}

#' Extract rate limit info from OpenAI Responses API headers
#'
#' @noRd
method(ratelimit_from_header, list(api_openai, new_S3_class("httr2_headers"))) <- function(.api, .headers) {
  if (!any(grepl("x-ratelimit", names(.headers)))) return(NULL)

  request_time <- strptime(.headers["date"]$date, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")

  ratelimit_requests_reset_dt <- parse_duration_to_seconds(
    .headers["x-ratelimit-reset-requests"]$`x-ratelimit-reset-requests`)
  ratelimit_tokens_reset_dt <- parse_duration_to_seconds(
    .headers["x-ratelimit-reset-tokens"]$`x-ratelimit-reset-tokens`)

  list(
    this_request_time              = request_time,
    ratelimit_requests             = as.integer(.headers["x-ratelimit-limit-requests"]),
    ratelimit_requests_remaining   = as.integer(.headers["x-ratelimit-remaining-requests"]),
    ratelimit_requests_reset_time  = request_time + ratelimit_requests_reset_dt,
    ratelimit_tokens               = as.integer(.headers["x-ratelimit-limit-tokens"]),
    ratelimit_tokens_remaining     = as.integer(.headers["x-ratelimit-remaining-tokens"]),
    ratelimit_tokens_reset_time    = request_time + ratelimit_tokens_reset_dt
  )
}

#' Parse chat response from OpenAI Responses API
#'
#' @noRd
method(parse_chat_response, list(api_openai, class_list)) <- function(.api, .content) {
  if (!is.null(.content$error)) {
    sprintf("OpenAI returned an Error:\nType: %s\nMessage: %s",
            .content$error$type, .content$error$message) |> stop()
  }

  # Walk output[] for type == "message"; skip reasoning and function_call items
  message_items <- Filter(function(x) !is.null(x$type) && x$type == "message", .content$output)

  # When the model issued tool calls, output[] has no message item yet — return NULL
  # so the tool loop in process_tool_loop() can take over.
  if (length(message_items) == 0) return(NULL)

  content_blocks <- message_items[[1]]$content
  text_blocks <- Filter(function(b) !is.null(b$type) && b$type == "output_text", content_blocks)

  if (length(text_blocks) == 0) return(NULL)

  text_blocks[[1]]$text
}

#' Extract metadata from OpenAI Responses API response
#'
#' @noRd
method(extract_metadata, list(api_openai, class_list)) <- function(.api, .response) {
  usage <- .response$usage
  list(
    model             = .response$model,
    timestamp         = lubridate::as_datetime(lubridate::now()),
    prompt_tokens     = usage$input_tokens,
    completion_tokens = usage$output_tokens,
    total_tokens      = (usage$input_tokens %||% 0L) + (usage$output_tokens %||% 0L),
    stream            = FALSE,
    specific_metadata = list(
      reasoning_tokens = usage$output_tokens_details$reasoning_tokens
    )
  )
}

#' Extract metadata from OpenAI Responses API streaming data
#'
#' @noRd
method(extract_metadata_stream, list(api_openai, class_list)) <- function(.api, .stream_raw_data) {
  # The response.completed event contains the full response object
  completed <- purrr::keep(.stream_raw_data, ~ !is.null(.x$type) && .x$type == "response.completed")

  if (length(completed) == 0) {
    return(callNextMethod())
  }

  resp <- completed[[1]]$response
  usage <- resp$usage %||% list()

  list(
    model             = resp$model,
    timestamp         = lubridate::as_datetime(lubridate::now()),
    prompt_tokens     = usage$input_tokens,
    completion_tokens = usage$output_tokens,
    total_tokens      = (usage$input_tokens %||% 0L) + (usage$output_tokens %||% 0L),
    stream            = TRUE,
    specific_metadata = list(
      reasoning_tokens = usage$output_tokens_details$reasoning_tokens
    )
  )
}

#' Handle SSE streaming for OpenAI Responses API
#'
#' The Responses API embeds the event type as `type` inside the JSON data field
#' rather than using SSE `event:` headers.
#'
#' @noRd
method(handle_stream, list(api_openai, new_S3_class("httr2_response"))) <- function(.api, .stream_response) {
  stream_text <- ""
  stream_data <- list()

  repeat {
    stream_chunk <- httr2::resp_stream_sse(.stream_response)

    # NULL means no data available yet — keep reading
    if (is.null(stream_chunk)) next

    data_str <- stream_chunk$data
    if (is.null(data_str) || !nzchar(data_str)) next

    parsed <- tryCatch(
      jsonlite::fromJSON(data_str, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(parsed)) next

    event_type <- parsed$type %||% ""

    if (event_type == "response.output_text.delta") {
      delta <- parsed$delta
      if (!is.null(delta) && nzchar(delta)) {
        stream_text <- paste0(stream_text, delta)
        cat(delta)
        utils::flush.console()
      }
    } else if (event_type == "response.completed") {
      stream_data <- append(stream_data, list(parsed))
      close(.stream_response)
      message("\n---------\nStream finished\n---------\n")
      break
    } else if (event_type == "response.failed") {
      close(.stream_response)
      stop("OpenAI Responses API stream failed")
    }
  }

  list(reply = stream_text, raw_data = stream_data)
}

#' Convert TOOL list to OpenAI Responses API flat tool schema
#'
#' @noRd
method(tools_to_api, list(api_openai, class_list)) <- function(.api, .tools) {
  purrr::map(.tools, function(tool) {
    if (length(tool@builtin) > 0) {
      return(tool@builtin[[1]])
    }
    list(
      type        = "function",
      name        = tool@name,
      description = tool@description,
      parameters  = list(
        type                 = "object",
        properties           = purrr::map(tool@input_schema, field_to_param_schema),
        required             = as.list(names(tool@input_schema)),
        additionalProperties = FALSE
      ),
      strict      = TRUE
    )
  })
}

#' Check if Responses API response contains custom function_call items
#'
#' @noRd
method(has_tool_calls, list(api_openai, class_any)) <- function(.api, .response) {
  output <- .response$raw$content$output %||% list()
  any(purrr::map_lgl(output, ~ !is.null(.x$type) && .x$type == "function_call"))
}

#' Extract function_call items from Responses API output
#'
#' @noRd
method(extract_tool_calls, list(api_openai, class_any)) <- function(.api, .response) {
  output <- .response$raw$content$output %||% list()
  Filter(function(x) !is.null(x$type) && x$type == "function_call", output)
}

#' Execute tool calls and return function_call_output items
#'
#' @noRd
method(run_tool_calls, list(api_openai, class_list, class_list)) <- function(.api, .tool_calls, .tools) {
  parse_json_args <- function(json_str) {
    tryCatch(jsonlite::fromJSON(json_str, simplifyVector = TRUE), error = function(e) NULL)
  }

  purrr::map(.tool_calls, function(tc) {
    tool_name <- tc$name
    tool_args <- parse_json_args(tc$arguments)
    call_id   <- tc$call_id

    if (is.null(tool_args)) {
      warning(sprintf("Failed to parse arguments for tool: %s", tool_name))
      return(NULL)
    }

    matching <- purrr::keep(.tools, ~ .x@name == tool_name)
    if (length(matching) == 0) {
      warning(sprintf("No matching tool found for: %s", tool_name))
      return(NULL)
    }

    result <- do.call(matching[[1]]@func, as.list(tool_args))

    list(
      type    = "function_call_output",
      call_id = call_id,
      output  = jsonlite::toJSON(result, auto_unbox = TRUE)
    )
  }) |> purrr::compact()
}

#' Append tool messages to Responses API request input array
#'
#' Carries the model's full output[] (including server-tool items) and the
#' function_call_output items back into input for the next round.
#'
#' @noRd
method(append_tool_messages, list(api_openai, class_any, class_any, class_any)) <-
  function(.api, .request_body, .response, .tool_results) {
    model_output <- .response$raw$content$output %||% list()
    .request_body$input <- c(.request_body$input, model_output, .tool_results)
    .request_body
  }

#' Prepare OpenAI Responses API request body parameters
#'
#' @noRd
add_no_additional_properties <- function(schema) {
  if (is.list(schema) && identical(schema$type, "object")) {
    schema$additionalProperties <- FALSE
    if (!is.null(schema$properties)) {
      schema$properties <- lapply(schema$properties, add_no_additional_properties)
    }
  } else if (is.list(schema) && !is.null(schema$items)) {
    schema$items <- add_no_additional_properties(schema$items)
  }
  schema
}

prepare_responses_request <- function(.llm,
                                      .api,
                                      .model         = "gpt-5.4",
                                      .max_output_tokens = NULL,
                                      .temperature   = NULL,
                                      .reasoning_effort = NULL,
                                      .json_schema   = NULL,
                                      .seed          = NULL) {
  fmt <- to_api_format(.llm, .api)

  # Handle JSON schema → text.format
  text_format <- NULL
  json <- FALSE
  if (!is.null(.json_schema)) {
    json <- TRUE
    schema_name <- "tidyllm_schema"
    if (requireNamespace("ellmer", quietly = TRUE) && S7_inherits(.json_schema, ellmer::TypeObject)) {
      .json_schema <- to_schema(.json_schema)
      schema_name <- "ellmer_schema"
    }
    if (!is.null(attr(.json_schema, "name"))) schema_name <- attr(.json_schema, "name")
    .json_schema <- add_no_additional_properties(.json_schema)
    text_format <- list(
      format = list(
        type        = "json_schema",
        strict      = TRUE,
        name        = schema_name,
        schema      = .json_schema
      )
    )
  }

  request_body <- list(
    model              = .model,
    input              = fmt$input,
    instructions       = fmt$instructions,
    max_output_tokens  = .max_output_tokens,
    temperature        = .temperature,
    seed               = .seed,
    text               = text_format
  ) |> purrr::compact()

  if (!is.null(.reasoning_effort)) {
    request_body$reasoning <- list(effort = .reasoning_effort)
  }

  list(request_body = request_body, json = json)
}

#' Send LLM Messages to the OpenAI Responses API
#'
#' @description
#' Sends a message history to the OpenAI Responses API (`POST /v1/responses`) and
#' returns the assistant's reply. Supports streaming, tool use, structured output,
#' and reasoning models (o-series) via `.reasoning_effort`.
#'
#' @param .llm An `LLMMessage` object containing the conversation history.
#' @param .model The model identifier (default: `"gpt-4o"`).
#' @param .max_output_tokens Maximum tokens to generate (caps reasoning + completion).
#' @param .temperature Sampling temperature (0-2).
#' @param .seed Seed for deterministic sampling.
#' @param .stream If TRUE, stream output to console (default: FALSE).
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .verbose Print rate-limit info after the call (default: FALSE).
#' @param .json_schema A tidyllm schema or ellmer type for structured output.
#' @param .max_tries Maximum retry attempts (default: 3).
#' @param .dry_run If TRUE, return the request object without sending (default: FALSE).
#' @param .reasoning_effort For o-series models: `"low"`, `"medium"`, or `"high"`.
#' @param .tools A TOOL object or list of TOOL objects for function calling.
#' @param .tool_choice Tool selection behavior: `"none"`, `"auto"`, or `"required"`.
#' @param .max_tool_rounds Maximum tool-loop iterations (default: 10).
#'
#' @return A new `LLMMessage` object with the assistant's response appended.
#' @export
openai_chat <- function(
    .llm,
    .model               = "gpt-5.4",
    .max_output_tokens   = NULL,
    .temperature         = NULL,
    .seed                = NULL,
    .stream              = FALSE,
    .timeout             = 60,
    .verbose             = FALSE,
    .json_schema         = NULL,
    .max_tries           = 3,
    .dry_run             = FALSE,
    .reasoning_effort    = NULL,
    .tools               = NULL,
    .tool_choice         = NULL,
    .max_tool_rounds     = 10
) {
  c(
    "Input .llm must be an LLMMessage object"                                             = S7_inherits(.llm, LLMMessage),
    "Input .model must be a string"                                                       = is.character(.model),
    "Input .max_output_tokens must be NULL or a positive integer"                         = is.null(.max_output_tokens) | (is_integer_valued(.max_output_tokens) & .max_output_tokens > 0),
    "Input .temperature must be numeric or NULL"                                          = is.null(.temperature) | is.numeric(.temperature),
    "Input .seed must be NULL or an integer"                                              = is.null(.seed) | is_integer_valued(.seed),
    "Input .stream must be logical"                                                       = is.logical(.stream),
    "Input .timeout must be integer-valued numeric"                                       = is_integer_valued(.timeout),
    "Input .verbose must be logical"                                                      = is.logical(.verbose),
    "Input .json_schema must be NULL, a list, or an ellmer type object"                   = is.null(.json_schema) | is.list(.json_schema) | is_ellmer_type(.json_schema),
    "Input .max_tries must be integer-valued numeric"                                     = is_integer_valued(.max_tries),
    "Input .dry_run must be logical"                                                      = is.logical(.dry_run),
    "Input .reasoning_effort must be NULL or one of 'low', 'medium', 'high'"              = is.null(.reasoning_effort) | (.reasoning_effort %in% c("low", "medium", "high")),
    "Input .tools must be NULL, a TOOL object, or a list of TOOL objects"                 = is.null(.tools) || S7_inherits(.tools, TOOL) || (is.list(.tools) && all(purrr::map_lgl(.tools, ~ S7_inherits(.x, TOOL)))),
    "Input .tool_choice must be NULL or one of 'none', 'auto', 'required'"                = is.null(.tool_choice) || (.tool_choice %in% c("none", "auto", "required")),
    ".max_tool_rounds must be a positive integer"                                         = is_integer_valued(.max_tool_rounds) && .max_tool_rounds >= 1,
    "Streaming is not supported for requests with tool calls"                             = is.null(.tools) || !isTRUE(.stream)
  ) |> validate_inputs()

  api_obj <- api_openai(
    short_name      = "openai",
    long_name       = "OpenAI",
    api_key_env_var = "OPENAI_API_KEY"
  )

  api_key <- get_api_key(api_obj, .dry_run)

  request_data <- prepare_responses_request(
    .llm              = .llm,
    .api              = api_obj,
    .model            = .model,
    .max_output_tokens = .max_output_tokens,
    .temperature      = .temperature,
    .reasoning_effort = .reasoning_effort,
    .json_schema      = .json_schema,
    .seed             = .seed
  )

  request_body <- request_data$request_body
  json         <- request_data$json

  # Tools
  tools_def <- if (!is.null(.tools)) {
    if (S7_inherits(.tools, TOOL)) list(.tools) else .tools
  } else {
    NULL
  }

  if (!is.null(tools_def)) {
    request_body$tools        <- tools_to_api(api_obj, tools_def)
    request_body$tool_choice  <- .tool_choice
  }

  if (isTRUE(.stream)) {
    request_body$stream <- TRUE
  }

  request <- httr2::request("https://api.openai.com/v1/responses") |>
    httr2::req_headers(
      Authorization  = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(data = request_body)

  if (.dry_run) return(request)

  response <- perform_chat_request(request, api_obj, .stream, .timeout, .max_tries)

  if (!isTRUE(.stream) && !is.null(tools_def)) {
    response <- process_tool_loop(
      .api           = api_obj,
      .response      = response,
      .tools_def     = tools_def,
      .request_body  = request_body,
      .request       = request,
      .timeout       = .timeout,
      .max_tries     = .max_tries,
      .max_tool_rounds = .max_tool_rounds
    )
  }

  track_rate_limit(api_obj, response$headers, .verbose)

  add_message(
    .llm     = .llm,
    .role    = "assistant",
    .content = response$assistant_reply,
    .json    = json,
    .meta    = response$meta
  )
}

#' OpenAI built-in web search tool (server-executed)
#'
#' @return A TOOL object declaring the web_search_preview built-in.
#' @export
openai_websearch <- function() {
  TOOL(
    name         = "web_search",
    description  = "Builtin: web_search_preview",
    input_schema = list(),
    func         = function() NULL,
    builtin      = list(list(type = "web_search_preview"))
  )
}

#' OpenAI built-in code interpreter tool (server-executed)
#'
#' @return A TOOL object declaring the code_interpreter built-in.
#' @export
openai_code_interpreter <- function() {
  TOOL(
    name         = "code_interpreter",
    description  = "Builtin: code_interpreter",
    input_schema = list(),
    func         = function() NULL,
    builtin      = list(list(type = "code_interpreter", container = list(type = "auto")))
  )
}

#' OpenAI built-in file search tool (server-executed)
#'
#' @param .vector_store_ids Character vector of vector store IDs to search.
#' @return A TOOL object declaring the file_search built-in.
#' @export
openai_file_search <- function(.vector_store_ids) {
  c(".vector_store_ids must be a character vector" = is.character(.vector_store_ids)) |>
    validate_inputs()
  TOOL(
    name         = "file_search",
    description  = "Builtin: file_search",
    input_schema = list(),
    func         = function() NULL,
    builtin      = list(list(type = "file_search", vector_store_ids = as.list(.vector_store_ids)))
  )
}

#' OpenAI Provider Function
#'
#' The `openai()` function acts as an interface for interacting with the OpenAI API
#' through main `tidyllm` verbs such as `chat()`, `embed()`, and `send_batch()`.
#' Chat uses the Responses API (`POST /v1/responses`); embeddings and batch operations
#' use the Chat Completions / Embeddings endpoints unchanged.
#'
#' @param ... Parameters passed to the appropriate OpenAI-specific function.
#' @param .called_from Internal routing argument; do not set manually.
#'
#' @return Result of the requested action.
#' @export
openai <- create_provider_function(
  .name        = "openai",
  chat         = openai_chat,
  embed        = openai_embedding,
  send_batch   = send_openai_batch,
  check_batch  = check_openai_batch,
  list_batches = list_openai_batches,
  fetch_batch  = fetch_openai_batch,
  list_models  = openai_list_models
)

#' Alias for the OpenAI Provider Function
#'
#' @param ... Parameters passed to the appropriate OpenAI-specific function.
#' @param .called_from Internal routing argument; do not set manually.
#' @keywords internal
#' @export
chatgpt <- openai

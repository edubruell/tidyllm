

#' The Google Gemini API provider stub
#'
#' At the moment this is just a stub but is needed for methods dispatch
#'
#' @noRd
api_gemini <- new_class("Google Gemini", APIProvider)

#' Convert LLMMessage to Gemini API-Compatible Format
#'
#' Converts the `message_history` of an `LLMMessage` object into the
#' one needed for the Google Gemini API.
#'
#' @noRd
method(to_api_format, list(LLMMessage, api_gemini)) <- function(.llm,
                                                                .api) {
  gemini_history <- filter_roles(.llm@message_history, c("user", "assistant"))

  lapply(gemini_history, function(m) {
    formatted_message <- format_message(m)
    parts <- list()

    # Text part first
    parts <- c(parts, list(list(text = formatted_message$content)))

    # Inline images
    for (img_struct in formatted_message$images) {
      parts <- c(parts, list(list(
        inline_data = list(mime_type = img_struct$media_type, data = img_struct$data)
      )))
    }

    # Inline audio (lazy-encode at format time)
    if (!is.null(m$media)) {
      for (med in m$media) {
        if (S7_inherits(med, tidyllm_audio)) {
          raw_bytes <- readBin(med@audiopath, what = "raw", n = file.size(med@audiopath))
          b64 <- base64enc::base64encode(raw_bytes)
          parts <- c(parts, list(list(
            inline_data = list(mime_type = med@audiomime, data = b64)
          )))
        }
        if (S7_inherits(med, tidyllm_video)) {
          raw_bytes <- readBin(med@videopath, what = "raw", n = file.size(med@videopath))
          b64 <- base64enc::base64encode(raw_bytes)
          parts <- c(parts, list(list(
            inline_data = list(mime_type = med@videomime, data = b64)
          )))
        }
        if (S7_inherits(med, tidyllm_pdf) && !med@text_extract) {
          raw_bytes <- readBin(med@pdfpath, what = "raw", n = file.size(med@pdfpath))
          b64 <- base64enc::base64encode(raw_bytes)
          parts <- c(parts, list(list(
            inline_data = list(mime_type = "application/pdf", data = b64)
          )))
        } else if (S7_inherits(med, tidyllm_pdf) && med@text_extract) {
          parts[[1]]$text <- paste0(parts[[1]]$text, " <pdf filename=\"", med@pdfname, "\">", med@pdftext, "</pdf>")
        }
      }
    }

    # Remote file references (tidyllm_file, provider == "gemini")
    if (!is.null(m$files)) {
      for (f in m$files) {
        if (f@provider != "gemini") next
        parts <- c(parts, list(list(
          fileData = list(mimeType = f@mime_type, fileUri = f@uri)
        )))
      }
    }

    list(
      role  = ifelse(m$role == "user", "user", "model"),
      parts = parts
    )
  })
}


#' A chat response parsing method for Gemini to extract the assistant response 
#'
#' @noRd
method(parse_chat_response, list(api_gemini,class_list)) <- function(.api,.content) {
  api_label <- .api@long_name 
  if("error" %in% names(.content)){
    sprintf("%s returned an Error:\nCode: %s\nMessage: %s",
            api_label,
            .content$error$code,
            .content$error$message) |>
      stop()
  }
  
  if (!"candidates" %in% names(.content) || length(.content$candidates) == 0) {
    paste0("Received empty response from ", api_label) |>
      stop()
  }
  
  .content$candidates[[1]]$content$parts[[1]]$text
}


#' Parse one Gemini SSE event
#'
#' Gemini streams ordinary SSE only when the request carries `alt=sse`. Without
#' it the endpoint returns a pretty-printed JSON array streamed in chunks, with
#' no SSE framing at all, which is why tidyllm buffered the text and pattern
#' matched it before 0.6.0. The request builder now always sends `alt=sse`, so
#' the buffer-and-match parser is gone.
#'
#' Parts carrying `thought = TRUE` are thinking output, not reply text, and must
#' be classified rather than concatenated into the reply.
#'
#' @noRd
method(parse_stream_event, api_gemini) <- function(.api, .chunk) {
  parsed <- parse_stream_json(.chunk$data)
  if (is.null(parsed)) return(stream_event("noop"))

  if (!is.null(parsed$error)) {
    detail <- parsed$error$message %||% "unknown error"
    return(stream_event("error", error = detail, keep = TRUE, event = parsed))
  }

  candidate <- parsed$candidates[[1]] %||% NULL
  # A finishReason of any kind ends the stream. Gemini sends the usage metadata
  # on that same event, so it is kept like every other one.
  done <- !is.null(candidate$finishReason)

  parts    <- candidate$content$parts %||% list()
  is_think <- vapply(parts, function(p) isTRUE(p$thought), logical(1))
  text     <- paste0(
    vapply(parts[!is_think], function(p) p$text %||% "", character(1)),
    collapse = ""
  )

  kind <- if (nzchar(text)) "text" else if (any(is_think)) "thinking" else "meta"
  stream_event(kind, text = text, done = done, keep = TRUE, event = parsed)
}

#' Rebuild a Gemini candidate body from its stream events
#'
#' Gemini splits a response across chunks but never splits a part: each chunk
#' carries whole parts, and `functionCall$args` arrives as a parsed object rather
#' than a JSON string. So the parts are concatenated in arrival order and handed
#' back untouched, which also preserves the `thoughtSignature` that
#' `append_tool_messages()` has to send back or the continued turn is rejected.
#'
#' Consecutive text parts are merged, because a caller reading `parts` expects
#' the shape of a blocking response, where the text is one part and not one per
#' chunk.
#'
#' @noRd
method(assemble_stream_response, list(api_gemini, class_list)) <- function(.api, .events) {
  parts     <- list()
  candidate <- list(role = "model")
  envelope  <- list()

  for (event in .events) {
    envelope <- utils::modifyList(
      envelope,
      event[intersect(names(event), c("usageMetadata", "modelVersion", "responseId"))]
    )

    if (length(event$candidates) == 0) next
    cand <- event$candidates[[1]]
    candidate <- utils::modifyList(
      candidate,
      cand[intersect(names(cand), c("finishReason", "index", "safetyRatings",
                                    "groundingMetadata", "citationMetadata"))]
    )

    for (part in cand$content$parts %||% list()) {
      last <- if (length(parts) > 0) parts[[length(parts)]] else NULL
      mergeable <- !is.null(part$text) && !isTRUE(part$thought) &&
        !is.null(last) && !is.null(last$text) && !isTRUE(last$thought) &&
        identical(names(last), names(part))

      if (mergeable) {
        parts[[length(parts)]]$text <- paste0(last$text, part$text)
      } else {
        parts <- append(parts, list(part))
      }
    }
  }

  candidate$content <- list(parts = parts, role = "model")
  candidate$role    <- NULL

  utils::modifyList(envelope, list(candidates = list(candidate)))
}



#' A function to get metadata from Openai responses
#'
#' @noRd
method(extract_metadata, list(api_gemini,class_list))<- function(.api,.response) {
  list(
    model             = .response$modelVersion,
    timestamp         = lubridate::as_datetime(lubridate::now()),
    prompt_tokens     = .response$usageMetadata$promptTokenCount,
    completion_tokens = .response$usageMetadata$candidatesTokenCount,
    total_tokens      = .response$usageMetadata$totalTokenCount,
    cached_tokens         = as_token_count(.response$usageMetadata$cachedContentTokenCount),
    cache_creation_tokens = NA_integer_,
    stream            = FALSE,
    specific_metadata = list(
      finishReason      = .response$candidates[[1]]$finishReason,
      cachedContentTokenCount = .response$usageMetadata$cachedContentTokenCount,
      avgLogprobs       = .response$candidates[[1]]$avgLogprobs,
      groundingMetadata = .response$candidates[[1]]$groundingMetadata,
      thinking_tokens   = .response$usageMetadata$thoughtsTokenCount
    )
  )
}  

#' A function to get metadata from Openai streaming responses
#'
#' @noRd
method(extract_metadata_stream, list(api_gemini,class_list))<- function(.api,.stream_raw_data) {
  # Under alt=sse the accumulator is a list of parsed events, like every other
  # provider's, rather than the single data.frame the buffer-and-match parser
  # produced. Gemini repeats usageMetadata on later events and only the final
  # one is complete, so take the last event that carries it.
  final_stream_chunk <- .stream_raw_data |>
    purrr::keep(~ !is.null(.x$usageMetadata)) |>
    utils::tail(1) |>
    purrr::pluck(1)

  if (is.null(final_stream_chunk)) {
    final_stream_chunk <- .stream_raw_data[[length(.stream_raw_data)]] %||% list()
  }

  usage <- final_stream_chunk$usageMetadata

  list(
    model             = final_stream_chunk$modelVersion,
    timestamp         = lubridate::as_datetime(lubridate::now()),
    prompt_tokens     = usage$promptTokenCount,
    completion_tokens = usage$candidatesTokenCount,
    total_tokens      = usage$totalTokenCount,
    cached_tokens         = as_token_count(usage$cachedContentTokenCount),
    cache_creation_tokens = NA_integer_,
    stream            = TRUE,
    specific_metadata = list(
      finishReason    = final_stream_chunk$candidates[[1]]$finishReason,
      thinking_tokens = usage$thoughtsTokenCount,
      token_details   = usage
    )
  )
}


#' Method to convert a tidyllm TOOL definition to the expected input for Gemini
#'
#' @noRd
method(tools_to_api, list(api_gemini, class_list)) <- function(.api, .tools) {
  list(
    function_declarations = purrr::map(.tools, function(tool) {
      if (length(tool@builtin) > 0) {
        tool@builtin[[1]]
      } else {
        tool_def <- list(
          name = tool@name,
          description = tool@description
        )
        if (length(tool@input_schema) > 0) {
          tool_def$parameters <- list(
            type = "object",
            properties = purrr::map(tool@input_schema, field_to_param_schema),
            required = names(tool@input_schema)
          )
        }
        tool_def
      }
    })
  )
}

method(has_tool_calls, list(api_gemini, class_any)) <- function(.api, .response) {
  parts <- .response$raw$content$candidates[[1]]$content$parts
  if (is.null(parts)) return(FALSE)
  any(purrr::map_lgl(parts, ~!is.null(.x$functionCall)))
}

method(extract_tool_calls, list(api_gemini, class_any)) <- function(.api, .response) {
  parts <- .response$raw$content$candidates[[1]]$content$parts
  purrr::keep(parts, ~!is.null(.x$functionCall)) |>
    purrr::map(~.x$functionCall)
}

method(append_tool_messages, list(api_gemini, class_any, class_any, class_any)) <-
  function(.api, .request_body, .response, .tool_results) {
    # The model's parts have to go back verbatim: since the thinking models,
    # Gemini rejects a functionCall part whose sibling thoughtSignature was
    # dropped ("Function call is missing a thought_signature").
    model_parts <- .response$raw$content$candidates[[1]]$content$parts
    if (is.null(model_parts)) {
      model_parts <- purrr::map(extract_tool_calls(.api, .response),
                                ~list(functionCall = .x))
    }
    assistant_message <- list(
      role = "model",
      parts = model_parts
    )
    .request_body$contents <- c(
      .request_body$contents,
      list(assistant_message),
      list(.tool_results)
    )
    .request_body
  }

#' A method to run tool calls on Gemini and create the expected response
#'
#' @noRd
method(run_tool_calls, list(api_gemini, class_list, class_list)) <- function(.api, .tool_calls, .tools) {
  # Iterate over each tool call returned by Gemini and build a list of parts.
  tool_parts <- purrr::map(.tool_calls, function(tool_call) {
    # Gemini returns the tool call information in a structure like:
    # list(name = "<tool_name>", args = list(...))
    tool_name <- tool_call$name
    tool_args <- tool_call$args
    
    # Find the corresponding tool in the provided tools list.
    matching_tool <- purrr::keep(.tools, ~ .x@name == tool_name)
    if (length(matching_tool) == 0) {
      warning(sprintf("No matching tool found for: %s", tool_name))
      return(NULL)
    }
    
    tool_function <- matching_tool[[1]]@func
    
    # Execute the tool function with the provided arguments.
    tool_result <- utils::capture.output(
      do.call(tool_function, as.list(tool_args)),
      file = NULL
    ) |> 
      stringr::str_c(collapse = "\n")
    
    # Format the tool response as a part for a single user message.
    list(
      functionResponse = list(
        name = tool_name,
        response = list(
          name = tool_name,
          content = tool_result
        )
      )
    )
  })
  
  tool_parts <- purrr::compact(tool_parts)
  
  # Combine all parts into one user message.
  list(
    role = "user",
    parts = tool_parts
  )
}





#' Inject files into Gemini message contents
#'
#' @param .gemini_contents The existing gemini contents list
#' @param .file_ids A vector or list of file IDs to inject
#' @return Updated gemini_contents with files injected
#' @noRd
gemini_inject_files <- function(.gemini_contents, 
                                .file_ids) {
  # If file_ids is NULL or empty, just return gemini_contents
  if (is.null(.file_ids) || length(.file_ids) == 0) {
    return(.gemini_contents)
  }
  
  # For each file ID, get the metadata and create file_data
  file_data_list <- lapply(.file_ids, function(file_id) {
    file_info <- gemini_file_metadata(file_id)
    list(
      fileData = list(
        fileUri = file_info$uri,
        mimeType = file_info$mime_type
      )
    )
  })
  
  # Now, inject the file_data into the parts of the last user message
  # First, get the last message
  last_msg <- .gemini_contents[[length(.gemini_contents)]]
  
  # Ensure the last message is a user message
  if (last_msg$role != "user") {
    stop("The last message must be a user message to inject files")
  }
  
  # Ensure that last_msg$parts is a list of parts
  if (!is.list(last_msg$parts)) {
    stop("The 'parts' of the last message is not a list")
  }
  
  # Check if last_msg$parts[[1]] is a list; if not, wrap it
  if (!is.list(last_msg$parts[[1]])) {
    last_msg$parts <- list(last_msg$parts)
  }
  
  # Append the file_data_list to the parts
  last_msg$parts <- c(
    last_msg$parts,
    file_data_list
  )
  
  .gemini_contents[[length(.gemini_contents)]] <- last_msg
}





#' Send LLMMessage to Gemini API
#'
#' @param .llm An existing LLMMessage object or an initial text prompt.
#' @param .model The model identifier (default: "gemini-1.5-flash").
#' @param .fileid Optional vector of file IDs uploaded via `gemini_upload_file()` (default: NULL).
#' @param .temperature Controls randomness in generation (default: NULL, range: 0.0-2.0).
#' @param .max_output_tokens Maximum tokens in the response (default: NULL).
#' @param .top_p Controls nucleus sampling (default: NULL, range: 0.0-1.0).
#' @param .top_k Controls diversity in token selection (default: NULL, range: 0 or more).
#' @param .presence_penalty Penalizes new tokens (default: NULL, range: -2.0 to 2.0).
#' @param .frequency_penalty Penalizes frequent tokens (default: NULL, range: -2.0 to 2.0).
#' @param .stop_sequences Optional character sequences to stop generation (default: NULL, up to 5).
#' @param .safety_settings A list of safety settings (default: NULL).
#' @param .json_schema A schema to enforce an output structure
#' @param .grounding_threshold A grounding threshold between 0 and 1. With lower 
#' grounding thresholds  Gemini will use Google to search for relevant information 
#' before answering.  (default: NULL).
#' @param .tools Either a single TOOL object or a list of TOOL objects representing the available functions for tool calls.
#' @param .timeout When should our connection time out (default: 120 seconds).
#' @param .dry_run If TRUE, perform a dry run and return the request object.
#' @param .max_tries Maximum retries to perform request (default: 3).
#' @param .verbose Should additional information be shown after the API call.
#' @param .stream Should the response be streamed (default: FALSE).
#' @param .max_tool_rounds Integer specifying the maximum number of tool use iterations (default: 10).
#'   Set to 1 for single-round tool use, or higher for multi-turn agentic loops.
#' @param .thinking_budget Token budget for internal reasoning (default: NULL). Works with `gemini-3.6-flash` and `gemini-3.1-pro`.
#'
#' @return A new `LLMMessage` object containing the original messages plus the assistant's response.
#'
#' @export
gemini_chat <- function(.llm,
                   .model = "gemini-3.6-flash",
                   .fileid = NULL,
                   .temperature = NULL,
                   .max_output_tokens = NULL,
                   .top_p = NULL,
                   .top_k = NULL,
                   .grounding_threshold = NULL,
                   .presence_penalty = NULL,
                   .frequency_penalty = NULL,
                   .stop_sequences = NULL,
                   .safety_settings = NULL,
                   .json_schema = NULL,
                   .tools = NULL,
                   .thinking_budget = NULL,
                   .timeout = 120,
                   .dry_run = FALSE,
                   .max_tries = 3,
                   .verbose = FALSE,
                   .stream = FALSE,
                   .max_tool_rounds = 10) {
  built <- do.call(gemini_build_chat_request, mget(names(formals())), quote = TRUE)
  run_chat_pipeline(built, .dry_run)
}

#' Build a Gemini chat request without performing it
#'
#' @noRd
gemini_build_chat_request <- function(.llm,
                   .model = "gemini-3.6-flash",
                   .fileid = NULL,
                   .temperature = NULL,
                   .max_output_tokens = NULL,
                   .top_p = NULL,
                   .top_k = NULL,
                   .grounding_threshold = NULL,
                   .presence_penalty = NULL,
                   .frequency_penalty = NULL,
                   .stop_sequences = NULL,
                   .safety_settings = NULL,
                   .json_schema = NULL,
                   .tools = NULL,
                   .thinking_budget = NULL,
                   .timeout = 120,
                   .dry_run = FALSE,
                   .max_tries = 3,
                   .verbose = FALSE,
                   .stream = FALSE,
                   .max_tool_rounds = 10) {

  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
    "Input .model must be a string" = is.character(.model) && length(.model) == 1,
    "Input .fileid must be NULL or a charcater vector of file IDs" = is.null(.fileid) | is.character(.fileid) ,
    "Input .temperature must be NULL or in [0.0, 2.0]" = is.null(.temperature) | (.temperature >= 0.0 & .temperature <= 2.0),
    "Input .max_output_tokens must be NULL or an integer-valued numeric greater than 1" = is.null(.max_output_tokens) | (.max_output_tokens >= 1 & is_integer_valued(.max_output_tokens)),
    "Input .top_p must be NULL or in [0.0, 1.0]" = is.null(.top_p) | (.top_p >= 0.0 & .top_p <= 1.0),
    "Input .top_k must be NULL or non-negative" = is.null(.top_k) | (.top_k >= 0),
    "Input .grounding_threshold must be NULL or in [0.0, 1.0]" = is.null(.grounding_threshold) | (.grounding_threshold >= 0.0 & .grounding_threshold <= 1.0),
    "Input .presence_penalty must be NULL or in [-2.0, 2.0]" = is.null(.presence_penalty) | (.presence_penalty >= -2.0 & .presence_penalty <= 2.0),
    "Input .frequency_penalty must be NULL or in [-2.0, 2.0]" = is.null(.frequency_penalty) | (.frequency_penalty >= -2.0 & .frequency_penalty <= 2.0),
    "Input .stop_sequences must be NULL or a list of up to 5 strings" = is.null(.stop_sequences) | (is.list(.stop_sequences) & length(.stop_sequences) <= 5),
    "Input .safety_settings must be NULL or a list" = is.null(.safety_settings) | is.list(.safety_settings),
    "Input .json_schema must be NULL or a list or an ellmer type object" = is.null(.json_schema) | is.list(.json_schema) | is_ellmer_type(.json_schema),
    "Input .timeout must be an integer-valued numeric and positive" = is_integer_valued(.timeout) & .timeout > 0,
    "Input .max_tries must be integer-valued numeric and positive" = is_integer_valued(.max_tries) & .max_tries > 0,
    "Input .dry_run must be logical" = is.logical(.dry_run),
    "Input .verbose must be logical" = is.logical(.verbose),
    "Input .stream must be logical" = is.logical(.verbose),
    "Input .tools must be NULL, a TOOL object, or a list of TOOL objects" = is.null(.tools) || S7_inherits(.tools, TOOL) || (is.list(.tools) && all(purrr::map_lgl(.tools, ~ S7_inherits(.x, TOOL)))),
    ".max_tool_rounds must be a positive integer" = is_integer_valued(.max_tool_rounds) && .max_tool_rounds >= 1,
    ".thinking_budget must be NULL or a non-negative integer" = is.null(.thinking_budget) || (is_integer_valued(.thinking_budget) && .thinking_budget >= 0)
  ) |>
    validate_inputs()
  
  api_obj <- api_gemini(short_name = "gemini",
                        long_name  = "Google Gemini",
                        api_key_env_var = "GOOGLE_API_KEY")
  
  api_key <- get_api_key(api_obj,.dry_run)
  
  # Deprecated .fileid: convert to tidyllm_file objects and inject into last message
  if (!is.null(.fileid)) {
    # See the note on the matching call in R/api_claude.R: the builder is one
    # frame deeper than gemini_chat(), so user_env must be passed explicitly.
    lifecycle::deprecate_warn(
      "0.5.0", "gemini(.fileid=)",
      details = "Pass tidyllm_file objects via .files on llm_message() instead.",
      user_env = rlang::caller_env(2)
    )
    file_objs <- lapply(.fileid, function(id) {
      meta <- tryCatch(gemini_file_metadata(id), error = function(e) NULL)
      uri  <- if (!is.null(meta)) meta$uri[1] else ""
      mime <- if (!is.null(meta)) meta$mime_type[1] else ""
      tidyllm_file(id = id, provider = "gemini", mime_type = mime,
                   filename = basename(id), uri = uri)
    })
    .llm <- inject_files_into_last_message(.llm, file_objs)
  }

  gemini_contents <- to_api_format(.llm, api_obj)
  
  # Handle JSON schema
  response_format <- NULL
  json=FALSE
  if (requireNamespace("ellmer", quietly = TRUE)) {
    #Handle ellmer json schemata Objects
    if(S7_inherits(.json_schema,ellmer::TypeObject)){
      .json_schema = to_schema(.json_schema)
    }
  }
  if (!is.null(.json_schema)) {
    json=TRUE
    # Gemini rejects additionalProperties on every node, not just the root
    .json_schema <- remove_extra_fields_key(.json_schema)
    response_format <- list(
      response_mime_type = "application/json",
      response_schema = .json_schema
    )
  } 
  
  
  #Put a single tool into a list if only one is provided.
  raw_tools_def <- if (!is.null(.tools)) {
    if (S7_inherits(.tools, TOOL)) list(.tools) else .tools
  } else {
    NULL
  }
  tools_def <- if (!is.null(raw_tools_def)) {
    tools_to_api(api_obj, raw_tools_def)
  } else {
    NULL
  }
  
  # Add grounding tool configuration if grounding_threshold is set
  if (!is.null(.grounding_threshold)) {
    grounding_tool <- list(
      google_search_retrieval = list(
        dynamic_retrieval_config = list(
          mode = "MODE_DYNAMIC",
          dynamic_threshold = .grounding_threshold
        )
      )
    )
    if(!is.null(tools_def)){
      tools_def <- tools_def |>
        append(list(grounding_tool))
    } else {
      tools_def <- list(grounding_tool)
    }
  }
  
  #Handle system prompt
  system_prompt <- list(parts = list(
    text = filter_roles(.llm@message_history, c("system"))[[1]]$content
    ))
  
  # Build generationConfig
  generation_config <- list(
    temperature = .temperature,
    maxOutputTokens = .max_output_tokens,
    topP = .top_p,
    topK = .top_k,
    presencePenalty = .presence_penalty,
    frequencyPenalty = .frequency_penalty,
    stopSequences = .stop_sequences,
    thinkingConfig = if (!is.null(.thinking_budget)) list(thinkingBudget = .thinking_budget) else NULL
  ) |>
    append(response_format) |>
    purrr::compact()
  
  # Construct the request body
  request_body <- list(
    model = .model,
    system_instruction = system_prompt,
    contents = gemini_contents,
    generationConfig = generation_config,
    safetySettings = .safety_settings,
    tools = tools_def
  ) |>
    purrr::compact()


  
  if(.stream==FALSE) request_type <- ":generateContent"
  if(.stream==TRUE)  request_type <- ":streamGenerateContent"

  # Build the request
  request <- httr2::request("https://generativelanguage.googleapis.com") |>
    httr2::req_url_path(paste0("/v1beta/models/", .model, request_type)) |>
    httr2::req_headers_redacted(`x-goog-api-key` = api_key) |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_json(request_body)

  # Without alt=sse the streaming endpoint returns a pretty-printed JSON array
  # in chunks, with no SSE framing at all; with it, ordinary text/event-stream.
  # This one query parameter is what lets the shared pump read Gemini.
  if (.stream) request <- httr2::req_url_query(request, alt = "sse")

  new_chat_request(
    .request          = request,
    .api              = api_obj,
    .llm              = .llm,
    .body             = request_body,
    .tools_def        = raw_tools_def,
    .json             = json,
    .mode             = if (isTRUE(.stream)) "stream" else "value",
    .timeout          = .timeout,
    .max_tries        = .max_tries,
    .max_tool_rounds  = .max_tool_rounds,
    .verbose          = .verbose
  )
}



#' Upload a File to Gemini API
#'
#' Uploads a file to the Gemini API and returns its metadata as a tibble.
#'
#' @param .file_path The local file path of the file to upload.
#' @return A tibble containing metadata about the uploaded file, including its name, URI, and MIME type.
#' @export
gemini_upload_file <- function(.file_path) {
  lifecycle::deprecate_warn("0.5.0", "gemini_upload_file()", "upload_file()",
    details = "Use upload_file(gemini(), .path = ...) instead.")
  mime_type <- guess_mime_type(.file_path)
  num_bytes <- file.info(.file_path)$size
  display_name <- basename(.file_path)
  
  # Retrieve API key
  api_key <- Sys.getenv("GOOGLE_API_KEY")
  if ((api_key == "")) {
    stop("API key is not set. Please set it with: Sys.setenv(GOOGLE_API_KEY = 'YOUR-KEY-GOES-HERE')")
  }
  
  # Step 1: Initiate the upload and get the resumable upload URL
  init_response <- httr2::request("https://generativelanguage.googleapis.com/upload/v1beta/files") |>
    httr2::req_headers_redacted(`x-goog-api-key` = api_key) |>
    httr2::req_headers(
      `X-Goog-Upload-Protocol` = "resumable",
      `X-Goog-Upload-Command` = "start",
      `X-Goog-Upload-Header-Content-Length` = as.character(num_bytes),
      `X-Goog-Upload-Header-Content-Type` = mime_type,
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(list(file = list(display_name = display_name))) |>
    httr2::req_perform()
  
  upload_url <- httr2::resp_header(init_response, "x-goog-upload-url")
  
  if (is.null(upload_url)) {
    stop("Failed to get upload URL")
  }
  
  # Step 2: Upload the file bytes
  response <- httr2::request(upload_url) |>
    httr2::req_headers(
      `Content-Length` = as.character(num_bytes),
      `X-Goog-Upload-Offset` = "0",
      `X-Goog-Upload-Command` = "upload, finalize"
    ) |>
    httr2::req_body_raw(readBin(.file_path, "raw", num_bytes)) |>
    httr2::req_progress(type = "up") |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  
  tibble::tibble(
    name = response$file$name,
    display_name = response$file$displayName,
    mime_type = response$file$mimeType,
    size_bytes = as.numeric(response$file$sizeBytes),
    create_time = response$file$createTime,
    uri = response$file$uri,
    state = response$file$state
  )
}

#' Retrieve Metadata for a File from Gemini API
#'
#' Retrieves metadata for a specific file uploaded to the Gemini API.
#'
#' @param .file_name The file ID (e.g., "files/abc-123") to retrieve metadata for.
#' @return A tibble containing metadata fields such as name, display name, MIME type, size, and URI.
#' @export
gemini_file_metadata <- function(.file_name) {
  lifecycle::deprecate_warn("0.5.0", "gemini_file_metadata()", "file_info()",
    details = "Use file_info(gemini(), .file_id = ...) instead.")
  api_key <- Sys.getenv("GOOGLE_API_KEY")
  if (api_key == "") {
    stop("API key is not set. Please set it with: Sys.setenv(GOOGLE_API_KEY = 'YOUR-KEY-GOES-HERE')")
  }
  
  # Request to get file metadata
  response <- httr2::request(paste0("https://generativelanguage.googleapis.com/v1beta/", .file_name)) |>
    httr2::req_headers_redacted(`x-goog-api-key` = api_key) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  
  # Convert the response to a tibble
  tibble::tibble(
    name = response$name,
    display_name = response$displayName,
    mime_type = response$mimeType,
    size_bytes = as.numeric(response$sizeBytes),
    create_time = response$createTime,
    update_time = response$updateTime,
    expiration_time = response$expirationTime,
    sha256_hash = response$sha256Hash,
    uri = response$uri,
    state = response$state
  )
}


#' List Files in Gemini API
#'
#' Lists metadata for files uploaded to the Gemini API, supporting pagination.
#'
#' @param .page_size The maximum number of files to return per page (default: 10, maximum: 100).
#' @param .page_token A token for fetching the next page of results (default: NULL).
#' @return A tibble containing metadata for each file, including fields such as name, display name, MIME type, and URI.
#' @export
gemini_list_files <- function(.page_size = 10,
                              .page_token = NULL) {
  lifecycle::deprecate_warn("0.5.0", "gemini_list_files()", "list_files()",
    details = "Use list_files(gemini()) instead.")
  api_key <- Sys.getenv("GOOGLE_API_KEY")
  if (api_key == "") {
    stop("API key is not set. Please set it with: Sys.setenv(GOOGLE_API_KEY = 'YOUR-KEY-GOES-HERE')")
  }
  
  # Request to list files
  response <- httr2::request("https://generativelanguage.googleapis.com/v1beta/files") |>
    httr2::req_headers_redacted(`x-goog-api-key` = api_key) |>
    httr2::req_url_query(pageSize = .page_size, pageToken = .page_token) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  
  files <- response$files
  tibble::tibble(
    name = sapply(files, function(x) x$name),
    display_name = sapply(files, function(x) x$displayName),
    mime_type = sapply(files, function(x) x$mimeType),
    size_bytes = as.numeric(sapply(files, function(x) x$sizeBytes)),
    create_time = sapply(files, function(x) x$createTime),
    update_time = sapply(files, function(x) x$updateTime),
    expiration_time = sapply(files, function(x) x$expirationTime),
    sha256_hash = sapply(files, function(x) x$sha256Hash),
    uri = sapply(files, function(x) x$uri),
    state = sapply(files, function(x) x$state)
  )
}

#' Delete a File from Gemini API
#'
#' Deletes a specific file from the Gemini API using its file ID.
#'
#' @param .file_name The file ID (e.g., "files/abc-123") to delete.
#' @return Invisibly returns `NULL`. Prints a confirmation message upon successful deletion.
#' @export
gemini_delete_file <- function(.file_name) {
  lifecycle::deprecate_warn("0.5.0", "gemini_delete_file()", "delete_file()",
    details = "Use delete_file(gemini(), .file_id = ...) instead.")
  api_key <- Sys.getenv("GOOGLE_API_KEY")
  if (api_key == "") {
    stop("API key is not set. Please set it with: Sys.setenv(GOOGLE_API_KEY = 'YOUR-KEY-GOES-HERE')")
  }
  
  # Request to delete the file
  httr2::request(paste0("https://generativelanguage.googleapis.com/v1beta/", .file_name)) |>
    httr2::req_headers_redacted(`x-goog-api-key` = api_key) |>
    httr2::req_method("DELETE") |>
    httr2::req_perform()
  
  message("File ", .file_name, " has been successfully deleted.")
}


#' Generate Embeddings Using the Google Gemini API
#'
#' @param .input  A character vector of texts to embed or an `LLMMessage` object
#' @param .model The embedding model identifier (default: "gemini-embedding-2").
#' @param .truncate Whether to truncate inputs to fit the model's context length (default: TRUE).
#' @param .timeout Timeout for the API request in seconds (default: 120).
#' @param .dry_run If TRUE, perform a dry run and return the request object.
#' @param .max_tries Maximum retry attempts for requests (default: 3).
#' @return A matrix where each column corresponds to the embedding of a message in the message history.
#' @export
gemini_embedding <- function(.input,
                             .model = "gemini-embedding-2",
                             .truncate = TRUE,
                             .timeout = 120,
                             .dry_run = FALSE,
                             .max_tries = 3) {

  # Get the API key
  api_key <- Sys.getenv("GOOGLE_API_KEY")
  if ((api_key == "") & .dry_run == FALSE) {
    stop("API key is not set. Please set it with: Sys.setenv(GOOGLE_API_KEY = 'YOUR-KEY-GOES-HERE')")
  }
  
  # Validate inputs
  c(
    "Input .input must be a character vector or an LLMMessage object" = S7_inherits(.input, LLMMessage) | is.character(.input),
    "Input .model must be a string" = is.character(.model),
    "Input .truncate must be logical" = is.logical(.truncate),
    "Input .timeout must be a positive numeric value" = is.numeric(.timeout) && .timeout > 0,
    ".dry_run must be logical" = is.logical(.dry_run)
  ) |> validate_inputs()
  
  # Prepare message texts
  input_texts <- parse_embedding_input(.input)
  
  # Prepare batch request
  request_body <- list(
    requests = lapply(input_texts, function(text) {
      list(
        model = paste0("models/",.model),
        content = list(
          parts = list(
            list(text = text)
          )
        )
      )
    })
  )
  
  # Build the request
  request <-httr2::request("https://generativelanguage.googleapis.com") |>
    httr2::req_url_path(paste0("/v1beta/models/", .model, ":batchEmbedContents")) |>
    httr2::req_headers_redacted(`x-goog-api-key` = api_key) |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_json(request_body)
  
  # Dry run
  if (.dry_run) {
    return(request)
  }
  
  extract_embeddings_fn <- function(response_content,error,headers){
    if(error){
      paste0("API error response - ", response_content$error$message) |>
        stop()
    }
    response_content$embeddings |>
      purrr::map(unlist)
  }
  
  # Perform a standard embedding API request
  perform_embedding_request(.request = request,
                            .timeout = .timeout,
                            .max_tries = 3,
                            .input_texts = input_texts, 
                            .fn_extract_embeddings = extract_embeddings_fn)
}

#' Submit a list of LLMMessage objects to Gemini's batch API
#'
#' Returns a named list (same as input) with batch_id and json attributes.
#' @param .llms List of LLMMessage objects (named or unnamed).
#' @param .model The model identifier (default: "gemini-1.5-flash").
#' @param .temperature Controls randomness (default: NULL, range: 0-2).
#' @param .max_output_tokens Maximum tokens in the response (default: NULL).
#' @param .top_p Nucleus sampling (default: NULL, range: 0-1).
#' @param .top_k Diversity in token selection (default: NULL).
#' @param .presence_penalty Penalizes new tokens (default: NULL, -2 to 2).
#' @param .frequency_penalty Penalizes frequent tokens (default: NULL, -2 to 2).
#' @param .stop_sequences Character vector or NULL of up to 5.
#' @param .safety_settings Optional list of safety settings (default: NULL).
#' @param .json_schema Optional schema to enforce output structure.
#' @param .grounding_threshold Optional grounding threshold (0-1) to enable Google Search.
#' @param .timeout Timeout in seconds (default: 120).
#' @param .dry_run If TRUE, returns the constructed request (default: FALSE).
#' @param .max_tries Maximum retry attempts (default: 3).
#' @param .display Display name for this batch (default: "tidyllm_batch").
#' @param .id_prefix Prefix for message IDs (default: "tidyllm_gemini_req_").
#' @return Named list of LLMMessage objects with attributes `batch_id` and `json`
#' @export
send_gemini_batch <- function(.llms,
                              .model = "gemini-3.6-flash",
                              .temperature = NULL,
                              .max_output_tokens = NULL,
                              .top_p = NULL,
                              .top_k = NULL,
                              .presence_penalty = NULL,
                              .frequency_penalty = NULL,
                              .stop_sequences = NULL,
                              .safety_settings = NULL,
                              .json_schema = NULL,
                              .grounding_threshold = NULL,
                              .timeout = 120,
                              .dry_run = FALSE,
                              .max_tries = 3,
                              .display = "tidyllm_batch",
                              .id_prefix = "tidyllm_gemini_req_") {
  
  # --- Input validation ---
  c(
    "Input .llms must be a list of LLMMessage objects" =
      is.list(.llms) && all(vapply(.llms, S7_inherits, logical(1), LLMMessage)),
    "Input .model must be a string" = is.character(.model) && length(.model) == 1,
    "Input .temperature must be NULL or in [0.0, 2.0]" =
      is.null(.temperature) || (.temperature >= 0.0 && .temperature <= 2.0),
    "Input .max_output_tokens must be NULL or integer >= 1" =
      is.null(.max_output_tokens) || (is_integer_valued(.max_output_tokens) && .max_output_tokens >= 1),
    "Input .top_p must be NULL or in [0.0, 1.0]" =
      is.null(.top_p) || (.top_p >= 0.0 && .top_p <= 1.0),
    "Input .top_k must be NULL or non-negative" =
      is.null(.top_k) || .top_k >= 0,
    "Input .presence_penalty must be NULL or in [-2.0, 2.0]" =
      is.null(.presence_penalty) || (.presence_penalty >= -2.0 && .presence_penalty <= 2.0),
    "Input .frequency_penalty must be NULL or in [-2.0, 2.0]" =
      is.null(.frequency_penalty) || (.frequency_penalty >= -2.0 && .frequency_penalty <= 2.0),
    "Input .stop_sequences must be NULL or a character vector of less or equal 5" =
      is.null(.stop_sequences) || (is.character(.stop_sequences) && length(.stop_sequences) <= 5),
    "Input .safety_settings must be NULL or a list" =
      is.null(.safety_settings) || is.list(.safety_settings),
    "Input .json_schema must be NULL, a list, or an ellmer type" =
      is.null(.json_schema) || is.list(.json_schema) || is_ellmer_type(.json_schema),
    "Input .grounding_threshold must be NULL or in [0.0, 1.0]" =
      is.null(.grounding_threshold) || (.grounding_threshold >= 0.0 && .grounding_threshold <= 1.0),
    "Input .timeout must be a positive integer" =
      is_integer_valued(.timeout) && .timeout > 0,
    "Input .max_tries must be a positive integer" =
      is_integer_valued(.max_tries) && .max_tries > 0,
    "Input .dry_run must be logical" = is.logical(.dry_run)
  ) |> validate_inputs()
  
  # --- Auto-name messages if needed ---
  if (is.null(names(.llms)) || anyNA(names(.llms)) || any(names(.llms) == "")) {
    names(.llms) <- sprintf("%s%s", .id_prefix, seq_along(.llms))
  }
  ids <- names(.llms)
  
  # --- Schema handling ---
  json <- FALSE
  if (requireNamespace("ellmer", quietly = TRUE)) {
    if (S7_inherits(.json_schema, ellmer::TypeObject)) {
      .json_schema <- to_schema(.json_schema)
    }
  }
  if (!is.null(.json_schema)) {
    json <- TRUE
    .json_schema <- remove_extra_fields_key(.json_schema)
  }
  
  # --- Generation config ---
  gen_config <- list(
    temperature = .temperature,
    topP = .top_p,
    topK = .top_k,
    presencePenalty = .presence_penalty,
    frequencyPenalty = .frequency_penalty,
    maxOutputTokens = .max_output_tokens,
    stopSequences = .stop_sequences
  )
  if (json) {
    gen_config$response_mime_type <- "application/json"
    gen_config$response_schema <- .json_schema
  }
  gen_config <- purrr::compact(gen_config)
  
  # --- Only Google search grounding tool if specified ---
  grounding_tool <- if (!is.null(.grounding_threshold)) {
    list(
      google_search_retrieval = list(
        dynamic_retrieval_config = list(
          mode = "MODE_DYNAMIC",
          dynamic_threshold = .grounding_threshold
        )
      )
    )
  } else NULL
  
  # --- API key and API object ---
  api_obj <- api_gemini(short_name = "gemini",
                        long_name = "Google Gemini",
                        api_key_env_var = "GOOGLE_API_KEY")
  api_key <- get_api_key(api_obj, .dry_run)
  
  # --- Build batch requests, tagged with names as keys ---
  requests <- purrr::imap(.llms, function(msg, id) {
    list(
      request = purrr::compact(list(
        contents = to_api_format(msg, api_obj),
        system_instruction = list(parts = list(text = msg@system_prompt)),
        generationConfig = gen_config,
        safetySettings = .safety_settings,
        tools = if (!is.null(grounding_tool)) list(grounding_tool)
      )),
      metadata = list(key = id)
    )
  })
  
  # --- Final body for Gemini batch ---
  body <- list(
    batch = list(
      display_name = .display,
      input_config = list(
        requests = list(requests = unname(requests))
      )
    )
  )
  
  req <- httr2::request("https://generativelanguage.googleapis.com") |>
    httr2::req_url_path(paste0("/v1beta/models/", .model, ":batchGenerateContent")) |>
    httr2::req_headers_redacted(`x-goog-api-key` = api_key) |>
    httr2::req_body_json(body)
  
  if (.dry_run) return(req)
  
  resp <- perform_generic_request(req, .timeout, .max_tries)
  
  # --- Surface Gemini API errors as R errors ---
  if (!is.null(resp$content$error)) {
    err <- resp$content$error
    stop(sprintf("Gemini batch request failed [%s]: %s\n%s",
                 err$status, err$message,
                 if (!is.null(err$details[[1]]$links[[1]]$url)) err$details[[1]]$links[[1]]$url else ""
    ),
    call. = FALSE
    )
  }
  
  batch_id <- resp$content$name
  
  # --- Attach attributes, return named list (same as Claude) ---
  attr(.llms, "batch_id") <- batch_id
  attr(.llms, "json") <- json
  .llms
}



#' Check the Status of a Gemini Batch Operation
#'
#' Retrieves processing status and metadata for a Gemini batch operation.
#'
#' You can supply either the `.batch_id` string (e.g. `"batches/xyz..."`) **or**
#' a list of LLMMessage objects (`.llms`) with a `"batch_id"` attribute as returned by `send_gemini_batch()`.
#'
#' @param .llms (Optional) List of LLMMessage objects with a `"batch_id"` attribute (as returned by `send_gemini_batch()`).
#' @param .batch_id (Optional) Character string: full batch operation name, e.g. `"batches/xyz123"`.
#'   If both `.llms` and `.batch_id` are provided, `.batch_id` is used.
#' @param .timeout Integer. Request timeout in seconds. Default: 60.
#' @param .max_tries Integer. Maximum retry attempts. Default: 3.
#' @param .dry_run Logical. If TRUE, return the request object instead of making the request (for debugging). Default: FALSE.
#' @return A tibble with the operation's metadata, including name, state, creation time, completion time, and done status.
#' @export
check_gemini_batch <- function(.llms = NULL,
                               .batch_id = NULL,
                               .timeout = 60,
                               .max_tries = 3,
                               .dry_run = FALSE) {
  # If .batch_id missing, try to get from .llms 
  if (is.null(.batch_id)) {
    if (!is.null(.llms)) {
      .batch_id <- attr(.llms, "batch_id")
      if (is.null(.batch_id)) {
        stop("No batch_id attribute found in provided .llms object. Use send_gemini_batch() to generate batch_id.")
      }
    } else {
      stop("Must provide either .batch_id or .llms with a batch_id attribute.")
    }
  }

  
  api_key <- Sys.getenv("GOOGLE_API_KEY")
  if (api_key == "") stop("Google API key is not set (GOOGLE_API_KEY).")
  op_path <- if (!startsWith(.batch_id, "/v1beta/")) paste0("/v1beta/", .batch_id) else .batch_id
  req <- httr2::request("https://generativelanguage.googleapis.com") |>
    httr2::req_url_path(op_path) |>
    httr2::req_headers_redacted(`x-goog-api-key` = api_key)
  if (.dry_run) return(req)
  resp <- perform_generic_request(req, .timeout, .max_tries)
  content <- resp$content
  if (!is.null(content$error)) {
    stop(sprintf("Gemini API Error: %s - %s", content$error$code, content$error$message))
  }
  tibble::tibble(
    name = content$name,
    state = purrr::pluck(content, "metadata", "state", .default=NA_character_),
    done = purrr::pluck(content, "done", .default=NA),
    create_time = lubridate::ymd_hms(purrr::pluck(content, "metadata", "createTime", .default=NA_character_), tz="UTC"),
    complete_time = lubridate::ymd_hms(purrr::pluck(content, "metadata", "completeTime", .default=NA_character_), tz="UTC")
  )
}
#' List Recent Gemini Batch Operations
#'
#' Returns a tibble with recent Gemini batch operations and their metadata.
#'
#' @param .filter Optional filter expression for batch listing (see Gemini API docs).
#' @param .page_size Integer. Maximum number of results to return. Default: 20.
#' @param .timeout Integer. Request timeout in seconds. Default: 60.
#' @param .max_tries Integer. Maximum retry attempts. Default: 3.
#' @param .dry_run Logical. If TRUE, returns the request object (for debugging). Default: FALSE.
#' @return A tibble with columns: name, state, done, create_time, complete_time.
#' @export
list_gemini_batches <- function(.filter    = NULL,
                                .page_size = 20,
                                .timeout   = 60,
                                .max_tries = 3,
                                .dry_run   = FALSE) {
  api_key <- Sys.getenv("GOOGLE_API_KEY")
  if (api_key == "") stop("Google API key is not set (GOOGLE_API_KEY).")
  req <- httr2::request("https://generativelanguage.googleapis.com") |>
    httr2::req_url_path("/v1beta/batches") |>
    httr2::req_headers_redacted(`x-goog-api-key` = api_key) |>
    httr2::req_url_query(
      filter   = .filter,
      pageSize = .page_size
    )
  if (.dry_run) return(req)
  resp <- perform_generic_request(req, .timeout, .max_tries)
  content <- resp$content
  if (!is.null(content$error)) {
    stop(sprintf("Gemini API Error: %s - %s", content$error$code, content$error$message))
  }
  batches <- purrr::pluck(content, "operations", .default = list())
  purrr::map_dfr(batches, function(x) tibble::tibble(
    name = x$name,
    state = purrr::pluck(x, "metadata", "state", .default=NA_character_),
    done = purrr::pluck(x, "done", .default=NA),
    create_time = lubridate::ymd_hms(purrr::pluck(x, "metadata", "createTime", .default=NA_character_), tz="UTC"),
    complete_time = lubridate::ymd_hms(purrr::pluck(x, "metadata", "completeTime", .default=NA_character_), tz="UTC")
  ))
}

#' Fetch Results for a Gemini Batch
#'
#' Retrieves the results of a completed Gemini batch and updates
#' the provided list of LLMMessage objects with the assistant's responses,
#' matching by original list order.
#'
#' @param .llms List of `LLMMessage` objects (as from `send_gemini_batch()`), must have a `batch_id` attribute if `.batch_name` is not given.
#' @param .batch_name (Optional) Character; batch operation name (e.g. "batches/xyz123"). If not provided, is taken from `attr(.llms, "batch_id")`.
#' @param .timeout Integer; request timeout in seconds (default: 60).
#' @param .max_tries Integer; maximum retry attempts (default: 3).
#' @param .dry_run Logical; if `TRUE`, returns the GET request object (default: FALSE).
#'
#' @return A list of updated LLMMessage objects with the assistant response appended to each, in the same order.
#' @export
fetch_gemini_batch <- function(.llms,
                               .batch_name = NULL,
                               .timeout = 60,
                               .max_tries = 3,
                               .dry_run = FALSE){
  
  # Preserve original names
  original_names <- names(.llms)
  if(!is.null(attr(.llms,"json"))) json <- attr(.llms,"json") else json <- FALSE
  
  # Validate inputs
  stopifnot(is.list(.llms), !is.null(.llms[[1]]))
  
  # Retrieve batch name
  if (is.null(.batch_name)) {
    .batch_name <- attr(.llms, "batch_id")
    if (is.null(.batch_name)) stop("No batch_name provided and no batch_id attribute found in .llms.")
  }
  
  api_obj <- api_gemini(short_name = "gemini",
                        long_name  = "Google Gemini",
                        api_key_env_var = "GOOGLE_API_KEY")
  
  api_key <- get_api_key(api_obj,.dry_run)
  
  op_path <- if (!startsWith(.batch_name, "/v1beta/")) paste0("/v1beta/", .batch_name) else .batch_name
  
  # Build GET request for batch status
  req <- httr2::request("https://generativelanguage.googleapis.com") |>
    httr2::req_url_path(op_path) |>
    httr2::req_headers_redacted(`x-goog-api-key` = api_key) |>
    httr2::req_headers(`Content-Type` = "application/json")
  
  if (.dry_run) return(req)
  
  resp <- perform_generic_request(req, .timeout, .max_tries)
  content <- resp$content
  
  # Handle batch not done or error state
  batch_state <- purrr::pluck(content, "metadata", "state", .default = "UNKNOWN")
  is_done <- purrr::pluck(content, "done", .default = FALSE)
  
  if (!is_done) stop(sprintf("Batch not finished processing (state: %s)", batch_state))
  
  if (batch_state == "JOB_STATE_FAILED") {
    stop(sprintf("Batch failed: %s", jsonlite::toJSON(content$error, auto_unbox=TRUE)))
  }
  if (batch_state == "JOB_STATE_CANCELLED") {
    stop("Batch was cancelled by the user.")
  }
  
  # Get responses: either inline or downloadable file
  response <- content$response
  responses <- NULL
  
  if (!is.null(response$inlinedResponses)) {
    # Inline responses: take as-is
    responses <- response$inlinedResponses$inlinedResponses
  } else if (!is.null(response$responsesFile)) {
    # Download responses file (JSONL format)
    file_path <- paste0("/download/v1beta/", response$responsesFile, ":download")
    download_req <- httr2::request("https://generativelanguage.googleapis.com") |>
      httr2::req_url_path(file_path) |>
      httr2::req_headers_redacted(`x-goog-api-key` = api_key) |>
      httr2::req_url_query(alt = "media")
    download_resp <- perform_generic_request(download_req, .timeout, .max_tries)
    responses_lines <- strsplit(httr2::resp_body_string(download_resp$raw_response), "\n")[[1]]
    responses <- lapply(responses_lines, function(line) if (nzchar(line)) jsonlite::fromJSON(line) else NULL)
    responses <- Filter(Negate(is.null), responses)
  } else {
    stop("No batch responses found in Gemini batch response.")
  }
  
  # Gemini batches preserve order; map responses to .llms in order
  if (length(responses) != length(.llms)) {
    stop(sprintf("Number of responses (%d) does not match number of LLM messages (%d).", length(responses), length(.llms)))
  }
  
  
  # Update LLMMessage objects with responses
  # Map results back to the original .llms list using names as custom IDs
  updated_llms <- purrr::imap(names(.llms),function(x,y){
    chat_response <- parse_chat_response(api_obj,responses[[y]]$response) 
    
    llm <- add_message(.llm = .llms[[x]],
                .role = "assistant", 
                .content = chat_response,
                .json = json,
                .meta = extract_metadata(api_obj,responses[[y]]$response) )
    llm
  }) 
  
  
  # Return updated list, with batch_id and json attributes removed
  attr(updated_llms, "batch_id") <- NULL
  attr(updated_llms, "json") <- NULL
  names(updated_llms) <- original_names
  updated_llms
}


#' List Available Models from the Google Gemini API
#'
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .max_tries Maximum number of retries for the API request (default: 3).
#' @param .dry_run Logical; if TRUE, returns the prepared request object without executing it.
#'
#' @return A tibble containing model information with columns including `name`, `base_model_id`, 
#'   `version`, `display_name`, `description`, `input_token_limit`, `output_token_limit`, 
#'   `supported_generation_methods`, `thinking`, `temperature`, `max_temperature`, `top_p`, and `top_k`,
#'   or NULL if no models are found.
#'
#' @export
gemini_list_models <- function(.timeout = 60,
                               .max_tries = 3,
                               .dry_run = FALSE) {
  
  api_obj <- api_gemini(short_name = "gemini",
                        long_name  = "Google Gemini",
                        api_key_env_var = "GOOGLE_API_KEY")
  
  api_key <- get_api_key(api_obj, .dry_run)
  
  request <- httr2::request("https://generativelanguage.googleapis.com") |>
    httr2::req_url_path("/v1beta/models") |>
    httr2::req_headers_redacted(`x-goog-api-key` = api_key)
  
  if (.dry_run) {
    return(request)
  }
  
  response <- request |>
    httr2::req_timeout(.timeout) |>
    httr2::req_retry(max_tries = .max_tries) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  
  if (!is.null(response$models)) {
    models <- response$models
    
    model_info <- purrr::map_dfr(models, function(m) {
      tibble::tibble(
        name = m$name,
        display_name = purrr::pluck(m, "displayName", .default = NA_character_),
        version = purrr::pluck(m, "version", .default = NA_character_),
        description = purrr::pluck(m, "description", .default = NA_character_),
        input_token_limit = purrr::pluck(m, "inputTokenLimit", .default = NA_integer_),
        output_token_limit = purrr::pluck(m, "outputTokenLimit", .default = NA_integer_),
        supported_generation_methods = list(purrr::pluck(m, "supportedGenerationMethods", .default = list())),
        thinking = purrr::pluck(m, "thinking", .default = NA),
        temperature = purrr::pluck(m, "temperature", .default = NA_real_),
        max_temperature = purrr::pluck(m, "maxTemperature", .default = NA_real_),
        top_p = purrr::pluck(m, "topP", .default = NA_real_),
        top_k = purrr::pluck(m, "topK", .default = NA_integer_)
      )
    })
    
    return(model_info)
  } else {
    return(NULL)
  }
}




#' Upload a file to Gemini's Files API (verb dispatch wrapper, normalized columns)
#' @noRd
gemini_upload_file_verb <- function(.path, .called_from = NULL, ...) {
  api_key <- Sys.getenv("GOOGLE_API_KEY")
  if (api_key == "") stop("GOOGLE_API_KEY is not set.")
  mime_type    <- guess_mime_type(.path)
  num_bytes    <- file.info(.path)$size
  display_name <- basename(.path)

  init_response <- httr2::request("https://generativelanguage.googleapis.com/upload/v1beta/files") |>
    httr2::req_headers_redacted(`x-goog-api-key` = api_key) |>
    httr2::req_headers(
      `X-Goog-Upload-Protocol` = "resumable",
      `X-Goog-Upload-Command` = "start",
      `X-Goog-Upload-Header-Content-Length` = as.character(num_bytes),
      `X-Goog-Upload-Header-Content-Type` = mime_type,
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(list(file = list(display_name = display_name))) |>
    httr2::req_perform()

  upload_url <- httr2::resp_header(init_response, "x-goog-upload-url")
  if (is.null(upload_url)) stop("Failed to get Gemini upload URL.")

  resp <- httr2::request(upload_url) |>
    httr2::req_headers(
      `Content-Length` = as.character(num_bytes),
      `X-Goog-Upload-Offset` = "0",
      `X-Goog-Upload-Command` = "upload, finalize"
    ) |>
    httr2::req_body_raw(readBin(.path, "raw", num_bytes)) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  f <- resp$file
  tidyllm_file(
    id        = f$name,
    provider  = "gemini",
    mime_type = f$mimeType,
    filename  = f$displayName,
    uri       = f$uri
  )
}

#' List files on Gemini's Files API (verb dispatch wrapper, normalized columns)
#' @noRd
gemini_list_files_verb <- function(.called_from = NULL, .page_size = 10, .page_token = NULL, ...) {
  api_key <- Sys.getenv("GOOGLE_API_KEY")
  if (api_key == "") stop("GOOGLE_API_KEY is not set.")
  req <- httr2::request("https://generativelanguage.googleapis.com/v1beta/files") |>
    httr2::req_headers_redacted(`x-goog-api-key` = api_key) |>
    httr2::req_url_query(pageSize = .page_size)
  if (!is.null(.page_token)) req <- httr2::req_url_query(req, pageToken = .page_token)
  resp <- req |> httr2::req_perform() |> httr2::resp_body_json()
  if (is.null(resp$files)) return(tibble::tibble(file_id=character(), filename=character(),
                                                  mime_type=character(), size_bytes=numeric(),
                                                  created_at=character(), expires_at=character(), uri=character()))
  purrr::map_dfr(resp$files, function(f) tibble::tibble(
    file_id    = f$name %||% NA_character_,
    filename   = f$displayName %||% NA_character_,
    mime_type  = f$mimeType %||% NA_character_,
    size_bytes = as.numeric(f$sizeBytes %||% NA),
    created_at = f$createTime %||% NA_character_,
    expires_at = f$expirationTime %||% NA_character_,
    uri        = f$uri %||% NA_character_
  ))
}

#' Get metadata for a Gemini file (verb dispatch wrapper, normalized columns)
#' @noRd
gemini_file_info_verb <- function(.file_id, .called_from = NULL, ...) {
  api_key <- Sys.getenv("GOOGLE_API_KEY")
  if (api_key == "") stop("GOOGLE_API_KEY is not set.")
  resp <- httr2::request(paste0("https://generativelanguage.googleapis.com/v1beta/", .file_id)) |>
    httr2::req_headers_redacted(`x-goog-api-key` = api_key) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  tibble::tibble(
    file_id    = resp$name %||% NA_character_,
    filename   = resp$displayName %||% NA_character_,
    mime_type  = resp$mimeType %||% NA_character_,
    size_bytes = as.numeric(resp$sizeBytes %||% NA),
    created_at = resp$createTime %||% NA_character_,
    expires_at = resp$expirationTime %||% NA_character_,
    uri        = resp$uri %||% NA_character_
  )
}

#' Delete a file from Gemini's Files API (verb dispatch wrapper)
#' @noRd
gemini_delete_file_verb <- function(.file_id, .called_from = NULL, ...) {
  api_key <- Sys.getenv("GOOGLE_API_KEY")
  if (api_key == "") stop("GOOGLE_API_KEY is not set.")
  httr2::request(paste0("https://generativelanguage.googleapis.com/v1beta/", .file_id)) |>
    httr2::req_headers_redacted(`x-goog-api-key` = api_key) |>
    httr2::req_method("DELETE") |>
    httr2::req_perform()
  message("File ", .file_id, " has been successfully deleted.")
  invisible(NULL)
}


#' Google Gemini Provider Function
#'
#' The `gemini()` function acts as a provider interface for interacting with the Google Gemini API 
#' through `tidyllm`'s main verbs such as `chat()` and `embed()`. 
#' It dynamically routes requests to Gemini-specific functions 
#' like `gemini_chat()` and `gemini_embedding()` based on the context of the call.
#'
#' Some functions, such as `gemini_upload_file()` and `gemini_delete_file()`, 
#' are specific to Gemini and do not have general verb counterparts.
#'
#' @param ... Parameters to be passed to the appropriate Gemini-specific function, 
#'   such as model configuration, input text, or API-specific options.
#' @param .called_from An internal argument specifying which action (e.g., 
#'   `chat`, `embed`) the function is invoked from. 
#'   This argument is automatically managed by the `tidyllm` verbs and should not be modified by the user.
#'
#' @return The result of the requested action, depending on the specific function invoked 
#'   (e.g., an updated `LLMMessage` object for `chat()`).
#'
#' @export
gemini <- create_provider_function(
  .name = "gemini",
  chat = gemini_chat,
  embed = gemini_embedding,
  send_batch = send_gemini_batch,
  check_batch = check_gemini_batch,
  list_batches = list_gemini_batches,
  fetch_batch = fetch_gemini_batch,
  list_models = gemini_list_models,
  upload_file = gemini_upload_file_verb,
  list_files  = gemini_list_files_verb,
  file_info   = gemini_file_info_verb,
  delete_file = gemini_delete_file_verb
)

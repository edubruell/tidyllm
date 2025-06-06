

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
  
  # Filter to only include user and assistant messages
  gemini_history <- filter_roles(.llm@message_history, c("user", "assistant"))
  
  # Map each message to the expected Gemini format
  lapply(gemini_history, function(m) {
    
    formatted_message <- format_message(m)
    if (!is.null(formatted_message$image)) {
      list(
        role = ifelse(m$role == "user", "user", "model"), 
        parts = list(
          list(text = formatted_message$content),
          list(
            inline_data = list(
              mime_type = formatted_message$image$media_type,
              data      = formatted_message$image$data
            )
          )
        )
      )
    } else {
      list(
        role = ifelse(m$role == "user", "user", "model"), 
        parts = list(text = formatted_message$content)
      )
    }
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


#' A method to handle streaming requests for Gemini
#'
#' @noRd
method(handle_stream,list(api_gemini,new_S3_class("httr2_response"))) <- function(.api,.stream_response) {
  stream_data <- ""
  current_buffer <-  ""
  repeat {
    stream_chunk   <- httr2::resp_stream_lines(.stream_response)
    stream_data    <- paste0(stream_data,stream_chunk)
    current_buffer <- paste0(current_buffer,stream_chunk) 
    
    parts_section <- stringr::str_extract(current_buffer, 
                                          '"parts":\\s*\\[\\s*\\{[^\\}]*\\}\\s*\\]')
    
    if(!is.na(parts_section)){
      current_part <- jsonlite::fromJSON(paste0("{",parts_section,"}"))
      stream_response <- current_part$parts$text
      cat(stream_response)
      utils::flush.console()
      current_buffer <-  ""
    }
    
    # Skip empty chunks
    if (httr2::resp_stream_is_complete(.stream_response)) {
      close(.stream_response)
      message("\n---------\nStream finished\n---------\n")
      break
    }
  }
  stream_data_parsed <- jsonlite::fromJSON(stream_data)
  stream_text <- stream_data_parsed$candidates |> 
    purrr::map_chr(~.x$content$parts[[1]]$text) |>
    stringr::str_c(collapse = "")
  
  list(
    reply = stream_text,
    raw_data = list(parsed=stream_data_parsed)
  )
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
    specific_metadata = list(
      finishReason = .response$candidates[[1]]$finishReason,
      avgLogprobs  = .response$candidates[[1]]$avgLogprobs,
      groundingMetadata = .response$candidates[[1]]$groundingMetadata
      ) 
  )
}  

#' A function to get metadata from Openai streaming responses
#'
#' @noRd
method(extract_metadata_stream, list(api_gemini,class_list))<- function(.api,.stream_raw_data) {
  parsed_stream_data <- .stream_raw_data$parsed
  final_stream_chunk <- parsed_stream_data[nrow(parsed_stream_data),]

  list(
    model             = final_stream_chunk$modelVersion,
    timestamp         = lubridate::as_datetime(lubridate::now()),
    prompt_tokens     = final_stream_chunk$usageMetadata$promptTokenCount,
    completion_tokens = final_stream_chunk$usageMetadata$candidatesTokenCount,
    total_tokens      = final_stream_chunk$usageMetadata$totalTokenCount,
    stream            = TRUE,
    specific_metadata = list(
      warning    = "Gemini outputs different metadata for streaming and non-streaming responses",
      token_details = final_stream_chunk$usageMetadata
    ) 
  )
}  


#' Method to convert a tidyllm TOOL definition to the expected input for Gemini
#'
#' @noRd
method(tools_to_api, list(api_gemini, class_list)) <- function(.api, .tools) {
  list(
    function_declarations = purrr::map(.tools, function(tool) {
      tool_def <- list(
        name = tool@name,
        description = tool@description
      )
      if (length(tool@input_schema) > 0) {
        tool_def$parameters <- list(
          type = "object",
          properties = purrr::map(tool@input_schema, function(param) {
            list(
              type = param@type,
              description = param@description
            )
          }),
          required = names(tool@input_schema)
        )
      }
      tool_def
    })
  )
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
#'
#' @return A new `LLMMessage` object containing the original messages plus the assistant's response.
#'
#' @export
gemini_chat <- function(.llm,
                   .model = "gemini-2.0-flash",
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
                   .timeout = 120,
                   .dry_run = FALSE,
                   .max_tries = 3,
                   .verbose = FALSE,
                   .stream = FALSE) {

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
    "Streaming is not supported for requests with tool calls" = is.null(.tools) || !isTRUE(.stream)
  ) |>
    validate_inputs()
  
  api_obj <- api_gemini(short_name = "gemini",
                        long_name  = "Google Gemini",
                        api_key_env_var = "GOOGLE_API_KEY")
  
  api_key <- get_api_key(api_obj,.dry_run)
  
  # Prepare message contents (inject files if available)
  gemini_contents <- to_api_format(.llm,api_obj) |>
    gemini_inject_files(.fileid)
  
  # Handle JSON schema
  response_format <- NULL
  json=FALSE
  if (requireNamespace("ellmer", quietly = TRUE)) {
    #Handle ellmer json schemata Objects
    if(S7_inherits(.json_schema,ellmer::TypeObject)){
      .json_schema = to_schema(.json_schema)
      # Remove additionalProperties if it exists
      if (!is.null(.json_schema) && "additionalProperties" %in% names(.json_schema)) {
        .json_schema <- .json_schema[setdiff(names(.json_schema), "additionalProperties")]
      }
    } 
  }
  if (!is.null(.json_schema)) {
    json=TRUE
    response_format <- list(
      response_mime_type = "application/json",
      response_schema = .json_schema
    )
  } 
  
  
  #Put a single tool into a list if only one is provided. 
  tools_def <- if (!is.null(.tools)) {
    .tools <- if (S7_inherits(.tools, TOOL))  list(.tools) else .tools
    tools_to_api(api_obj,.tools) 
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
    stopSequences = .stop_sequences
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
    httr2::req_url_query(key = api_key) |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_json(request_body)
  
  if (.dry_run) return(request)
  
  # Perform the API request
  response <- perform_chat_request(request,api_obj,.stream,.timeout,.max_tries)
  
  if(.stream==FALSE) {
    #Handle tool calls
    if (!is.null(response$raw$content$candidates[[1]]$content$parts[[1]]$functionCall)) {
      tool_call <- response$raw$content$candidates[[1]]$content$parts[[1]]$functionCall
      
      tool_messages <- run_tool_calls(api_obj, list(tool_call), .tools)
      
      # Create an assistant message that contains the functionCall.
      assistant_function_call_message <- list(
        role = "model",
        parts = list(
          list(
            functionCall = tool_call
          )
        )
      )
      
      # Append both the assistant's functionCall message and the tool response (user message)
      # to the conversation history.
      request_body$contents <- c(
        request_body$contents,
        list(assistant_function_call_message),
        list(tool_messages)
      )
      
      # Update the request with the new conversation history.
      request <- request |>
        httr2::req_body_json(data = request_body)
      
      # Resend the request to Gemini with the appended tool responses.
      response <- perform_chat_request(request, api_obj, .stream, .timeout, .max_tries)
    }
  }
  
  add_message(.llm     = .llm,
              .role    = "assistant", 
              .content = response$assistant_reply, 
              .json    = json,
              .meta    = response$meta)
}



#' Upload a File to Gemini API
#'
#' Uploads a file to the Gemini API and returns its metadata as a tibble.
#'
#' @param .file_path The local file path of the file to upload.
#' @return A tibble containing metadata about the uploaded file, including its name, URI, and MIME type.
#' @export
gemini_upload_file <- function(.file_path) {
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
    httr2::req_url_query(key = api_key) |>
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
  # Retrieve API key
  api_key <- Sys.getenv("GOOGLE_API_KEY")
  if (api_key == "") {
    stop("API key is not set. Please set it with: Sys.setenv(GOOGLE_API_KEY = 'YOUR-KEY-GOES-HERE')")
  }
  
  # Request to get file metadata
  response <- httr2::request(paste0("https://generativelanguage.googleapis.com/v1beta/", .file_name)) |>
    httr2::req_url_query(key = api_key) |>
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
  # Retrieve API key
  api_key <- Sys.getenv("GOOGLE_API_KEY")
  if (api_key == "") {
    stop("API key is not set. Please set it with: Sys.setenv(GOOGLE_API_KEY = 'YOUR-KEY-GOES-HERE')")
  }
  
  # Request to list files
  response <- httr2::request("https://generativelanguage.googleapis.com/v1beta/files") |>
    httr2::req_url_query(key = api_key, pageSize = .page_size, pageToken = .page_token) |>
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
  # Retrieve API key
  api_key <- Sys.getenv("GOOGLE_API_KEY")
  if (api_key == "") {
    stop("API key is not set. Please set it with: Sys.setenv(GOOGLE_API_KEY = 'YOUR-KEY-GOES-HERE')")
  }
  
  # Request to delete the file
  httr2::request(paste0("https://generativelanguage.googleapis.com/v1beta/", .file_name)) |>
    httr2::req_url_query(key = api_key) |>
    httr2::req_method("DELETE") |>
    httr2::req_perform()
  
  message("File ", .file_name, " has been successfully deleted.")
}


#' Generate Embeddings Using the Google Gemini API
#'
#' @param .input  A character vector of texts to embed or an `LLMMessage` object
#' @param .model The embedding model identifier (default: "text-embedding-3-small").
#' @param .truncate Whether to truncate inputs to fit the model's context length (default: TRUE).
#' @param .timeout Timeout for the API request in seconds (default: 120).
#' @param .dry_run If TRUE, perform a dry run and return the request object.
#' @param .max_tries Maximum retry attempts for requests (default: 3).
#' @return A matrix where each column corresponds to the embedding of a message in the message history.
#' @export
gemini_embedding <- function(.input,
                             .model = "text-embedding-004",
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
    httr2::req_url_query(key = api_key) |>
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
  embed = gemini_embedding
)

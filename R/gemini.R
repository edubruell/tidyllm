#' Send LLMMessage to Gemini API
#'
#' @param .llm An existing LLMMessage object or an initial text prompt.
#' @param .model The model identifier (default: "gemini-1.5-flash").
#' @param .fileid Optional file name for a file uploaded via `gemini_upload_file()`(default: NULL)
#' @param .temperature Controls randomness in generation (default: NULL, range: 0.0-2.0).
#' @param .max_output_tokens Maximum tokens in the response (default: NULL).
#' @param .top_p Controls nucleus sampling (default: NULL, range: 0.0-1.0).
#' @param .top_k Controls diversity in token selection (default: NULL, range: 0 or more).
#' @param .presence_penalty Penalizes new tokens (default: NULL, range: -2.0 to 2.0).
#' @param .frequency_penalty Penalizes frequent tokens (default: NULL, range: -2.0 to 2.0).
#' @param .stop_sequences Optional character sequences to stop generation (default: NULL, up to 5).
#' @param .safety_settings A list of safety settings (default: NULL).
#' @param .tools Optional tools for function calling or code execution (default: NULL).
#' @param .tool_config Optional configuration for the tools specified (default: NULL).
#' @param .json_schema A JSON schema object as R list to enforce the output structure (If defined has precedence over JSON mode).
#' @param .timeout When should our connection time out (default: 120 seconds).
#' @param .dry_run If TRUE, perform a dry run and return the request object.
#' @param .max_tries Maximum retries to perform request (default: 3).
#' @param .verbose Should additional information be shown after the API call.
#' @param .stream Should the response be streamed (default: FALSE).
#' @return Returns an updated LLMMessage object.
#' @export
gemini <- function(.llm,
                   .model = "gemini-1.5-flash",
                   .fileid = NULL,
                   .temperature = NULL,
                   .max_output_tokens = NULL,
                   .top_p = NULL,
                   .top_k = NULL,
                   .presence_penalty = NULL,
                   .frequency_penalty = NULL,
                   .stop_sequences = NULL,
                   .safety_settings = NULL,
                   .tools = NULL,
                   .tool_config = NULL,
                   .json_schema = NULL,
                   .timeout = 120,
                   .dry_run = FALSE,
                   .max_tries = 3,
                   .verbose = FALSE,
                   .stream = FALSE) {
  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = inherits(.llm, "LLMMessage"),
    "Input .model must be a string" = is.character(.model) && length(.model) == 1,
    "Input .temperature must be NULL or in [0.0, 2.0]" = is.null(.temperature) | (.temperature >= 0.0 & .temperature <= 2.0),
    "Input .max_output_tokens must be NULL or an integer-valued numeric greater than 1" = is.null(.max_output_tokens) | (.max_output_tokens >= 1 & is_integer_valued(.max_output_tokens)),
    "Input .top_p must be NULL or in [0.0, 1.0]" = is.null(.top_p) | (.top_p >= 0.0 & .top_p <= 1.0),
    "Input .top_k must be NULL or non-negative" = is.null(.top_k) | (.top_k >= 0),
    "Input .presence_penalty must be NULL or in [-2.0, 2.0]" = is.null(.presence_penalty) | (.presence_penalty >= -2.0 & .presence_penalty <= 2.0),
    "Input .frequency_penalty must be NULL or in [-2.0, 2.0]" = is.null(.frequency_penalty) | (.frequency_penalty >= -2.0 & .frequency_penalty <= 2.0),
    "Input .stop_sequences must be NULL or a list of up to 5 strings" = is.null(.stop_sequences) | (is.list(.stop_sequences) & length(.stop_sequences) <= 5),
    "Input .safety_settings must be NULL or a list" = is.null(.safety_settings) | is.list(.safety_settings),
    "Input .tools must be NULL or a list" = is.null(.tools) | is.list(.tools),
    "Input .tool_config must be NULL or a list" = is.null(.tool_config) | is.list(.tool_config),
    "Input .json_schema must be NULL or a list" = is.null(.json_schema) | is.list(.json_schema),
    "Input .timeout must be an integer-valued numeric and positive" = is_integer_valued(.timeout) & .timeout > 0,
    "Input .max_tries must be integer-valued numeric and positive" = is_integer_valued(.max_tries) & .max_tries > 0,
    "Input .dry_run must be logical" = is.logical(.dry_run),
    "Input .verbose must be logical" = is.logical(.verbose),
    "Input .stream must be logical" = is.logical(.verbose)
  ) |>
    validate_inputs()
  
  # Prepare message contents
  gemini_contents <- .llm$to_api_format("gemini")
  
  # Inject file if provided
  if (!is.null(.fileid)) {
    file_info <- gemini_file_metadata(.fileid)  
    file_data <- list(
      file_data = list(
        mime_type = file_info$mime_type,
        file_uri = file_info$uri
      )
    )
    
    # Injecting file data as a new "part" in the last user message
    gemini_contents[[length(gemini_contents)]]$parts <- c(
      list(gemini_contents[[length(gemini_contents)]]$parts),
      list(file_data)
    )
  }
  
  # Handle JSON schema
  response_format <- NULL
  json=FALSE
  if (!is.null(.json_schema)) {
    #Deal with the different schema format for gemini
    gemini_schema <- .json_schema$schema
    #gemini_schema$name <- NULL
      
    json=TRUE
    response_format <- list(
      response_mime_type = "application/json",
      response_schema = gemini_schema
    )
  } 
  
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
    append(response_format)
  generation_config <- base::Filter(Negate(is.null), generation_config)
  
  # Construct the request body
  request_body <- list(
    model = .model,
    contents = list(gemini_contents),
    generationConfig = generation_config,
    safetySettings = .safety_settings,
    tools = .tools,
    toolConfig = .tool_config
  ) 
  request_body <- base::Filter(Negate(is.null), request_body)
  
  # Retrieve API key
  api_key <- Sys.getenv("GOOGLE_API_KEY")
  if ((api_key == "") & .dry_run == FALSE) {
    stop("API key is not set. Please set it with: Sys.setenv(GOOGLE_API_KEY = 'YOUR-KEY-GOES-HERE')")
  }
  
  if(.stream==FALSE) request_type <- ":generateContent"
  if(.stream==TRUE)  request_type <- ":streamGenerateContent"
  
  # Build the request
  request <- httr2::request("https://generativelanguage.googleapis.com") |>
    httr2::req_url_path(paste0("/v1beta/models/", .model, request_type)) |>
    httr2::req_url_query(key = api_key) |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_json(request_body)
  
  # Perform the API request
  response <- perform_api_request(
    .request = request,
    .api = "gemini",
    .stream = .stream,
    .timeout = .timeout,
    .max_tries = .max_tries,
    .parse_response_fn = function(body_json) {
      if ("error" %in% names(body_json)) {
        stop(sprintf("Gemini API returned an Error:\nCode: %s\nMessage: %s",
                     body_json$error$code, body_json$error$message))
      }
      if (!"candidates" %in% names(body_json) || length(body_json$candidates) == 0) {
        stop("Received empty response from Gemini API")
      }
      body_json$candidates[[1]]$content$parts[[1]]$text
    },
    .dry_run = .dry_run
  )
  
  if (.dry_run) return(response)
  assistant_reply <- response$assistant_reply
  llm_copy <- .llm$clone_deep()
  llm_copy$add_message(role = "assistant", 
                       content = assistant_reply , 
                       json    = json,
                       meta    = response$meta)
  
  return(llm_copy)
}


#' Upload a File to Gemini API
#'
#' Uploads a file to the Gemini API and returns its metadata as a tibble.
#'
#' @param .file_path The local file path of the file to upload.
#' @return A tibble containing metadata about the uploaded file, including its name, URI, and MIME type.
#' @export
gemini_upload_file <- function(.file_path) {
  mime_type <- system(paste("file -b --mime-type", shQuote(.file_path)), intern = TRUE)
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
#' @param .llm An existing LLMMessage object (or a character vector of texts to embed)
#' @param .model The embedding model identifier (default: "text-embedding-3-small").
#' @param .truncate Whether to truncate inputs to fit the model's context length (default: TRUE).
#' @param .timeout Timeout for the API request in seconds (default: 120).
#' @param .dry_run If TRUE, perform a dry run and return the request object.
#' @param .max_tries Maximum retry attempts for requests (default: 3).
#' @return A matrix where each column corresponds to the embedding of a message in the message history.
#' @export
gemini_embedding <- function(.llm,
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
    "Input .llm must be an LLMMessage object or a character vector" = inherits(.llm, "LLMMessage") | is.character(.llm),
    "Input .model must be a string" = is.character(.model),
    "Input .truncate must be logical" = is.logical(.truncate),
    "Input .timeout must be a positive numeric value" = is.numeric(.timeout) && .timeout > 0,
    ".dry_run must be logical" = is.logical(.dry_run)
  ) |> validate_inputs()
  
  # Prepare message texts
  if (!is.character(.llm)) {
    openai_history <- Filter(function(x) {
      "role" %in% names(x) && x$role %in% c("user", "assistant")
    }, .llm$message_history)
    
    message_texts <- lapply(openai_history, function(m) {
      base_content <- m$content
      media_list <- m$media
      text_media <- extract_media(media_list, "text")
      paste(base_content, paste(unlist(text_media), collapse = " "), sep = " ")
    })
  } else {
    message_texts <- .llm
  }
  
  # Prepare batch request
  request_body <- list(
    requests = lapply(message_texts, function(text) {
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
  
  # Execute request
  response <- request |>
    httr2::req_timeout(.timeout) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_retry(max_tries = .max_tries,
                     retry_on_failure = TRUE,
                     is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 503)) |>
    httr2::req_perform()
  
  # Handle the response
  if (httr2::resp_status(response) != 200) {
    stop("API request failed: ", httr2::resp_status(response), " - ", httr2::resp_body_json(response)$error$message)
  }
  
  response_json <- httr2::resp_body_json(response) 
  # Extract the embeddings
  embeddings <- response_json$embeddings |> 
    purrr::map(unlist)
  
  # Check if embeddings are present
  if (is.null(embeddings)) {
    stop("No embeddings returned in the response.")
  }
  
  # Convert embeddings to a matrix
  embedding_matrix <- do.call(cbind, embeddings)
  
  embedding_matrix
}



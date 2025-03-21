
#' The Grow API provider class (inherits from OpenAI)
#'
#' @noRd
api_groq <- new_class("Groq", api_openai)

#' A function to get metadata from Openai responses
#'
#' @noRd
method(extract_metadata, list(api_groq,class_list))<- function(.api, .response) {
  list(
    model             = .response$model,
    timestamp         = lubridate::as_datetime(.response$created),
    prompt_tokens     = .response$usage$prompt_tokens,
    completion_tokens = .response$usage$completion_tokens,
    total_tokens      = .response$usage$total_tokens,
    specific_metadata = list(
      system_fingerprint        = .response$system_fingerprint,
      completion_time           = .response$usage$completion_time,
      total_time                = .response$usage$total_time,
      groq_id                   = .response$x_groq$id
    ) 
  )
}  


#' Send LLM Messages to the Groq Chat API
#'
#' @description
#' This function sends a message history to the Groq Chat API and returns the assistant's reply.
#'
#' @param .llm An `LLMMessage` object containing the conversation history.
#' @param .model The identifier of the model to use (default: "llama-3.2-11b-vision-preview").
#' @param .max_tokens The maximum number of tokens that can be generated in the response (default: 1024).
#' @param .temperature Controls the randomness in the model's response. Values between 0 and 2 are allowed, where higher values increase randomness (optional).
#' @param .top_p Nucleus sampling parameter that controls the proportion of probability mass considered. Values between 0 and 1 are allowed (optional).
#' @param .frequency_penalty Number between -2.0 and 2.0. Positive values penalize repeated tokens, reducing likelihood of repetition (optional).
#' @param .presence_penalty Number between -2.0 and 2.0. Positive values encourage new topics by penalizing tokens that have appeared so far (optional).
#' @param .stop One or more sequences where the API will stop generating further tokens. Can be a string or a list of strings (optional).
#' @param .tools Either a single TOOL object or a list of TOOL objects representing the available functions for tool calls (optional).
#' @param .tool_choice A character string specifying the tool-calling behavior; valid values are "none", "auto", or "required" (optional).
#' @param .seed An integer for deterministic sampling. If specified, attempts to return the same result for repeated requests with identical parameters (optional).
#' @param .api_url Base URL for the Groq API (default: "https://api.groq.com/").
#' @param .json Whether the response should be structured as JSON (default: FALSE).
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .stream Logical; if TRUE, streams the response piece by piece (default: FALSE).
#' @param .verbose If TRUE, displays additional information after the API call, including rate limit details (default: FALSE).
#' @param .max_tries Maximum retries to peform request
#' @param .dry_run If TRUE, performs a dry run and returns the constructed request object without executing it (default: FALSE).
#'
#' @return A new `LLMMessage` object containing the original messages plus the assistant's response.
#' 
#' @examples
#' \dontrun{
#' # Basic usage
#' msg <- llm_message("What is Groq?")
#' result <- groq_chat(msg)
#' 
#' # With custom parameters
#' result2 <- groq_chat(msg, 
#'                .model = "llama-3.2-vision",
#'                .temperature = 0.5, 
#'                .max_tokens = 512)
#' }
#'
#' @export
groq_chat <- function(.llm,
                 .model = "deepseek-r1-distill-llama-70b",
                 .max_tokens = 1024,
                 .temperature = NULL,
                 .top_p = NULL,
                 .frequency_penalty = NULL,
                 .presence_penalty = NULL,
                 .stop = NULL,
                 .seed = NULL,
                 .tools = NULL,
                 .tool_choice = NULL,
                 .api_url = "https://api.groq.com/",
                 .json = FALSE,
                 .timeout = 60,
                 .verbose = FALSE,
                 .stream = FALSE,
                 .dry_run = FALSE,
                 .max_tries = 3) {

  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
    "Input .max_tokens must be an integer" = is_integer_valued(.max_tokens) & .max_tokens > 0,
    "Input .model must be a non-empty string" = is.character(.model) & nzchar(.model),
    "Input .api_url must be a valid URL" = is.character(.api_url) & nzchar(.api_url),
    "Input .timeout must be an integer-valued numeric greater than 0" = is_integer_valued(.timeout) & .timeout > 0,
    "Input .temperature must be numeric between 0 and 2 if provided" = is.null(.temperature) | (.temperature >= 0 & .temperature <= 2),
    "Input .top_p must be numeric between 0 and 1 if provided" = is.null(.top_p) | (.top_p >= 0 & .top_p <= 1),
    "Input .frequency_penalty must be numeric between -2 and 2 if provided" = is.null(.frequency_penalty) | (.frequency_penalty >= -2 & .frequency_penalty <= 2),
    "Input .presence_penalty must be numeric between -2 and 2 if provided" = is.null(.presence_penalty) | (.presence_penalty >= -2 & .presence_penalty <= 2),
    "Input .stop must be a charachter vector or a list of character vectors, or NULL" = is.null(.stop) | is.character(.stop) | is.list(.stop),
    "Input .seed must be an integer if provided" = is.null(.seed) | is_integer_valued(.seed),
    "Input .tools must be NULL, a TOOL object, or a list of TOOL objects" = is.null(.tools) || S7_inherits(.tools, TOOL) || (is.list(.tools) && all(purrr::map_lgl(.tools, ~ S7_inherits(.x, TOOL)))),
    "Input .tool_choice must be NULL or a character (one of 'none', 'auto', 'required')" = is.null(.tool_choice) || (is.character(.tool_choice) && .tool_choice %in% c("none", "auto", "required")),
    "Input .json must be logical" = is.logical(.json),
    "Input .verbose must be logical" = is.logical(.verbose),
    "Input .max_tries must be integer-valued numeric" = is_integer_valued(.max_tries),
    "Input .dry_run must be logical" = is.logical(.dry_run),
    "Streaming is not supported for requests with tool calls" = is.null(.tools) || !isTRUE(.stream)
  ) |>
    validate_inputs()
  
  api_obj <- api_groq(short_name = "groq",
                    long_name  = "Groq",
                    api_key_env_var = "GROQ_API_KEY")
  
  # Get formatted message list for Groq models
  messages <- to_api_format(.llm,api_obj,TRUE)
  
  api_key <- get_api_key(api_obj,.dry_run)
  
  #Put a single tool into a list if only one is provided. 
  tools_def <- if (!is.null(.tools)) {
    if (S7_inherits(.tools, TOOL))  list(.tools) else .tools
  } else {
    NULL
  }
  
  # Fill the request body
  request_body <- list(
    model = .model,
    max_tokens = .max_tokens,
    messages = messages,
    temperature = .temperature,
    top_p = .top_p,
    frequency_penalty = .frequency_penalty,
    presence_penalty = .presence_penalty,
    stop = .stop,
    seed = .seed,
    stream = .stream,
    tools = if(!is.null(tools_def)) tools_to_api(api_obj,tools_def) else NULL,
    tool_choice = .tool_choice
  ) |> purrr::compact()
  
  # Handle JSON mode
  if (.json == TRUE) {
    request_body$response_format <- list(type = "json_object")
  }
  
  request <- httr2::request(.api_url) |>
    httr2::req_url_path("/openai/v1/chat/completions") |>
    httr2::req_headers(
      `Authorization` = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(data = request_body)
  
  # Return only the request object in a dry run.
  if (.dry_run) {
    return(request)  
  }
  
  response <- perform_chat_request(request,api_obj,.stream,.timeout,.max_tries)
  if(!is.null(response$raw$content$choices[[1]]$message$tool_calls)){
    tool_messages <- run_tool_calls(api_obj,
                                    response$raw$content$choices[[1]]$message$tool_calls,
                                    tools_def)
    ##Append the tool call to API
    request_body$messages <- request_body$messages |> 
      append(tool_messages)
    
    request <- request |>
      httr2::req_body_json(data = request_body)
    
    response <- perform_chat_request(request,api_obj,.stream,.timeout,.max_tries)
  }
  
  # Extract assistant reply and rate limiting info from response headers
  assistant_reply <- response$assistant_reply
  track_rate_limit(api_obj,response$headers,.verbose)
  
  # Add model's message to the history of the LLMMessage object
  add_message(.llm     = .llm,
              .role    = "assistant", 
              .content = assistant_reply , 
              .json    = .json,
              .meta    = response$meta)
}



#' Transcribe an Audio File Using Groq transcription API 
#'
#' @description
#' This function reads an audio file and sends it to the Groq transcription API for transcription.
#'
#' @param .audio_file The path to the audio file (required). Supported formats include flac, mp3, mp4, mpeg, mpga, m4a, ogg, wav, or webm.
#' @param .model The model to use for transcription (default: "whisper-large-v3").
#' @param .language The language of the input audio, in ISO-639-1 format (optional).
#' @param .prompt A prompt to guide the transcription style. It should match the audio language (optional).
#' @param .temperature Sampling temperature, between 0 and 1, with higher values producing more randomness (default: 0).
#' @param .api_url Base URL for the API (default: "https://api.groq.com/openai/v1/audio/transcriptions").
#' @param .verbose Logical; if TRUE, outputs additional information (default: FALSE).
#' @param .dry_run Logical; if TRUE, performs a dry run and returns the request object without making the API call (default: FALSE).
#' @param .verbose Logical; if TRUE, rate limiting info is displayed after the API request (default: FALSE).
#' @param .max_tries Maximum retries to peform request
#' @examples
#' \dontrun{
#' # Basic usage
#' groq_transcribe(.audio_file = "example.mp3")
#' }
#'
#' @return A character vector containing the transcription.
#' @export
groq_transcribe <- function(
    .audio_file,
    .model = "whisper-large-v3",
    .language = NULL,
    .prompt = NULL,
    .temperature = 0,
    .api_url = "https://api.groq.com/openai/v1/audio/transcriptions",
    .dry_run = FALSE,
    .verbose = FALSE,
    .max_tries = 3) {

  # Validate audio file path
  if (!file.exists(.audio_file)) stop("Audio file does not exist.")
  api_key <- Sys.getenv("GROQ_API_KEY")
  if ((api_key == "")& .dry_run==FALSE){
    stop("API key is not set. Please set it with: Sys.setenv(GROQ_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  }
  
  # Validate inputs
  c(
    "Input .model must be a character vector" = is.character(.model),
    "Input .language must be a character vector if specified" = is.null(.language) || is.character(.language),
    "Input .prompt must be a character vector if specified" = is.null(.prompt) || is.character(.prompt),
    "Input .temperature" = is.numeric(.temperature) && .temperature >= 0 && .temperature <= 1,
    "Input .max_tries must be integer-valued numeric" = is_integer_valued(.max_tries)
  ) |>
    validate_inputs()
  
  # Build the request
  request <- httr2::request(.api_url) |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", Sys.getenv("GROQ_API_KEY"))
    ) |>
    httr2::req_body_multipart(
      file = curl::form_file(.audio_file),
      model = .model,
      prompt = .prompt,
      response_format ="json",
      language = .language,
      temperature = as.character(.temperature)
    )
  
  # Handle dry run
  if (.dry_run) return(request)
  

  #Perform the request
  response <- request |>
               httr2::req_error(is_error = function(resp) FALSE) |>
               httr2::req_retry(max_tries = .max_tries,  
                                retry_on_failure = TRUE,
                                 is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 503)) |>
               httr2::req_perform()
  
  content <- httr2::resp_body_json(response)
  if("error" %in% names(content)){
    sprintf("Groq API returned an Error:\nCode: %s\nMessage: %s",
            content$error$code,
            content$error$message) |>
      stop()
  }
  
  groq_audio_headers <- httr2::resp_headers(response)
  
  #Parse the headers from the response
  request_time <- strptime(groq_audio_headers[["date"]], 
                           format="%a, %d %b %Y %H:%M:%S", tz="GMT")
  rl_total_sec     <- as.integer(groq_audio_headers[["x-ratelimit-limit-audio-seconds"]])
  rl_total_req     <- as.integer(groq_audio_headers[["x-ratelimit-limit-requests"]])
  rl_remaining_sec <- as.integer(groq_audio_headers[["x-ratelimit-remaining-audio-seconds"]])
  rl_remaining_req <- as.integer(groq_audio_headers[["x-ratelimit-remaining-requests"]])
  rl_durartion_sec <- parse_duration_to_seconds(groq_audio_headers[["x-ratelimit-reset-audio-seconds"]])
  rl_durartion_req <- parse_duration_to_seconds(groq_audio_headers[["x-ratelimit-reset-requests"]])
  
  #Compute reset times
  rl_reset_req <- request_time + rl_durartion_req
  rl_reset_sec <- request_time + rl_durartion_sec
  
  # Show rate limit info if verbose
  if (.verbose == TRUE) {
    glue::glue(
     "Groq Transcribe API answer received at {request_time}.
      Remaining requests rate limit: {rl_remaining_req}/{rl_total_req}
      Requests rate limit reset at: {rl_reset_req}
      Remaining Audio seconds rate limit: {rl_remaining_sec}/{rl_total_sec}
      Audio seconds rate limit reset at: {rl_reset_sec}\n"
    ) |> cat("\n")
  }
  
  
  return(content$text)
}

#' List Available Models from the Groq API
#'
#' @param .api_url Base URL for the API (default: "https://api.groq.com").
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .max_tries Maximum number of retries for the API request (default: 3).
#' @param .dry_run Logical; if TRUE, returns the prepared request object without executing it.
#' @param .verbose Logical; if TRUE, prints additional information about the request.
#'
#' @return A tibble containing model information (columns include `id`, `created`, `owned_by`, and `context_window`),
#'   or NULL if no models are found.
#'
#' @export
groq_list_models <- function(.api_url = "https://api.groq.com",
                             .timeout = 60,
                             .max_tries = 3,
                             .dry_run = FALSE,
                             .verbose = FALSE) {
  # Create an API object for Groq using the tidyllm helper
  api_obj <- api_openai(short_name = "groq",
                        long_name  = "Groq",
                        api_key_env_var = "GROQ_API_KEY")
  
  # Retrieve the API key (will error if not set, unless in dry run mode)
  api_key <- get_api_key(api_obj, .dry_run)
  
  # Build the request to the /openai/v1/models endpoint
  request <- httr2::request(.api_url) |>
    httr2::req_url_path("/openai/v1/models") |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    )
  
  # If dry run is requested, return the constructed request object
  if (.dry_run) {
    return(request)
  }
  
  # Perform the request with specified timeout and retry logic
  response <- request |>
    httr2::req_timeout(.timeout) |>
    httr2::req_retry(max_tries = .max_tries) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  
  if (.verbose) {
    message("Retrieved response from Groq: ", response$object)
  }
  
  # Check if the "data" field exists and contains models
  if (!is.null(response$data)) {
    models <- response$data
    
    # Create a tibble with selected model information
    model_info <- tibble::tibble(
      id = vapply(models, function(model) model$id, character(1)),
      created = vapply(models, function(model) {
        as.character(strptime(
          format(as.POSIXct(model$created, origin = "1970-01-01", tz = "GMT"),
                 format = "%a, %d %b %Y %H:%M:%S"),
          format = "%a, %d %b %Y %H:%M:%S", tz = "GMT"))
      }, character(1)),
      owned_by = vapply(models, function(model) model$owned_by, character(1)),
      context_window = vapply(models, function(model) model$context_window, numeric(1))
    )
    
    return(model_info)
  } else {
    return(NULL)
  }
}

#' Send a Batch of Messages to the Groq API
#'
#' This function creates and submits a batch of messages to the Groq API for asynchronous processing.
#'
#' @param .llms A list of LLMMessage objects containing conversation histories.
#' @param .model Character string specifying the model to use (default: "deepseek-r1-distill-llama-70b").
#' @param .max_tokens Integer specifying the maximum tokens per response (default: 1024).
#' @param .temperature Numeric between 0 and 2 controlling response randomness.
#' @param .top_p Numeric between 0 and 1 for nucleus sampling.
#' @param .frequency_penalty Number between -2.0 and 2.0 to penalize repetition.
#' @param .presence_penalty Number between -2.0 and 2.0 to encourage new topics.
#' @param .stop One or more sequences where the API will stop generating further tokens.
#' @param .seed An integer for deterministic sampling.
#' @param .api_url Base URL for the Groq API (default: "https://api.groq.com/").
#' @param .json Whether the response should be structured as JSON (default: FALSE).
#' @param .completion_window Character string for the batch completion window (default: "24h").
#' @param .verbose Logical; if TRUE, prints a message with the batch ID (default: FALSE).
#' @param .overwrite Logical; if TRUE, allows overwriting an existing batch ID (default: FALSE).
#' @param .max_tries Maximum number of retries to perform the request.
#' @param .timeout Integer specifying the request timeout in seconds (default: 60).
#' @param .dry_run Logical; if TRUE, returns the prepared request objects without executing (default: FALSE).
#' @param .id_prefix Character string to specify a prefix for generating custom IDs when names in `.llms` are missing.
#' 
#' @return An updated and named list of `.llms` with identifiers that align with batch responses, including a `batch_id` attribute.
#' @export
send_groq_batch <- function(.llms, 
                            .model = "deepseek-r1-distill-llama-70b", 
                            .max_tokens = 1024, 
                            .temperature = NULL, 
                            .top_p = NULL, 
                            .frequency_penalty = NULL,
                            .presence_penalty = NULL,
                            .stop = NULL, 
                            .seed = NULL,
                            .api_url = "https://api.groq.com/",
                            .json = FALSE,
                            .completion_window = "24h",
                            .verbose = FALSE,
                            .dry_run = FALSE,
                            .overwrite = FALSE,
                            .max_tries = 3,
                            .timeout = 60,
                            .id_prefix = "tidyllm_groq_req_") {

  # Input validation
  c(
    ".llms must be a list of LLMMessage objects" = is.list(.llms) && all(sapply(.llms, S7_inherits, LLMMessage)),
    ".max_tokens must be an integer" = is_integer_valued(.max_tokens),
    ".model must be a non-empty string" = is.character(.model) & nzchar(.model),
    ".temperature must be numeric between 0 and 2 if provided" = is.null(.temperature) || (.temperature >= 0 && .temperature <= 2),
    ".top_p must be numeric between 0 and 1 if provided" = is.null(.top_p) || (.top_p >= 0 && .top_p <= 1),
    ".frequency_penalty must be numeric between -2 and 2 if provided" = is.null(.frequency_penalty) || (.frequency_penalty >= -2 && .frequency_penalty <= 2),
    ".presence_penalty must be numeric between -2 and 2 if provided" = is.null(.presence_penalty) || (.presence_penalty >= -2 && .presence_penalty <= 2),
    ".stop must be a character vector or NULL" = is.null(.stop) || is.character(.stop) || is.list(.stop),
    ".seed must be an integer if provided" = is.null(.seed) || is_integer_valued(.seed),
    ".completion_window must be a valid window format" = is.character(.completion_window) && grepl("^\\d+[hd]$", .completion_window),
    ".verbose must be logical" = is.logical(.verbose),
    ".dry_run must be logical" = is.logical(.dry_run),
    ".overwrite must be logical" = is.logical(.overwrite),
    ".id_prefix must be a character vector of length 1" = is.character(.id_prefix),
    ".max_tries must be integer-valued numeric" = is_integer_valued(.max_tries),
    ".timeout must be an integer" = is_integer_valued(.timeout)
  ) |> validate_inputs()
  
  api_obj <- api_groq(short_name = "groq",
                      long_name  = "Groq",
                      api_key_env_var = "GROQ_API_KEY")
  
  api_key <- get_api_key(api_obj, .dry_run)
  
  prepared_llms <- prepare_llms_for_batch(api_obj,
                                          .llms = .llms,
                                          .id_prefix = .id_prefix,
                                          .overwrite = .overwrite)
  
  # Create JSONL content for batch submission
  jsonl_lines <- lapply(seq_along(prepared_llms), function(i) {
    # Get messages from each LLMMessage object
    messages <- to_api_format(prepared_llms[[i]], api_obj, TRUE)
    
    custom_id <- names(prepared_llms)[i]
    request_body <- list(
      model = .model,
      max_tokens = .max_tokens,
      messages = messages,
      temperature = .temperature,
      top_p = .top_p,
      frequency_penalty = .frequency_penalty,
      presence_penalty = .presence_penalty,
      stop = .stop,
      seed = .seed
    ) |> purrr::compact()
    
    # Handle JSON mode
    if (.json == TRUE) {
      request_body$response_format <- list(type = "json_object")
    }
    
    # Create the batch request entry
    batch_entry <- list(
      custom_id = custom_id,
      method = "POST",
      url = "/v1/chat/completions",
      body = request_body
    )
    
    # Convert to JSON string
    jsonlite::toJSON(batch_entry, auto_unbox = TRUE)
  })
  
  # Combine all lines into a single JSONL string
  jsonl_content <- paste(jsonl_lines, collapse = "\n")
  
  # Create a temporary file to hold the JSONL content
  temp_file <- tempfile(fileext = ".jsonl")
  writeLines(jsonl_content, temp_file)
  
  if (.dry_run) {
    return(readLines(temp_file))
  }
  
  # 1. Upload the JSONL file
  upload_request <- httr2::request(paste0(.api_url, "openai/v1/files")) |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key),
      .redact = "Authorization"
    ) |>
    httr2::req_body_multipart(
      file = curl::form_file(temp_file),
      purpose = "batch"
    )
  
  upload_response <- upload_request |>
    perform_generic_request(.timeout = .timeout, .max_tries = .max_tries)
  
  if ("error" %in% names(upload_response$content)) {
    stop(sprintf("Groq API returned an Error while uploading file:\nMessage: %s",
                 upload_response$content$error$message))
  }
  
  file_id <- upload_response$content$id
  
  # 2. Create the batch job
  batch_request <- httr2::request(paste0(.api_url, "openai/v1/batches")) |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key),
      "Content-Type" = "application/json",
      .redact = "Authorization"
    ) |>
    httr2::req_body_json(list(
      input_file_id = file_id,
      endpoint = "/v1/chat/completions",
      completion_window = .completion_window
    ))
  
  batch_response <- batch_request |>
    perform_generic_request(.timeout = .timeout, .max_tries = .max_tries)
  
  if ("error" %in% names(batch_response$content)) {
    stop(sprintf("Groq API returned an Error while creating batch:\nMessage: %s",
                 batch_response$content$error$message))
  }
  
  # Attach batch_id as an attribute to .llms
  batch_id <- batch_response$content$id
  attr(prepared_llms, "batch_id") <- batch_id
  attr(prepared_llms, "file_id") <- file_id
  
  if (.verbose) {
    message("Batch submitted successfully. Batch ID: ", batch_id)
  }
  
  return(prepared_llms)
}

#' Check Batch Processing Status for Groq API
#'
#' This function retrieves the processing status and other details of a specified Groq batch.
#'
#' @param .llms A list of LLMMessage objects with a batch_id attribute.
#' @param .batch_id A character string with the batch ID to check.
#' @param .api_url Character; base URL of the Groq API (default: "https://api.groq.com/").
#' @param .max_tries Maximum retries to perform request.
#' @param .timeout Integer specifying the request timeout in seconds (default: 60).
#' @param .dry_run Logical; if TRUE, returns the prepared request object without executing (default: FALSE).
#' 
#' @return A tibble with information about the status of batch processing.
#' @export
check_groq_batch <- function(.llms = NULL, 
                             .batch_id = NULL, 
                             .api_url = "https://api.groq.com/",
                             .dry_run = FALSE,
                             .max_tries = 3,
                             .timeout = 60) {
  # Extract batch_id
  if (is.null(.batch_id)) {
    if (!is.null(.llms)) {
      .batch_id <- attr(.llms, "batch_id")
      if (is.null(.batch_id)) {
        stop("No batch_id attribute found in the provided list.")
      }
    } else {
      stop("Either .llms or .batch_id must be provided.")
    }
  }
  
  # Retrieve API key
  api_key <- Sys.getenv("GROQ_API_KEY")
  if ((api_key == "") & !.dry_run) {
    stop("API key is not set.")
  }
  
  # Build request
  request_url <- paste0(.api_url, "openai/v1/batches/", .batch_id)
  request <- httr2::request(request_url) |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key),
      .redact = "Authorization"
    )
  
  if (.dry_run) {
    return(request)
  }
  
  # Perform request with retries and error handling
  response <- request |>
    perform_generic_request(.timeout = .timeout, .max_tries = .max_tries)
  
  # Parse response
  response_body <- response$content
  if ("error" %in% names(response_body)) {
    stop(sprintf("Groq API returned an Error:\nMessage: %s",
                 response_body$error$message))
  }
  
  # Create tibble with batch details
  tibble::tibble(
    batch_id = response_body$id,
    status = response_body$status,
    created_at = lubridate::as_datetime(response_body$created_at, origin = "1970-01-01"),
    expires_at = lubridate::as_datetime(response_body$expires_at, origin = "1970-01-01"),
    req_total = response_body$request_counts$total,
    req_completed = response_body$request_counts$completed,
    req_failed = response_body$request_counts$failed,
    input_file_id = response_body$input_file_id,
    output_file_id = response_body$output_file_id,
    error_file_id = response_body$error_file_id
  )
}

#' Fetch Results for a Groq Batch
#'
#' This function retrieves the results of a completed Groq batch and updates
#' the provided list of `LLMMessage` objects with the responses.
#'
#' @param .llms A list of `LLMMessage` objects that were part of the batch.
#' @param .batch_id Character; the unique identifier for the batch.
#' @param .api_url Character; the base URL for the Groq API (default: "https://api.groq.com/").
#' @param .dry_run Logical; if `TRUE`, returns the constructed request without executing it (default: `FALSE`).
#' @param .max_tries Integer; maximum number of retries if the request fails (default: `3`).
#' @param .timeout Integer; request timeout in seconds (default: `60`).
#'
#' @return A list of updated `LLMMessage` objects, each with the assistant's response added if successful.
#' @export
fetch_groq_batch <- function(.llms, 
                             .batch_id = NULL, 
                             .api_url = "https://api.groq.com/",
                             .dry_run = FALSE,
                             .max_tries = 3,
                             .timeout = 60) {
  c(
    ".llms must be a list of LLMMessage objects with names as custom IDs" = is.list(.llms) && all(sapply(.llms, S7_inherits, LLMMessage)),
    ".batch_id must be a non-empty character string or NULL" = is.null(.batch_id) || (is.character(.batch_id) && nzchar(.batch_id)),
    ".api_url must be a non-empty character string" = is.character(.api_url) && nzchar(.api_url),
    ".dry_run must be logical" = is.logical(.dry_run),
    ".max_tries must be an integer" = is_integer_valued(.max_tries),
    ".timeout must be an integer" = is_integer_valued(.timeout)
  ) |> validate_inputs()
  
  # Preserve original names
  original_names <- names(.llms)
  
  # Retrieve batch_id from .llms if not provided
  if (is.null(.batch_id)) {
    .batch_id <- attr(.llms, "batch_id")
    if (is.null(.batch_id)) {
      stop("No batch_id provided and no batch_id attribute found in the provided list.")
    }
  }
  
  api_obj <- api_groq(short_name = "groq",
                      long_name  = "Groq",
                      api_key_env_var = "GROQ_API_KEY")
  
  api_key <- get_api_key(api_obj, .dry_run)
  
  batch_status <- check_groq_batch(.batch_id = .batch_id)
  
  # Check if batch has completed processing
  if (batch_status$status != "completed") {
    stop("Batch processing has not completed yet. Current status: ", batch_status$status)
  }
  
  if (is.null(batch_status$output_file_id)) {
    stop("Batch completed but no output file is available.")
  }
  
  # Download the results file
  output_file_id <- batch_status$output_file_id
  file_download_url <- paste0(.api_url, "openai/v1/files/", output_file_id, "/content")
  file_download_request <- httr2::request(file_download_url) |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key),
      .redact = "Authorization"
    )
  
  file_download_response <- file_download_request |>
    httr2::req_timeout(.timeout) |>
    httr2::req_retry(max_tries = .max_tries) |>
    httr2::req_perform()
  
  # Parse the JSONL response
  results_content <- httr2::resp_body_string(file_download_response)
  results_lines <- strsplit(results_content, "\n")[[1]]
  results_list <- lapply(results_lines, function(line) {
    if (nzchar(line)) jsonlite::fromJSON(line) else NULL
  })
  results_list <- Filter(Negate(is.null), results_list)
  
  # Map results by custom_id
  results_by_custom_id <- purrr::set_names(
    results_list, 
    sapply(results_list, function(x) x$custom_id)
  )
  
  # Map results back to the original .llms list using names as custom IDs
  updated_llms <- lapply(names(.llms), function(custom_id) {
    result <- results_by_custom_id[[custom_id]]
    
    if (!is.null(result) && result$response$status_code == 200) {
      # Extract response content
      response_body <- result$response$body
      
      # Extract assistant message content
      assistant_reply <- result$response$body$choices$message$content
      metadata        <- extract_metadata(api_obj,result$response$body)
      
      # Update LLMMessage object with response
      llm <- add_message(
        .llm = .llms[[custom_id]],
        .role = "assistant",
        .content = assistant_reply,
        .meta = metadata
      )
      
      return(llm)
    } else {
      if (!is.null(result$error)) {
        warning(sprintf("Error for custom_id %s: %s", custom_id, result$error$message))
      } else {
        warning(sprintf("Result for custom_id %s was unsuccessful or not found", custom_id))
      }
      return(.llms[[custom_id]])
    }
  })
  
  # Restore original names
  names(updated_llms) <- original_names
  
  # Remove batch attributes before returning to avoid reuse conflicts
  attr(updated_llms, "batch_id") <- NULL
  attr(updated_llms, "file_id") <- NULL
  
  return(updated_llms)
}

#' List Groq Batch Requests
#'
#' Retrieves batch request details from the Groq API.
#'
#' @param .api_url Base URL for the Groq API (default: "https://api.groq.com/").
#' @param .limit Maximum number of batches to retrieve (default: 20).
#' @param .max_tries Maximum retry attempts for requests (default: 3).
#' @param .timeout Request timeout in seconds (default: 60).
#'
#' @return A tibble with batch details including batch ID, status, creation time, and request counts.
#'
#' @export
list_groq_batches <- function(.api_url = "https://api.groq.com/", 
                              .limit = 20, 
                              .max_tries = 3,
                              .timeout = 60) {
  # Retrieve API key
  api_key <- Sys.getenv("GROQ_API_KEY")
  if (api_key == "") {
    stop("API key is not set. Please set it with: Sys.setenv(GROQ_API_KEY = \"YOUR-KEY-GOES-HERE\").")
  }
  
  # Set up request URL with query parameters
  request <- httr2::request(.api_url) |>
    httr2::req_url_path("openai/v1/batches") |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key),
      .redact = "Authorization"
    ) |>
    httr2::req_url_query(limit = .limit)
  
  # Perform the request with retries and error handling
  response <- request |>
    perform_generic_request(.timeout = .timeout, .max_tries = .max_tries)
  
  # Parse response and handle any errors
  if ("error" %in% names(response$content)) {
    stop(sprintf("Groq API Error: %s", response$content$error$message))
  }
  
  # Extract batch list details
  batches <- response$content$data
  if (length(batches) == 0) {
    return(tibble::tibble())
  }
  
  batch_list <- purrr::map_dfr(batches, function(batch) {
    tibble::tibble(
      batch_id = batch$id,
      status = batch$status,
      created_at = lubridate::as_datetime(batch$created_at, origin = "1970-01-01"),
      expires_at = lubridate::as_datetime(batch$expires_at, origin = "1970-01-01"),
      req_total = batch$request_counts$total,
      req_completed = batch$request_counts$completed,
      req_failed = batch$request_counts$failed,
      input_file_id = batch$input_file_id,
      output_file_id = batch$output_file_id,
      error_file_id = batch$error_file_id
    )
  })
  
  return(batch_list)
}

#' Groq API Provider Function
#'
#' The `groq()` function acts as an interface for interacting with the Groq API 
#' through `tidyllm`'s main verbs. Currently, Groq only supports `groq_chat()` 
#' for chat-based interactions and `groq_transcribe()` for transcription tasks.
#'
#' Since `groq_transcribe()` is unique to Groq and does not have a general verb counterpart, 
#' `groq()` currently routes messages only to `groq_chat()` when used with verbs like `chat()`.
#'
#' @param ... Parameters to be passed to the Groq-specific function, 
#'   such as model configuration, input text, or API-specific options.
#' @param .called_from An internal argument that specifies which action (e.g., 
#'   `chat`) the function is being invoked from. 
#'   This argument is automatically managed and should not be modified by the user.
#'
#' @return The result of the requested action, depending on the specific function invoked 
#'   (currently, only an updated `LLMMessage` object for `groq_chat()`).
#'
#' @export
groq <- create_provider_function(
  .name = "groq",
  chat = groq_chat,
  list_models = groq_list_models,
  send_batch = send_groq_batch,
  check_batch = check_groq_batch,
  fetch_batch = fetch_groq_batch,
  list_batches = list_groq_batches
)

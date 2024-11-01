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
#' result <- groq(msg)
#' 
#' # With custom parameters
#' result2 <- groq(msg, 
#'                .model = "llama-3.2-vision",
#'                .temperature = 0.5, 
#'                .max_tokens = 512)
#' }
#'
#' @export
groq <- function(.llm,
                 .model = "llama-3.2-11b-vision-preview",
                 .max_tokens = 1024,
                 .temperature = NULL,
                 .top_p = NULL,
                 .frequency_penalty = NULL,
                 .presence_penalty = NULL,
                 .stop = NULL,
                 .seed = NULL,
                 .api_url = "https://api.groq.com/",
                 .json = FALSE,
                 .timeout = 60,
                 .verbose = FALSE,
                 .stream = FALSE,
                 .dry_run = FALSE,
                 .max_tries = 3) {

  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = inherits(.llm, "LLMMessage"),
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
    "Input .json must be logical" = is.logical(.json),
    "Input .verbose must be logical" = is.logical(.verbose),
    "Input .max_tries must be integer-valued numeric" = is_integer_valued(.max_tries),
    "Input .dry_run must be logical" = is.logical(.dry_run)
  ) |>
    validate_inputs()
  
  # Get formatted message list for Groq models
  messages <- .llm$to_api_format("openai",no_system=TRUE)
  
  # Retrieve API key from environment variables
  api_key <- Sys.getenv("GROQ_API_KEY")
  if ((api_key == "")& .dry_run==FALSE){
    stop("API key is not set. Please set it with: Sys.setenv(GROQ_API_KEY = \"YOUR-KEY-GOES-HERE\")")
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
    stream = .stream
  )
  request_body <- base::Filter(Negate(is.null), request_body)
  
  # Handle JSON mode
  if (.json == TRUE) {
    request_body$response_format <- list(type = "json_object")
  }
  
  groq_request <- httr2::request(.api_url) |>
    httr2::req_url_path("/openai/v1/chat/completions") |>
    httr2::req_headers(
      `Authorization` = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(data = request_body)
  
  # Perform the API request using perform_api_request
  response <- perform_api_request(
    .request = groq_request,
    .api = "groq",
    .stream = .stream,
    .timeout = .timeout,
    .max_tries = .max_tries,
    .parse_response_fn = function(body_json) {
      if ("error" %in% names(body_json)) {
        sprintf("Groq API returned an Error:\nType: %s\nMessage: %s",
                body_json$error$type,
                body_json$error$message) |>
          stop()
      }
      
      # Check if content is present in the response
      if (!"choices" %in% names(body_json) || length(body_json$choices) == 0) {
        stop("Received empty response from OpenAI API")
      }
      
      assistant_reply <- body_json$choices[[1]]$message$content
      return(assistant_reply)
    },
    .dry_run = .dry_run
  )
  
  # Return only the request object in a dry run.
  if (.dry_run) {
    return(response)  
  }
  
  # Extract assistant reply and rate limiting info from response headers
  assistant_reply <- response$assistant_reply
  rl <- ratelimit_from_header(response$headers,"groq")
  
  # Update the rate-limit environment with info from rl
  update_rate_limit("groq", rl)
  
  # Show request rate limit info if we are verbose
  if (.verbose == TRUE) {
    glue::glue("Groq API answer received at {rl$this_request_time}.
              Remaining requests rate limit: {rl$ratelimit_requests_remaining}/{rl$ratelimit_requests}
              Requests rate limit reset at: {rl$ratelimit_requests_reset_time} 
              Remaining tokens   rate limit: {rl$ratelimit_tokens_remaining}/{rl$ratelimit_tokens}
              Tokens rate limit reset at: {rl$ratelimit_tokens_reset_time} \n
              ") |> cat("\n")
  }
  
  # Create a deep copy of the LLMMessage object
  llm_copy <- .llm$clone_deep()
  
  # Add model's message to the history of the LLMMessage object
  llm_copy$add_message("assistant", assistant_reply , json = .json)
  
  return(llm_copy)
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


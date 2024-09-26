

#' Call the Anthropic API to interact with Claude models
#'
#' @param .llm An existing LLMMessage object or an initial text prompt.
#' @param .model The model identifier (default: "claude-3-5-sonnet-20240620").
#' @param .max_tokens The maximum number of tokens to generate (default: 1024).
#' @param .temperature Control for randomness in response generation (optional).
#' @param .top_k Top k sampling parameter (optional).
#' @param .top_p Nucleus sampling parameter (optional).
#' @param .system Additional system parameters (optional).
#' @param .metadata Additional metadata for the request (optional).
#' @param .stop_sequences Sequences that stop generation (optional).
#' @param .tools Additional tools used by the model (optional).
#' @param .api_url Base URL for the API (default: "https://api.anthropic.com/v1/messages").
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .verbose Should additional information be shown after the API call
#' @param .wait Should we wait for rate limits if necessary?
#' @param .min_tokens_reset How many tokens should be remaining to wait until we wait for token reset?
#' @param .stream Stream back the response piece by piece (default: FALSE).
#'
#' @return Returns an updated LLMMessage object.
#' @export
claude <- function(.llm,
                   .model = "claude-3-5-sonnet-20240620",
                   .max_tokens = 1024,
                   .temperature = NULL,
                   .top_k = NULL,
                   .top_p = NULL,
                   .metadata = NULL,
                   .stop_sequences = NULL,
                   .tools = NULL,
                   .api_url = "https://api.anthropic.com/",
                   .verbose = FALSE,
                   .wait=TRUE,
                   .min_tokens_reset = 0L,
                   .timeout = 60,
                   .stream = FALSE
                   ) {  
  
  #Validate inputes to the Claude function
  c(
    ".llm must be an LLMMessage object"    = inherits(.llm, "LLMMessage"),
    ".max_tokens must be an integer"       = is_integer_valued(.max_tokens),
    ".timeout must be an integer-valued numeric (seconds till timeout)" = is_integer_valued(.timeout),
    ".temperature must be numeric if provided"   = is.null(.temperature)  | is.numeric(.temperature),
    ".top_k must be numeric if provided"         = is.null(.top_k) | is.numeric(.top_k),
    ".top_p must be numeric if provided"         = is.null(.top_p) | is.numeric(.top_p),
    ".stop_sequences must be a character vector" = is.null(.stop_sequences) | is.character(.stop_sequences),
    ".verbose must be logical"                   = is.logical(.verbose),
    ".wait must be logical"                      = is.logical(.wait),
    ".stream must be logical"                    = is.logical(.stream),
    ".min_tokens_reset must be an integer"       = is_integer_valued(.min_tokens_reset)
  ) |>
    validate_inputs()
  
  #Get the formated message list so we can send it to a claude model
  messages <- .llm$to_api_format("claude")
  
  # Retrieve API key from environment variables
  api_key <- base::Sys.getenv("ANTHROPIC_API_KEY")
  if (api_key == "") stop("API key is not set. Please set it with : Sys.setenv(ANTHROPIC_API_KEY = \"YOUR-KEY-GOES-HERE\").")
  
  #If the rate limit environment is set we wait for the rate limit
  if(.wait==TRUE & !is.null(.tidyllm_rate_limit_env[["claude"]])){
    wait_rate_limit("claude",.min_tokens_reset)
  }  
  
  # Data payload
  request_body <- list(
    model = .model,
    max_tokens = .max_tokens,
    messages = messages,
    system = .llm$system_prompt,
    temperature = .temperature,
    top_k = .top_k,
    top_p = .top_p,
    metadata = .metadata,
    stop_sequences = .stop_sequences,
    stream = .stream,
    tools = .tools
  )
  
  # Filter out NULL values from the body
  request_body <- base::Filter(Negate(is.null), request_body)
  
  #An internal callback function  for streaming with claude
  callback_claude_stream <- function(.stream){
    # Read the stream content as raw text
    stream_content <- rawToChar(.stream, multiple = FALSE)
    
    # Split the content into event and data sections
    lines <- strsplit(stream_content, "\n")[[1]]
    
    for (line in lines) {
      # Ignore empty lines
      if (line == "") next
      
      # Check for event lines
      if (grepl("^event:", line)) {
        # Handle different types of events
        if (grepl("message_start", line)) {
          .tidyllm_stream_env$stream <- ""
        } else if (grepl("message_stop", line)) {
          cat("\n---------\nStream finished\n---------\n")
          return(FALSE)
        }
      } 
      
      # Check for data lines (JSON content)
      if (grepl("^data:", line)) {
        # Extract the JSON part after "data: "
        json_part <- sub("^data: ", "", line)
        
        # Try to parse the JSON content
        tryCatch({
          parsed_event <- jsonlite::fromJSON(json_part)
          
          # Handle content block deltas
          if (parsed_event$type == "content_block_delta") {
            delta_content <- parsed_event$delta$text
            .tidyllm_stream_env$stream <- as.character(glue::glue("{.tidyllm_stream_env$stream}{delta_content}"))
            cat(delta_content)
            flush.console()
          }
          
        }, error = function(e) {
          message("Failed to parse JSON: ", e$message)
        })
      }
    }
    
    return(TRUE)
  }

  # Perform the request using httr2 package
  if (.stream == TRUE) {
    # Initialize the streaming environment variable
    .tidyllm_stream_env$stream <- ""
    cat("\n---------\nStart Claude streaming: \n---------\n")
    
    # Perform the streaming request and process it with the callback function
    response <- httr2::req_perform_stream(
      httr2::req_body_json(
        httr2::req_headers(
          httr2::req_url_path(httr2::request(.api_url), "/v1/messages"), 
          `x-api-key` = api_key,
          `anthropic-version` = "2023-06-01",
          `content-type` = "application/json; charset=utf-8"
        ), 
        data = request_body
      ), 
      callback_claude_stream, 
      buffer_kb = 0.05, 
      round = "line"
    )
    
    # Assign the final streamed response
    assistant_reply <- .tidyllm_stream_env$stream
    
    # Capture response headers for rate limiting information
    response_headers <- httr2::resp_headers(response)
    
  } else {
    # Non-streaming mode
    response <- httr2::req_perform(
      httr2::req_timeout(
        httr2::req_body_json(
          httr2::req_headers(
            httr2::req_url_path(httr2::request(.api_url), "/v1/messages"), 
            `x-api-key` = api_key,
            `anthropic-version` = "2023-06-01",
            `content-type` = "application/json; charset=utf-8"
          ), 
          data = request_body
        ), 
        .timeout
      )
    )
    
    # Parse the response body as JSON when not streaming
    body_json <- httr2::resp_body_json(response)
    assistant_reply <- body_json$content[[1]]$text
    
    # Capture response headers for rate limiting information
    response_headers <- httr2::resp_headers(response)
  }
  
  # Update the rate-limit environment with the headers
  rl <- list(
    this_request_time             = strptime(response_headers["date"], format="%a, %d %b %Y %H:%M:%S", tz="GMT"),
    ratelimit_requests            = as.integer(response_headers["anthropic-ratelimit-requests-limit"]),
    ratelimit_requests_remaining  = as.integer(response_headers["anthropic-ratelimit-requests-remaining"]),
    ratelimit_requests_reset_time = as.POSIXct(
      response_headers["anthropic-ratelimit-requests-reset"]$`anthropic-ratelimit-requests-reset`,
      format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
    ratelimit_tokens              = as.integer(response_headers["anthropic-ratelimit-tokens-limit"]),
    ratelimit_tokens_remaining    = as.integer(response_headers["anthropic-ratelimit-tokens-remaining"]),
    ratelimit_tokens_reset_time   = as.POSIXct(response_headers["anthropic-ratelimit-tokens-reset"]$`anthropic-ratelimit-tokens-reset`,
                                               format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
  )
  
  # Initialize or update the rate limit environment
  initialize_api_env("claude")
  update_rate_limit("claude", rl)
  
  # Return the updated LLMMessage object
  llm_copy <- .llm$clone_deep()
  llm_copy$add_message("assistant", assistant_reply)
  
  return(llm_copy)
}

#' Call the OpenAI API to interact with ChatGPT or o-reasoning models
#'
#' @param .llm An existing LLMMessage object or an initial text prompt.
#' @param .model The model identifier (default: "gpt-4o").
#' @param .max_tokens The maximum number of tokens to generate (default: 1024).
#' @param .temperature Control for randomness in response generation (optional).
#' @param .top_k Top k sampling parameter (optional).
#' @param .top_p Nucleus sampling parameter (optional).
#' @param .system Additional system parameters (optional).
#' @param .metadata Additional metadata for the request (optional).
#' @param .stop_sequences Sequences that stop generation (optional).
#' @param .tools Additional tools used by the model (optional).
#' @param .api_url Base URL for the API (default: https://api.openai.com/v1/completions).
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .verbose Should additional information be shown after the API call
#' @param .wait Should we wait for rate limits if necessary?
#' @param .min_tokens_reset How many tokens should be remaining to wait until we wait for token reset?
#'
#' @return Returns an updated LLMMessage object.
#' @export
chatgpt <- function(.llm,
                    .model = "gpt-4o",
                    .max_tokens = 1024,
                    .temperature = NULL,
                    .top_p = NULL,
                    .frequency_penalty = NULL,
                    .presence_penalty = NULL,
                    .api_url = "https://api.openai.com/",
                    .timeout = 60,
                    .image_detail = "auto",
                    .verbose = FALSE,
                    .wait=TRUE,
                    .min_tokens_reset = 0L) {

  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = inherits(.llm, "LLMMessage"),
    "Input .max_tokens must be an integer" = is_integer_valued(.max_tokens),
    ".timeout must be an integer-valued numeric" = is_integer_valued(.timeout),
    ".temperature must be numeric if provided" = is.null(.temperature) | is.numeric(.temperature),
    ".top_p must be numeric if provided" = is.null(.top_p) | is.numeric(.top_p),
    ".frequency_penalty must be numeric if provided" = is.null(.frequency_penalty) | is.numeric(.frequency_penalty),
    ".presence_penalty must be numeric if provided" = is.null(.presence_penalty) | is.numeric(.presence_penalty),
    ".verbose must be logical"                   = is.logical(.verbose),
    ".wait must be logical"                      = is.logical(.wait),
    ".min_tokens_reset must be an integer"       = is_integer_valued(.min_tokens_reset)
  ) |>
    validate_inputs()
  
  #Get messages from llm object
  messages <- .llm$to_api_format("chatgpt")
  
  #Get the OpenAI API key
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") stop("API key is not set. Please set it with: Sys.setenv(OPENAI_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  
  #Fill the request body
  request_body <- list(
    model = .model,
    max_tokens = .max_tokens,
    messages = messages,
    temperature = .temperature,
    top_p = .top_p,
    frequency_penalty = .frequency_penalty,
    presence_penalty = .presence_penalty
  )
  request_body <- base::Filter(Negate(is.null), request_body)
  
  #Wait for the rate-limit if neccesary 
  if(.wait==TRUE & !is.null(.tidyllm_rate_limit_env[["chatgpt"]])){
    wait_rate_limit("chatgpt",.min_tokens_reset)
  }  
  
  response <- httr2::request(.api_url) |>
    httr2::req_url_path("/v1/chat/completions") |>
    httr2::req_headers(
      `Authorization` = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(data = request_body) |> 
    httr2::req_timeout(.timeout) |>
    httr2::req_perform()
  
  #Get the response body
  body_json <- response |> httr2::resp_body_json()
  
  # Retrieve the response headers and parse them so they can be stored in the .tidyllm_rate_limit_env
  response_headers <- response |> httr2::resp_headers()
  
  #Do some parsing for the rl list 
  request_time                  <- strptime(response_headers["date"]$date, format="%a, %d %b %Y %H:%M:%S", tz="GMT")
  ratelimit_requests_reset_dt   <- parse_duration_to_seconds(response_headers["x-ratelimit-reset-requests"]$`x-ratelimit-reset-requests`)
  ratelimit_requests_reset_time <- request_time + ratelimit_requests_reset_dt
  ratelimit_tokens_reset_dt     <- parse_duration_to_seconds(response_headers["x-ratelimit-reset-tokens"]$`x-ratelimit-reset-tokens`)
  ratelimit_tokens_reset_time   <- request_time + ratelimit_tokens_reset_dt
  
  #Ratelimit list
  rl <- list(
    this_request_time             = request_time ,
    ratelimit_requests            = as.integer(response_headers["x-ratelimit-limit-requests"]),
    ratelimit_requests_remaining  = as.integer(response_headers["x-ratelimit-remaining-requests"]),
    ratelimit_requests_reset_time = ratelimit_requests_reset_time ,
    ratelimit_tokens              = as.integer(response_headers["x-ratelimit-limit-tokens"]),
    ratelimit_tokens_remaining    = as.integer(response_headers["x-ratelimit-remaining-tokens"]),
    ratelimit_tokens_reset_time   = ratelimit_tokens_reset_time
  )
  
  #Initialize an environment to store rate-limit info
  initialize_api_env("chatgpt")
  
  #Update the rate-limit environment with info from rl
  update_rate_limit("chatgpt",rl)

  
  #Show Request rate limit info if we are verbose
  if(.verbose==TRUE){
    glue::glue("OpenAI API answer received at {rl$this_request_time}.
              Remaining requests rate limit: {rl$ratelimit_requests_remaining}/{rl$ratelimit_requests}
              Requests rate limit reset at: {rl$ratelimit_requests_reset_time} 
              Remaining tokens   rate limit: {rl$ratelimit_tokens_remaining}/{rl$ratelimit_tokens}
              Tokens rate limit reset at: {rl$ratelimit_tokens_reset_time} 
              
              ") |> cat()
  }
  
  
  # Check for errors
  if (httr2::resp_status(response) != 200) {
    stop("API request failed! Here is the raw response from the server", httr2::resp_raw(response))
  }
  
  # Create a deep copy of the LLMMessage object
  llm_copy <- .llm$clone_deep()
  
  # Add ChatGPT's message to the history of the LLMMessage object
  llm_copy$add_message("assistant", body_json$choices[[1]]$message$content)
  
  return(llm_copy)
}


#' Call the Groq API to interact with fast opensource models on Groq
#'
#' @param .llm An existing LLMMessage object or an initial text prompt.
#' @param .model The model identifier (default: "llama-3.2-90b-text-preview").
#' @param .max_tokens The maximum number of tokens to generate (default: 1024).
#' @param .temperature Control for randomness in response generation (optional).
#' @param .top_k Top k sampling parameter (optional).
#' @param .top_p Nucleus sampling parameter (optional).
#' @param .system Additional system parameters (optional).
#' @param .metadata Additional metadata for the request (optional).
#' @param .stop_sequences Sequences that stop generation (optional).
#' @param .tools Additional tools used by the model (optional).
#' @param .api_url Base URL for the API (default: "https://api.anthropic.com/v1/messages").
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .verbose Should additional information be shown after the API call
#' @param .wait Should we wait for rate limits if necessary?
#' @param .min_tokens_reset How many tokens should be remaining to wait until we wait for token reset?
#'
#' @return Returns an updated LLMMessage object.
#' @export
groq <- function(.llm,
                 .model = "llama-3.2-90b-text-preview",
                 .max_tokens = 1024,
                 .temperature = NULL,
                 .top_p = NULL,
                 .frequency_penalty = NULL,
                 .presence_penalty = NULL,
                 .api_url = "https://api.groq.com/",
                 .timeout = 60,
                 .verbose = FALSE,
                 .wait=TRUE,
                 .min_tokens_reset = 0L) {
  
  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = inherits(.llm, "LLMMessage"),
    "Input .max_tokens must be an integer" = is_integer_valued(.max_tokens),
    ".timeout must be an integer-valued numeric" = is_integer_valued(.timeout),
    ".temperature must be numeric if provided" = is.null(.temperature) | is.numeric(.temperature),
    ".top_p must be numeric if provided" = is.null(.top_p) | is.numeric(.top_p),
    ".frequency_penalty must be numeric if provided" = is.null(.frequency_penalty) | is.numeric(.frequency_penalty),
    ".presence_penalty must be numeric if provided" = is.null(.presence_penalty) | is.numeric(.presence_penalty),
    ".verbose must be logical"                   = is.logical(.verbose),
    ".wait must be logical"                      = is.logical(.wait),
    ".min_tokens_reset must be an integer"       = is_integer_valued(.min_tokens_reset)
  ) |>
    validate_inputs()
  
  # Get formatted message list for Groq models
  messages <- .llm$to_api_format("groq")
  
  if(.llm$has_image()){warning("The message history contains image data, but groq models only support texts.
                               Only text data is sent to the groq API")}
  
  # Retrieve API key from environment variables
  api_key <- Sys.getenv("GROQ_API_KEY")
  if (api_key == "") stop("API key is not set. Please set it with: Sys.setenv(GROQ_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  
  #Fill the request body
  request_body <- list(
    model = .model,
    max_tokens = .max_tokens,
    messages = messages,
    temperature = .temperature,
    top_p = .top_p,
    frequency_penalty = .frequency_penalty,
    presence_penalty = .presence_penalty
  )
  request_body <- base::Filter(Negate(is.null), request_body)
  
  #Wait for the rate-limit if neccesary 
  if(.wait==TRUE & !is.null(.tidyllm_rate_limit_env[["groq"]])){
    wait_rate_limit("groq",.min_tokens_reset)
  }  

  #Get the response via httr2
  response <- httr2::request(.api_url) |>
    httr2::req_url_path("/openai/v1/chat/completions") |>
    httr2::req_headers(
      `Authorization` = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(data = request_body) |> 
    httr2::req_timeout(.timeout) |>
    httr2::req_perform()
  
  #Get the response body
  body_json <- response |> httr2::resp_body_json()
  
  # Retrieve the response headers and parse them so they can be stored in the .tidyllm_rate_limit_env
  response_headers <- response |> httr2::resp_headers()
  
  #Do some parsing for the rl list 
  request_time                  <- strptime(response_headers["date"]$date, format="%a, %d %b %Y %H:%M:%S", tz="GMT")
  ratelimit_requests_reset_dt   <- parse_duration_to_seconds(response_headers["x-ratelimit-reset-requests"]$`x-ratelimit-reset-requests`)
  ratelimit_requests_reset_time <- request_time + ratelimit_requests_reset_dt
  ratelimit_tokens_reset_dt     <- parse_duration_to_seconds(response_headers["x-ratelimit-reset-tokens"]$`x-ratelimit-reset-tokens`)
  ratelimit_tokens_reset_time   <- request_time + ratelimit_tokens_reset_dt
  
  #Ratelimit list
  rl <- list(
    this_request_time             = request_time ,
    ratelimit_requests            = as.integer(response_headers["x-ratelimit-limit-requests"]),
    ratelimit_requests_remaining  = as.integer(response_headers["x-ratelimit-remaining-requests"]),
    ratelimit_requests_reset_time = ratelimit_requests_reset_time ,
    ratelimit_tokens              = as.integer(response_headers["x-ratelimit-limit-tokens"]),
    ratelimit_tokens_remaining    = as.integer(response_headers["x-ratelimit-remaining-tokens"]),
    ratelimit_tokens_reset_time   = ratelimit_tokens_reset_time
  )
  
  #Initialize an environment to store rate-limit info
  initialize_api_env("groq")
  
  #Update the rate-limit environment with info from rl
  update_rate_limit("groq",rl)
  
  #Show Request rate limit info if we are verbose
  if(.verbose==TRUE){
    glue::glue("Groq API answer received at {rl$this_request_time}.
              Remaining requests rate limit: {rl$ratelimit_requests_remaining}/{rl$ratelimit_requests}
              Requests rate limit reset at: {rl$ratelimit_requests_reset_time} 
              Remaining tokens   rate limit: {rl$ratelimit_tokens_remaining}/{rl$ratelimit_tokens}
              Tokens rate limit reset at: {rl$ratelimit_tokens_reset_time} 
              
              ") |> cat()
  }
  
  # Check for errors
  if (httr2::resp_status(response) != 200) {
    stop("API request failed! Here is the raw response from the server", httr2::resp_raw(response))
  }
  
  # Create a deep copy of the LLMMessage object
  llm_copy <- .llm$clone_deep()
  
  # Add model's message to the history of the LLMMessage object
  llm_copy$add_message("assistant", body_json$choices[[1]]$message$content)
  
  return(llm_copy)
}

#' Send LLMMessage to ollama API
#'
#' @param .llm An existing LLMMessage object or an initial text prompt.
#' @param .model The model identifier (default: "llama3").
#' @param .temperature Control for randomness in response generation (optional).
#' @param .seed Which seed should be used for random numbers  (optional).
#' @param .num_ctx The size of the context window in tokens (optional)
#' @param .stream  Should the answer be streamed to console as it comes (optional)
#' @param .ollama_server The URL of the ollama server to be used
#' @return Returns an updated LLMMessage object.
#' @export
ollama <- function(.llm,
                   .model="llama3",
                   .stream=FALSE,
                   .seed = NULL,
                   .json = FALSE,
                   .temperature = NULL,
                   .num_ctx = 2048,
                   .ollama_server= "http://localhost:11434"){

  #Validate the inputs
  c(
    "Input .llm must be an LLMMessage object" = inherits(.llm, "LLMMessage"),
    "Input .model must be a string" = is.character(.model),
    "Input .stream must be logical if provided"      = is.logical(.stream),
    "Input .json must be logical if provided"      = is.logical(.json),
    "Input .temperature must be numeric if provided" = is.null(.temperature) | is.numeric(.temperature),
    "Input .seed must be an integer-valued numeric if provided" = is.null(.seed) | is_integer_valued(.seed),
    "Input .num_ctx must be an integer-valued numeric if provided" = is.null(.num_ctx) | is_integer_valued(.num_ctx)
  ) |>
    validate_inputs()
  
  # Get formatted message list for ollama models
  ollama_messages <- .llm$to_api_format("ollama")
  
  ollama_options <- list(temperature=.temperature,
                         seed    = .seed,
                         num_ctx =.num_ctx) 
  ollama_options <- base::Filter(Negate(is.null), ollama_options)
  
  ollama_request_body <- list(model=.model, 
       messages=ollama_messages, 
       options = ollama_options,
       stream=.stream)
  
  #Add format to request body if applicable
  if(.json==TRUE){ollama_request_body$format <- "json"}
  
  #Build the request
  ollama_api <- httr2::request(.ollama_server) |>
    httr2::req_url_path("/api/chat") 
  
  assistant_reply <- NULL
  if(.stream==FALSE){
    resp <- ollama_api |>
      httr2::req_body_json(ollama_request_body) |> 
      httr2::req_perform() |> 
      httr2::resp_body_json()
    
    assistant_reply <- resp$message$content
  }
  
  
  #Do we want to stream the response and show the message when single bits arrive?
  if(.stream==TRUE){
    #Initialize the streaming environment variable
    .tidyllm_stream_env$stream <- ""
    cat("\n---------\nStart ollama streaming: \n---------\n")
    #A callback function to stream responses from ollama 
    callback_ollama_stream <- function(.stream){
      stream_content <- rawToChar(.stream, multiple = FALSE) |> 
        jsonlite::fromJSON()
      
      stream_response <- stream_content$message$content 
      .tidyllm_stream_env$stream <- glue::glue("{.tidyllm_stream_env$stream}{stream_response}") |> as.character()
      cat(stream_response)
      flush.console()
      TRUE
    }
    ollama_api |>
      httr2::req_body_json(ollama_request_body) |> 
      httr2::req_perform_stream(callback_ollama_stream, buffer_kb = 0.05, round="line")
    
    cat("\n---------\nStream finished\n---------\n")
    assistant_reply <- .tidyllm_stream_env$stream
  }
  
  
  # Create a deep copy of the LLMMessage object
  llm_copy <- .llm$clone_deep()
  
  # Add model's message to the history of the LLMMessage object
  llm_copy$add_message("assistant", assistant_reply)
  
  return(llm_copy)

}

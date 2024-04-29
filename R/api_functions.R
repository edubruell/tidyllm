

#' Call the Anthropic API to interact with Claude models
#'
#' @param .llm An existing LLMMessage object or an initial text prompt.
#' @param .model The model identifier (default: "claude-3-sonnet-20240229").
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
claude <- function(.llm,
                   .model = "claude-3-sonnet-20240229",
                   .max_tokens = 1024,
                   .temperature = NULL,
                   .top_k = NULL,
                   .top_p = NULL,
                   .metadata = NULL,
                   .stop_sequences = NULL,
                   .tools = NULL,
                   .api_url = "https://api.anthropic.com/v1/messages",
                   .verbose = FALSE,
                   .wait=TRUE,
                   .min_tokens_reset = 0L,
                   .timeout = 60) {  # Default timeout set to 60 seconds
  
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
    ".min_tokens_reset must be an integer"       = is_integer_valued(.min_tokens_reset)
  ) |>
    validate_inputs()
  
  
  #Get the formated message list so we can send it to a claude model
  messages <- .llm$to_api_format("claude")
  
  # Retrieve API key from environment variables
  api_key <- base::Sys.getenv("ANTHROPIC_API_KEY")
  if (api_key == "") stop("API key is not set. Please set it with : Sys.setenv(ANTHROPIC_API_KEY = \"YOUR-KEY-GOES-HERE\").")
  
  # Setup headers using httr package
  headers <- httr::add_headers(
    `x-api-key` = api_key,
    `anthropic-version` = "2023-06-01",
    `content-type` = "application/json; charset=utf-8"
  )
  
  # Data payload
  body <- list(
    model = .model,
    max_tokens = .max_tokens,
    messages = messages,
    system = .llm$system_prompt,
    temperature = .temperature,
    top_k = .top_k,
    top_p = .top_p,
    metadata = .metadata,
    stop_sequences = .stop_sequences,
    stream = FALSE,
    tools = .tools
  )
  
  # Filter out NULL values from the body
  body <- base::Filter(base::Negate(base::is.null), body)
  
  # Encode the body as JSON
  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)
  
  if(.wait==TRUE & !is.null(.tidyllm_rate_limit_env[["claude"]])){
    wait_rate_limit("claude",.min_tokens_reset)
  }  
  
  # Make the POST request using httr package
  response <- httr::POST(.api_url, headers, body = body_json, encode = "json", httr::timeout(.timeout))
  
  # Retrieve the response headers and parse them so they can be stored in the .tidyllm_rate_limit_env
  response_headers <- httr::headers(response)
  rl <- list(
    this_request_time             = strptime(response_headers["date"], format="%a, %d %b %Y %H:%M:%S", tz="GMT"),
    ratelimit_requests            = as.integer(response_headers["anthropic-ratelimit-requests-limit"]),
    ratelimit_requests_remaining  = as.integer(response_headers["anthropic-ratelimit-requests-remaining"]),
    ratelimit_requests_reset_time = as.POSIXct(
      response_headers["anthropic-ratelimit-requests-reset"]$`anthropic-ratelimit-requests-reset`,
      format="%Y-%m-%dT%H:%M:%SZ", tz="UTC") ,
    ratelimit_tokens              = as.integer(response_headers["anthropic-ratelimit-tokens-limit"]),
    ratelimit_tokens_remaining    = as.integer(response_headers["anthropic-ratelimit-tokens-remaining"]),
    ratelimit_tokens_reset_time   = as.POSIXct(response_headers["anthropic-ratelimit-tokens-reset"]$`anthropic-ratelimit-tokens-reset`,
                                               format="%Y-%m-%dT%H:%M:%SZ", tz="UTC") 
  )
  
  #Initialize an environment to store rate-limit info
  initialize_api_env("claude")
  #Update the rate-limit environment with info from rl
  update_rate_limit("claude",rl)

  
  # Check for errors
  if (httr::http_status(response)$category != "Success") {
    stop("API request failed: ", httr::http_status(response))
  }
  
  #Show Request rate limit info if we are verbose
  if(.verbose==TRUE){
    glue::glue("Anthropic API answer received at {rl$this_request_time}.
              Remaining requests rate limit: {rl$ratelimit_requests_remaining}/{rl$ratelimit_requests}
              Requests rate limit reset at: {rl$ratelimit_requests_reset_time} 
              Remaining tokens   rate limit: {rl$ratelimit_tokens_remaining}/{rl$ratelimit_tokens}
              Tokens rate limit reset at: {rl$ratelimit_tokens_reset_time} 
              
              ") |> cat()
  }
  
  # Get the content of the response
  response_content <-  httr::content(response, "text", encoding = "UTF-8")
  
  # Decode the response from JSON
  response_decoded <- jsonlite::fromJSON(response_content)
  
  # Create a deep copy of the LLMMessage object
  llm_copy <- .llm$clone_deep()
  
  #Add claudes message to the history of the llm message object
  llm_copy$add_message("assistant",response_decoded$content$text)
  
  return(llm_copy)
}

#' Call the OpenAI API to interact with ChatGPT models
#'
#' @param .llm An existing LLMMessage object or an initial text prompt.
#' @param .model The model identifier (default: "gpt-4").
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
                    .model = "gpt-4-turbo",
                    .max_tokens = 1024,
                    .temperature = NULL,
                    .top_p = NULL,
                    .frequency_penalty = NULL,
                    .presence_penalty = NULL,
                    .api_url = "https://api.openai.com/v1/chat/completions",
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
  
  # Get formatted message list for ChatGPT
  messages <- .llm$to_api_format("chatgpt",.image_detail)
  
  # Retrieve API key from environment variables
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") stop("API key is not set. Please set it with: Sys.setenv(OPENAI_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  
  # Setup headers using httr package
  headers <- httr::add_headers(
    `Authorization` = sprintf("Bearer %s", api_key),
    `Content-Type` = "application/json"
  )
  
  # Data payload
  body <- list(
    model = .model,
    max_tokens = .max_tokens,
    messages = messages,
    temperature = .temperature,
    top_p = .top_p,
    frequency_penalty = .frequency_penalty,
    presence_penalty = .presence_penalty
  )
  
  
  # Filter out NULL values from the body
  body <- Filter(Negate(is.null), body)
  
  # Encode the body as JSON
  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)
  
  #Wait for the rate-limit if neccesary 
  if(.wait==TRUE & !is.null(.tidyllm_rate_limit_env[["chatgpt"]])){
    wait_rate_limit("chatgpt",.min_tokens_reset)
  }  
  
  # Make the POST request using httr package
  response <- httr::POST(.api_url, headers, body = body_json, encode = "json", httr::timeout(.timeout))
  
  # Retrieve the response headers
  response_headers <- httr::headers(response)

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
  if (httr::http_status(response)$category != "Success") {
    stop("API request failed: ", httr::http_status(response))
  }
  
  # Get the content of the response
  response_content <- httr::content(response, "text", encoding = "UTF-8")
  
  # Decode the response from JSON
  response_decoded <- jsonlite::fromJSON(response_content)
  
  # Create a deep copy of the LLMMessage object
  llm_copy <- .llm$clone_deep()
  
  # Add ChatGPT's message to the history of the LLMMessage object
  llm_copy$add_message("assistant", response_decoded$choices$message$content)
  
  return(llm_copy)
}


#' Call the Groq API to interact with fast opensource models on Groq
#'
#' @param .llm An existing LLMMessage object or an initial text prompt.
#' @param .model The model identifier (default: "gpt-4").
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
#'
#' @return Returns an updated LLMMessage object.
#' @export
groq <- function(.llm,
                 .model = "mixtral-8x7b-32768",
                 .max_tokens = 1024,
                 .temperature = NULL,
                 .top_p = NULL,
                 .frequency_penalty = NULL,
                 .presence_penalty = NULL,
                 .api_url = "https://api.groq.com/openai/v1/chat/completions",
                 .timeout = 60) {
  
  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = inherits(.llm, "LLMMessage"),
    "Input .max_tokens must be an integer" = is_integer_valued(.max_tokens),
    ".timeout must be an integer-valued numeric" = is_integer_valued(.timeout),
    ".temperature must be numeric if provided" = is.null(.temperature) | is.numeric(.temperature),
    ".top_p must be numeric if provided" = is.null(.top_p) | is.numeric(.top_p),
    ".frequency_penalty must be numeric if provided" = is.null(.frequency_penalty) | is.numeric(.frequency_penalty),
    ".presence_penalty must be numeric if provided" = is.null(.presence_penalty) | is.numeric(.presence_penalty)
  ) |>
    validate_inputs()
  
  # Get formatted message list for Groq models
  messages <- .llm$to_api_format("groq")
  
  if(.llm$has_image()){warning("The message history contains image data, but groq models only support texts.
                               Only text data is sent to the groq API")}
  
  # Retrieve API key from environment variables
  api_key <- Sys.getenv("GROQ_API_KEY")
  if (api_key == "") stop("API key is not set. Please set it with: Sys.setenv(GROQ_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  
  # Setup headers using httr package
  headers <- httr::add_headers(
    `Authorization` = sprintf("Bearer %s", api_key),
    `Content-Type` = "application/json; charset=utf-8"
  )
  
  # Data payload
  body <- list(
    model = .model,
    max_tokens = .max_tokens,
    messages = messages,
    temperature = .temperature,
    top_p = .top_p,
    frequency_penalty = .frequency_penalty,
    presence_penalty = .presence_penalty
  )
  
  # Filter out NULL values from the body
  body <- Filter(Negate(is.null), body)
  
  # Encode the body as JSON
  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)
  
  # Make the POST request using httr package
  response <- httr::POST(.api_url, headers, body = body_json, encode = "json", httr::timeout(.timeout))
  
  # Check for errors
  if (httr::http_status(response)$category != "Success") {
    stop("API request failed: ", httr::http_status(response))
  }
  
  # Get the content of the response
  response_content <- httr::content(response, "text", encoding = "UTF-8")
  
  # Decode the response from JSON
  response_decoded <- jsonlite::fromJSON(response_content)
  
  # Create a deep copy of the LLMMessage object
  llm_copy <- .llm$clone_deep()
  
  # Add model's message to the history of the LLMMessage object
  llm_copy$add_message("assistant", response_decoded$choices$message$content)
  
  return(llm_copy)
}

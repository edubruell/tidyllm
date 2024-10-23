#' Call the Groq API to interact with fast opensource models on Groq
#'
#' @param .llm An existing LLMMessage object.
#' @param .model The model identifier (default: "llama-3.2-11b-vision-preview").
#' @param .max_tokens The maximum number of tokens to generate (default: 1024).
#' @param .temperature Control for randomness in response generation (optional).
#' @param .top_p Nucleus sampling parameter (optional).
#' @param .frequency_penalty Controls repetition frequency (optional).
#' @param .presence_penalty Controls how much to penalize repeating content (optional)
#' @param .api_url Base URL for the API (default: "https://api.anthropic.com/v1/messages").
#' @param .json Should output be structured as JSON  (default: FALSE).
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .verbose Should additional information be shown after the API call
#' @param .wait Should we wait for rate limits if necessary?
#' @param .min_tokens_reset How many tokens should be remaining to wait until we wait for token reset?
#' @param .dry_run If TRUE, perform a dry run and return the request object.
#'
#' @return Returns an updated LLMMessage object.
#' @export
groq <- function(.llm,
                 .model = "llama-3.2-11b-vision-preview",
                 .max_tokens = 1024,
                 .temperature = NULL,
                 .top_p = NULL,
                 .frequency_penalty = NULL,
                 .presence_penalty = NULL,
                 .api_url = "https://api.groq.com/",
                 .json = FALSE,
                 .timeout = 60,
                 .verbose = FALSE,
                 .wait=TRUE,
                 .min_tokens_reset = 0L,
                 .dry_run = FALSE) {
  
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
    ".json must be logical if provided"          = is.logical(.json),
    ".min_tokens_reset must be an integer"       = is_integer_valued(.min_tokens_reset),
    ".dry_run must be logical"                   = is.logical(.dry_run)
  ) |>
    validate_inputs()
  
  # Get formatted message list for Groq models
  messages <- .llm$to_api_format("groq")
  
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
  
  # Handle JSON mode
  if (.json == TRUE) {
    # Add response_format to request_body
    request_body$response_format <- list(type = "json_object")
  }
  
  #Wait for the rate-limit if neccesary 
  if(.wait==TRUE & !is.null(.tidyllm_rate_limit_env[["groq"]])){
    wait_rate_limit("groq",.min_tokens_reset)
  }  
  
  
  groq_request <- httr2::request(.api_url) |>
    httr2::req_url_path("/openai/v1/chat/completions") |>
    httr2::req_headers(
      `Authorization` = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(data = request_body) |> 
    httr2::req_timeout(.timeout) 
  
  # Return only the request object in a dry run.
  if (.dry_run) {
    return(groq_request)  
  }
  
  # Perform the request
  response <- groq_request |> 
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
  llm_copy$add_message("assistant", body_json$choices[[1]]$message$content,json=.json)
  
  return(llm_copy)
}


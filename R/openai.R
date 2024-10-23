#' Send LLMMessages to the OpenAI API to interact with ChatGPT
#'
#' @param .llm An  LLMMessage object. 
#' @param .model The model identifier (default: "gpt-4o").
#' @param .max_tokens The maximum number of tokens to generate (default: 1024).
#' @param .temperature Control for randomness in response generation (optional).
#' @param .top_k Top k sampling parameter (optional).
#' @param .top_p Nucleus sampling parameter (optional).
#' @param .frequency_penalty Controls repetition frequency (optional).
#' @param .presence_penalty Controls how much to penalize repeating content (optional)
#' @param .api_url Base URL for the API (default: https://api.openai.com/v1/completions).
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .json Should output be in JSON  mode (default: FALSE).
#' @param .verbose Should additional information be shown after the API call
#' @param .wait Should we wait for rate limits if necessary?
#' @param .min_tokens_reset How many tokens should be remaining to wait until we wait for token reset?
#' @param .stream Stream back the response piece by piece (default: FALSE).
#' @param .dry_run If TRUE, perform a dry run and return the request object.
#'
#' @return Returns an updated LLMMessage object.
#' @export
chatgpt <- function(.llm,
                    .model = "gpt-4o",
                    .max_tokens = 1024,
                    .temperature = NULL,
                    .top_p = NULL,
                    .top_k = NULL,
                    .frequency_penalty = NULL,
                    .presence_penalty = NULL,
                    .api_url = "https://api.openai.com/",
                    .timeout = 60,
                    .verbose = FALSE,
                    .wait = TRUE,
                    .json = FALSE,
                    .min_tokens_reset = 0L,
                    .stream = FALSE,
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
    ".verbose must be logical" = is.logical(.verbose),
    ".wait must be logical" = is.logical(.wait),
    ".min_tokens_reset must be an integer" = is_integer_valued(.min_tokens_reset),
    ".json must be logical if provided"          = is.logical(.json),
    ".stream must be logical" = is.logical(.stream),
    ".dry_run must be logical"                   = is.logical(.dry_run)
  ) |>
    validate_inputs()
  
  # Get messages from llm object
  messages <- .llm$to_api_format("chatgpt")
  
  # Get the OpenAI API key
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") stop("API key is not set. Please set it with: Sys.setenv(OPENAI_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  
  # Wait for the rate-limit if necessary
  if (.wait == TRUE & !is.null(.tidyllm_rate_limit_env[["chatgpt"]])) {
    wait_rate_limit("chatgpt", .min_tokens_reset)
  }
  
  # Build the request body
  request_body <- list(
    model = .model,
    max_tokens = .max_tokens,
    messages = messages,
    temperature = .temperature,
    top_p = .top_p,
    top_k = .top_k,
    frequency_penalty = .frequency_penalty,
    presence_penalty = .presence_penalty,
    stream = .stream
  )
  request_body <- base::Filter(Negate(is.null), request_body)
  
  # Handle JSON mode
  if (.json == TRUE) {
    # Add response_format to request_body
    request_body$response_format <- list(type = "json_object")
  }
  
  # Build the request
  request <- httr2::request(.api_url) |>
    httr2::req_url_path("/v1/chat/completions") |>
    httr2::req_headers(
      `Authorization` = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(data = request_body)
  
  # Perform the API request using perform_api_request
  response <- perform_api_request(
    .request = request,
    .api = "chatgpt",
    .stream = .stream,
    .timeout = .timeout,
    .parse_response_fn = function(body_json) {
      assistant_reply <- body_json$choices[[1]]$message$content
      return(assistant_reply)
    },
    .dry_run = .dry_run
  )
  
  # Return only the request object in a dry run.
  if (.dry_run) {
    return(response)  
  }
  
  # Extract assistant reply and response headers
  response_headers <- response$headers
  assistant_reply <- response$assistant_reply
  
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
  
  # Update rate limit information (implement this according to your rate limit handling)
  #Show Request rate limit info if we are verbose
  if(.verbose==TRUE){
    glue::glue("OpenAI API answer received at {rl$this_request_time}.
              Remaining requests rate limit: {rl$ratelimit_requests_remaining}/{rl$ratelimit_requests}
              Requests rate limit reset at: {rl$ratelimit_requests_reset_time} 
              Remaining tokens   rate limit: {rl$ratelimit_tokens_remaining}/{rl$ratelimit_tokens}
              Tokens rate limit reset at: {rl$ratelimit_tokens_reset_time} 
              
              ") |> cat()
  }
  
  # Create a deep copy of the LLMMessage object and add the assistant's reply
  llm_copy <- .llm$clone_deep()
  llm_copy$add_message("assistant", assistant_reply,json=.json)
  
  return(llm_copy)
}


#' Generate Embeddings Using OpenAI API
#'
#' @param .llm An existing LLMMessage object (or a character vector of texts to embed)
#' @param .model The embedding model identifier (default: "text-embedding-3-small").
#' @param .truncate Whether to truncate inputs to fit the model's context length (default: TRUE).
#' @param .timeout Timeout for the API request in seconds (default: 120).
#' @param .dry_run If TRUE, perform a dry run and return the request object.
#' @param .wait Logical; if TRUE, respects rate limits by waiting when necessary (default: TRUE).
#' @param .min_tokens_reset Integer specifying the minimum token threshold before waiting for reset.
#' @return A matrix where each column corresponds to the embedding of a message in the message history.
#' @export
openai_embedding <- function(.llm,
                             .model = "text-embedding-3-small",
                             .truncate = TRUE,
                             .timeout = 120,
                             .dry_run = FALSE,
                             .min_tokens_reset = 0L,
                             .wait=TRUE) {
  
  # Get the OpenAI API key
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") stop("API key is not set. Please set it with: Sys.setenv(OPENAI_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  
  # Validate the inputs
  c(
    "Input .llm must be an LLMMessage object or a character vector" = inherits(.llm, "LLMMessage") | is.character(.llm),
    "Input .model must be a string" = is.character(.model),
    "Input .truncate must be logical" = is.logical(.truncate),
    "Input .timeout must be an integer-valued numeric (seconds till timeout)" = is.numeric(.timeout) && .timeout > 0,
    ".dry_run must be logical" = is.logical(.dry_run),
    ".wait must be logical" = is.logical(.wait),
    ".min_tokens_reset must be an integer" = is_integer_valued(.min_tokens_reset)
  ) |> validate_inputs()
  
  if (!is.character(.llm)) {
    openai_history <- Filter(function(x) {
      if ("role" %in% names(x)) {
        return(x$role %in% c("user", "assistant"))
      } else {
        return(FALSE)
      }
    }, .llm$message_history)
    
    # Extract messages and combine content and text media
    message_texts <- lapply(openai_history, function(m) {
      base_content <- m$content
      media_list <- m$media
      text_media <- extract_media(media_list, "text")
      text_media_combined <- paste(unlist(text_media), collapse = " ")
      combined_text <- paste(base_content, text_media_combined, sep = " ")
      combined_text
    })
  }
  
  if (is.character(.llm)) {
    message_texts <- .llm
  }
  
  # Prepare the request body
  request_body <- list(
    model = .model,
    input = message_texts
  )
  
  # If the rate limit environment is set, wait for the rate limit
  if(.wait==TRUE & !is.null(.tidyllm_rate_limit_env[["chatgpt"]])){
    wait_rate_limit("chatgpt", .min_tokens_reset)
  }  
  
  
  # Build the request
  request <- httr2::request("https://api.openai.com/v1/embeddings") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", api_key)
    ) |>
    httr2::req_body_json(request_body)
  
  # Return the request object if it's a dry run
  if (.dry_run) {
    return(request)
  }
  
  # Perform the API request
  response <- request |>
    httr2::req_timeout(.timeout) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
  
  # Check for API errors
  tryCatch({
    # Check for HTTP errors
    if (httr2::resp_is_error(response)) {
      # Try to parse the JSON response body
      error_message <- tryCatch({
        json_content <- httr2::resp_body_json(response)
        if (!is.null(json_content)) {
          paste0("API error response - ", json_content$error$message)
        } else {
          "Unknown error occurred"
        }
      }, error = function(e) {
        paste("HTTP error:", httr2::resp_status(response), "- Unable to parse error message")
      })
      
      stop(error_message)
    }    
    # Parse the response and extract embeddings
    # Parse the response
    response_content <- httr2::resp_body_json(response)
    
    # Parse and update rate limit info from headers
    rl <- ratelimit_from_header(response$headers,"chatgpt")
    initialize_api_env("chatgpt")
    update_rate_limit("chatgpt",rl)
    
    # Extract the embeddings
    embeddings <- response_content$data |> purrr::map("embedding")
    
    # Check if embeddings are present
    if (is.null(embeddings)) {
      stop("No embeddings returned in the response.")
    }
    
    # Convert embeddings to a matrix
    embedding_matrix <- do.call(cbind, embeddings)
    
    
    # Return the embeddings
    return(embedding_matrix)
    
  }, error = function(e) {
    stop("An error occurred during the API request - ", e$message)
  })
  
}




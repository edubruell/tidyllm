#' Send LLMMessage to Mistral API
#'
#' @param .llm An `LLMMessage` object.
#' @param .model The model identifier to use (default: `"mistral-large-latest"`). 
#' @param .stream Whether to stream back partial progress to the console. (default: `FALSE`).
#' @param .temperature Sampling temperature to use, between `0.0` and `1.5`. Higher values make the output more random, while lower values make it more focused and deterministic (default: `0.7`).
#' @param .top_p Nucleus sampling parameter, between `0.0` and `1.0`. The model considers tokens with top_p probability mass (default: `1`).
#' @param .max_tokens The maximum number of tokens to generate in the completion. Must be `>= 0` (default: `1024`).
#' @param .min_tokens The minimum number of tokens to generate in the completion. Must be `>= 0` (optional).
#' @param .seed The seed to use for random sampling. If set, different calls will generate deterministic results (optional).
#' @param .stop Stop generation if this token is detected, or if one of these tokens is detected when providing a list (optional).
#' @param .json Whether the output should be in JSON mode(default: `FALSE`).
#' @param .safe_prompt Whether to inject a safety prompt before all conversations (default: `FALSE`).
#' @param .timeout When should our connection time out in seconds (default: `120`).
#' @param .verbose Should additional information be shown after the API call? (default: `FALSE`)
#' @param .dry_run If `TRUE`, perform a dry run and return the request object (default: `FALSE`).
#' @param .max_tries Maximum retries to peform request
#' @return Returns an updated `LLMMessage` object.
#' @export
mistral <- function(.llm,
                    .model = "mistral-large-latest",
                    .stream = FALSE,
                    .seed = NULL,
                    .json = FALSE,
                    .temperature = 0.7,
                    .top_p = 1,
                    .stop = NULL,
                    .safe_prompt = FALSE,
                    .timeout = 120,
                    .max_tries = 3,
                    .max_tokens = 1024,
                    .min_tokens = NULL,
                    .dry_run = FALSE,
                    .verbose = FALSE) {
  
  # Validate the inputs
  c(
    "Input .llm must be an LLMMessage object" = inherits(.llm, "LLMMessage"),
    "Input .model must be a string or NULL" = is.null(.model) || (is.character(.model) && length(.model) == 1),
    "Input .stream must be logical" = is.logical(.stream) && length(.stream) == 1,
    "Input .temperature must be numeric between 0 and 1.5" = is.null(.temperature) || (is.numeric(.temperature) && .temperature >= 0 && .temperature <= 1.5),
    "Input .top_p must be numeric between 0 and 1" = is.null(.top_p) || (is.numeric(.top_p) && .top_p >= 0 && .top_p <= 1),
    "Input .max_tokens must be integer >= 0" = is.null(.max_tokens) || (is_integer_valued(.max_tokens) && .max_tokens >= 0),
    "Input .min_tokens must be integer >= 0 if provided" = is.null(.min_tokens) || (is_integer_valued(.min_tokens) && .min_tokens >= 0),
    "Input .seed must be integer >= 0 if provided" = is.null(.seed) || (is_integer_valued(.seed) && .seed >= 0),
    "Input .stop must be a string or vector of strings if provided" = is.null(.stop) || (is.character(.stop)),
    "Input .json must be logical" = is.logical(.json) && length(.json) == 1,
    "Input .safe_prompt must be logical" = is.logical(.safe_prompt) && length(.safe_prompt) == 1,
    "Input .max_tries must be integer-valued numeric" = is_integer_valued(.max_tries),
    "Input .timeout must be integer-valued numeric (seconds till timeout)" = is_integer_valued(.timeout),
    "Input .dry_run must be logical" = is.logical(.dry_run) && length(.dry_run) == 1,
    "Input .verbose must be logical" = is.logical(.verbose) && length(.verbose) == 1
  ) |>
    validate_inputs()
  
  # Mistral and groq have the same  OpenAI without a system prompt format
  mistral_messages <- .llm$to_api_format("openai",no_system=TRUE)
  
  #set options
  mistral_request_body <- list(
    model    = .model,
    messages = mistral_messages,
    temperature = .temperature,
    random_seed = .seed,
    max_tokens  = .max_tokens,
    min_tokens  = .min_tokens,
    stream      = .stream,
    top_p        = .top_p,          
    stop         = .stop,           
    safe_prompt  = .safe_prompt     
  )
  mistral_request_body <- base::Filter(Negate(is.null), mistral_request_body)
  
  # Add format to request body if applicable
  if (.json == TRUE) {
    mistral_request_body$response_format <- list(type="json_object")
  }
  
  # Retrieve API key from environment variables
  api_key <- Sys.getenv("MISTRAL_API_KEY")
  if ((api_key == "")& .dry_run==FALSE){
    stop("API key is not set. Please set it with: Sys.setenv(GROQ_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  }
  
  # Build the request
  request <- httr2::request("https://api.mistral.ai") |>
    httr2::req_url_path("/v1/chat/completions") |>
    httr2::req_headers(
      `Authorization` = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json",
      .redact = "Authorization"
    ) |>
    httr2::req_body_json(mistral_request_body)
  
  # Perform the API request
  response <- perform_api_request(
    .request = request,
    .api = "mistral",
    .stream = .stream,
    .timeout = .timeout,
    .max_tries = .max_tries,
    .parse_response_fn = function(body_json) {
      if(body_json$object=="error"){
        sprintf("Mistral API returned an Error (code: %s)\nMessage: %s",
                body_json$type,
                body_json$message) |>
          stop()
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
  
  assistant_reply <- response$assistant_reply
  
  # Extract response headers for rate-limiting
  # Extract response headers for rate-limiting
  response_headers <- response$headers
  request_time <- strptime(response_headers[["date"]], format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  
  # Extract tokens remaining and reset time
  ratelimit_tokens_remaining <- as.integer(response_headers[["x-ratelimitbysize-remaining-minute"]])
  ratelimit_tokens_limit <- as.integer(response_headers[["x-ratelimitbysize-limit-minute"]])
  ratelimit_tokens_reset_dt <- as.integer(response_headers[["ratelimitbysize-reset"]])
  ratelimit_tokens_reset_time <- request_time + ratelimit_tokens_reset_dt
  
  # For requests per second limit
  ratelimit_requests_remaining <- 0
  ratelimit_requests_reset_time <- request_time + 1  # Mistral allows 1 request per second
  
  # Rate-limit list
  rl <- list(
    this_request_time = request_time,
    ratelimit_requests_remaining = ratelimit_requests_remaining,
    ratelimit_requests_reset_time = ratelimit_requests_reset_time,
    ratelimit_tokens_remaining = ratelimit_tokens_remaining,
    ratelimit_tokens_reset_time = ratelimit_tokens_reset_time
  )
  
  # Update the rate-limit environment with info from rl
  update_rate_limit("mistral", rl)
  
  # Show request rate limit info if verbose is enabled
  if (.verbose == TRUE) {
    glue::glue("Mistral API answer received at {rl$this_request_time}.
                Remaining tokens: {rl$ratelimit_tokens_remaining}/{ratelimit_tokens_limit}
                Tokens rate limit resets at: {rl$ratelimit_tokens_reset_time}\n\n") |> cat("\n")
  }
  
  # Create a deep copy of the LLMMessage object
  llm_copy <- .llm$clone_deep()
  
  # Add model's message to the history of the LLMMessage object
  llm_copy$add_message("assistant", assistant_reply,json=.json)
  
  return(llm_copy)
}



#' Generate Embeddings Using Mistral API
#'
#' @param .llm An existing LLMMessage object (or a character vector of texts to embed)
#' @param .model The embedding model identifier (default: "mistral-embed").
#' @param .timeout Timeout for the API request in seconds (default: 120).
#' @param .dry_run If TRUE, perform a dry run and return the request object.
#' @param .max_tries Maximum retries to peform request
#' @return A matrix where each column corresponds to the embedding of a message in the message history.
#' @export
mistral_embedding <- function(.llm,
                              .model = "mistral-embed",
                              .timeout = 120,
                              .max_tries = 3,
                              .dry_run = FALSE) {
  
  # Retrieve API key from environment variables
  api_key <- Sys.getenv("MISTRAL_API_KEY")
  if ((api_key == "")& .dry_run==FALSE){
    stop("API key is not set. Please set it with: Sys.setenv(GROQ_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  }
  # Validate the inputs
  c(
    "Input .llm must be an LLMMessage object or a character vector" = inherits(.llm, "LLMMessage") | is.character(.llm),
    "Input .model must be a string" = is.character(.model),
    "Input .timeout must be an integer-valued numeric (seconds till timeout)" = is.numeric(.timeout) && .timeout > 0,
    ".dry_run must be logical" = is.logical(.dry_run)
  ) |> validate_inputs()
  
  if (!is.character(.llm)) {
    mistral_history <- Filter(function(x) {
      if ("role" %in% names(x)) {
        return(x$role %in% c("user", "assistant"))
      } else {
        return(FALSE)
      }
    }, .llm$message_history)
    
    # Extract messages and combine content and text media
    message_texts <- lapply(mistral_history, function(m) {
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
  
  # Build the request
  request <- httr2::request("https://api.mistral.ai/v1/embeddings") |>
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
    httr2::req_retry(max_tries = .max_tries,  
                     retry_on_failure = TRUE,
                     is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 503)) |>
    httr2::req_perform()
  
  # Check for API errors
  tryCatch({
    # Check for HTTP errors
    if (httr2::resp_is_error(response)) {
      # Try to parse the JSON response body
      error_message <- tryCatch({
        json_content <- httr2::resp_body_json(response)
        if (!is.null(json_content)) {
          paste0("API error response (Code ", json_content$code ,") ", json_content$message)
        } else {
          "Unknown error occurred"
        }
      }, error = function(e) {
        paste("HTTP error:", httr2::resp_status(response), "- Unable to parse error message")
      })
      
      stop(error_message)
    }    
    # Parse the response and extract embeddings
    response_content <- httr2::resp_body_json(response)
    embeddings <- response_content$data |> purrr::map("embedding")
    embedding_matrix <- do.call(cbind, embeddings)
    
    # Return the embeddings
    return(embedding_matrix)
    
  }, error = function(e) {
    stop("An error occurred during the API request - ", e$message)
  })
}

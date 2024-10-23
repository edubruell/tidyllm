
#' Interact with Claude AI models via the Anthropic API
#'
#' @param .llm An LLMMessage object containing the conversation history and system prompt.
#' @param .model Character string specifying the Claude model version (default: "claude-3-5-sonnet-20241022").
#' @param .max_tokens Integer specifying the maximum number of tokens in the response (default: 1024).
#' @param .temperature Numeric between 0 and 1 controlling response randomness.
#' @param .top_k Integer controlling diversity by limiting the top K tokens.
#' @param .top_p Numeric between 0 and 1 for nucleus sampling.
#' @param .metadata List of additional metadata to include with the request.
#' @param .stop_sequences Character vector of sequences that will halt response generation.
#' @param .tools List of additional tools or functions the model can use.
#' @param .api_url Base URL for the Anthropic API (default: "https://api.anthropic.com/").
#' @param .verbose Logical; if TRUE, displays additional information about the API call (default: FALSE).
#' @param .wait Logical; if TRUE, respects rate limits by waiting when necessary (default: TRUE).
#' @param .min_tokens_reset Integer specifying the minimum token threshold before waiting for reset.
#' @param .timeout Integer specifying the request timeout in seconds (default: 60).
#' @param .json Logical; if TRUE, instructs Claude to return responses in JSON format (default: FALSE).
#' @param .stream Logical; if TRUE, streams the response piece by piece (default: FALSE).
#' @param .dry_run Logical; if TRUE, returns the prepared request object without executing it (default: FALSE).
#'
#' @return A new LLMMessage object containing the original messages plus Claude's response.
#' @examples
#' \dontrun{
#' # Basic usage
#' msg <- llm_message("What is R programming?")
#' result <- claude(msg)
#' 
#' # With custom parameters
#' result2 <- claude(msg, 
#'                  .temperature = 0.7, 
#'                  .max_tokens = 2000)
#' }
#'
#' @export
claude <- function(.llm,
                   .model = "claude-3-5-sonnet-20241022",
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
                   .json =FALSE,
                   .stream = FALSE,
                   .dry_run = FALSE) {  
  
  # Validate inputs to the Claude function
  c(
    ".llm must be an LLMMessage object" = inherits(.llm, "LLMMessage"),
    ".max_tokens must be an integer" = is_integer_valued(.max_tokens),
    ".timeout must be an integer-valued numeric (seconds till timeout)" = is_integer_valued(.timeout),
    ".temperature must be numeric between 0 and 1 if provided" = 
      is.null(.temperature) | (is.numeric(.temperature) && .temperature >= 0 && .temperature <= 1),
    ".top_k must be a positive integer if provided" = 
      is.null(.top_k) | (is.numeric(.top_k) && .top_k > 0 && floor(.top_k) == .top_k),
    ".top_p must be numeric between 0 and 1 if provided" = 
      is.null(.top_p) | (is.numeric(.top_p) && .top_p >= 0 && .top_p <= 1),
    "Only one of .temperature or .top_p should be specified" = 
      is.null(.temperature) | is.null(.top_p),
    ".stop_sequences must be a character vector" = 
      is.null(.stop_sequences) | is.character(.stop_sequences),
    ".verbose must be logical" = is.logical(.verbose),
    ".wait must be logical" = is.logical(.wait),
    ".stream must be logical" = is.logical(.stream),
    ".json must be logical if provided" = is.logical(.json),
    ".min_tokens_reset must be an integer" = is_integer_valued(.min_tokens_reset),
    ".dry_run must be logical" = is.logical(.dry_run)
  ) |>
    validate_inputs()
  
  # Get the formatted message list so we can send it to a Claude model
  messages <- .llm$to_api_format("claude")
  
  # Handle JSON mode (which is not as explicitly supported as for other APIs)
  if (.json == TRUE) {
    #Get the raw messages text for some tests
    raw_messages <- purrr::map_chr(.llm$message_history,"content") |> stringr::str_c(collapse="\n")
    #Throw a warning i json mode is set but the word json is nowhere in the messages
    if(!grepl("json",stringr::str_to_lower(raw_messages), fixed = TRUE)){
      warning("JSON mode is enabled, but the word 'json' is not found in the messages. Make sure to explicitly ask for JSON formatting in your prompt to improve consistency in the response.")
    }
  }
  
  # Retrieve API key from environment variables
  api_key <- base::Sys.getenv("ANTHROPIC_API_KEY")
  if (api_key == "") stop("API key is not set. Please set it with: Sys.setenv(ANTHROPIC_API_KEY = \"YOUR-KEY-GOES-HERE\").")
  
  # If the rate limit environment is set, wait for the rate limit
  if(.wait==TRUE & !is.null(.tidyllm_rate_limit_env[["claude"]])){
    wait_rate_limit("claude", .min_tokens_reset)
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
  
  # Build the request
  request <- httr2::request(.api_url) |>
    httr2::req_url_path("/v1/messages") |>
    httr2::req_headers(
      `x-api-key` = api_key,
      `anthropic-version` = "2023-06-01",
      `content-type` = "application/json; charset=utf-8"
    ) |>
    httr2::req_body_json(data = request_body)
  
  
  # Perform the API request
  response <- tryCatch({
    perform_api_request(
      .request = request,
      .api = "claude", 
      .stream = .stream, 
      .timeout = .timeout, 
      .parse_response_fn = function(body_json) {
        if ("error" %in% names(body_json)) {
          error_type <- body_json$error$type
          error_message <- body_json$error$message
          
          # Handle specific Claude API error cases
          switch(error_type,
                 "invalid_request_error" = stop(sprintf("Invalid request: %s", error_message)),
                 "authentication_error" = stop("Authentication failed. Please check your API key."),
                 "permission_error" = stop("Permission denied. Please check your API key permissions."),
                 "rate_limit_error" = stop(sprintf("Rate limit exceeded: %s", error_message)),
                 "model_not_ready_error" = stop(sprintf("Model not ready: %s", error_message)),
                 "invalid_model_error" = stop(sprintf("Invalid model specified: %s", error_message)),
                 "content_policy_violation" = stop(sprintf("Content policy violation: %s", error_message)),
                 "context_length_exceeded" = stop(sprintf("Context length exceeded: %s", error_message)),
                 stop(sprintf("Unexpected error (%s): %s", error_type, error_message))
          )
        }
        
        # Check if content is present in the response
        if (!"content" %in% names(body_json) || length(body_json$content) == 0) {
          stop("Received empty response from Claude API")
        }
        
        assistant_reply <- body_json$content[[1]]$text
        return(assistant_reply)
      },
      .dry_run = .dry_run
    )
  }, error = function(e) {
    # Handle timeout errors specifically
    if (inherits(e, "httr2_timeout")) {
      stop(sprintf("Request timed out after %d seconds", .timeout))
    }
    
    # Handle connection errors
    if (inherits(e, "httr2_error")) {
      status <- attr(e, "status")
      if (!is.null(status)) {
        switch(as.character(status),
               "401" = stop("Unauthorized: Invalid API key"),
               "403" = stop("Forbidden: You don't have permission to use this model"),
               "404" = stop("Not Found: The requested resource doesn't exist"),
               "429" = stop("Rate limit exceeded. Please wait before making more requests"),
               "500" = stop("Internal server error from Claude API"),
               "503" = stop("Claude API is temporarily unavailable"),
               stop(sprintf("HTTP error %d: %s", status, conditionMessage(e)))
        )
      }
    }
    
    # Re-throw other errors
    stop(conditionMessage(e))
  })
  
  # Return only the request object in a dry run.
  if (.dry_run) {
    return(response)  
  }
  
  # Extract assistant reply and response headers
  response_headers <- response$headers
  assistant_reply  <- response$assistant_reply
  
  # Initialize or update the rate limit environment
  rl <- ratelimit_from_header(response_headers,"claude")
  initialize_api_env("claude")
  update_rate_limit("claude", rl)
  
  # Return the updated LLMMessage object
  llm_copy <- .llm$clone_deep()
  llm_copy$add_message("assistant", assistant_reply,json=.json)
  
  return(llm_copy)
}



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
#' @param .max_tries Maximum retries to peform request
#' @param .timeout Integer specifying the request timeout in seconds (default: 60).
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
#'                  .max_tokens = 1000)
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
                   .max_tries = 3,
                   .timeout = 60,
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
    ".stream must be logical" = is.logical(.stream),
    "Input .max_tries must be integer-valued numeric" = is_integer_valued(.max_tries),
    ".dry_run must be logical" = is.logical(.dry_run)
  ) |>
    validate_inputs()
  

  # Retrieve API key from environment variables
  api_key <- base::Sys.getenv("ANTHROPIC_API_KEY")
  if ((api_key == "") & .dry_run==FALSE){
    stop("API key is not set. Please set it with: Sys.setenv(ANTHROPIC_API_KEY = \"YOUR-KEY-GOES-HERE\").")
  }
  
  # Format message list for Claude model
  messages <- .llm$to_api_format("claude")
  
  # Build request with httr2
  request <- httr2::request(.api_url) |>
    httr2::req_url_path("/v1/messages") |>
    httr2::req_headers(
      `x-api-key` = Sys.getenv("ANTHROPIC_API_KEY"),
      `anthropic-version` = "2023-06-01",
      `content-type` = "application/json; charset=utf-8",
      .redact = "x-api-key"
    ) |>
    httr2::req_body_json(data = list(
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
    ) |> purrr::discard(is.null))  # Filter out NULL values
  
  # Perform API request with retries and backoff handled in perform_api_request
  response <- tryCatch({
    perform_api_request(
      .request = request,
      .api = "claude",
      .stream = .stream,
      .timeout = .timeout, 
      .parse_response_fn = function(body_json) {
        if (!"content" %in% names(body_json) || length(body_json$content) == 0) {
          stop("Received empty response from Claude API")
        }
        body_json$content[[1]]$text  # Return the parsed assistant reply
      },
      .dry_run = .dry_run
    )
  }, error = function(e) {
    # Handle specific Claude API errors and HTTP errors
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
    stop(conditionMessage(e))  # Re-throw other errors
  })
  
  # Return only the request object in a dry run.
  if (.dry_run) {
    return(response)  
  }
  
  # Extract the assistant reply and headers from response
  assistant_reply <- response$assistant_reply
  response_headers <- response$headers
  
  # Update rate limits as before if you still need to track them for monitoring
  rl <- ratelimit_from_header(response_headers, "claude")
  update_rate_limit("claude",rl )
  
  # Show rate limit info if verbose
  if (.verbose == TRUE) {
    glue::glue(
      "\nCLaude API answer received at {rl$this_request_time}.
      Remaining requests rate limit: {rl$ratelimit_requests_remaining}/{rl$ratelimit_requests}
      Requests rate limit reset at: {rl$ratelimit_requests_reset_time}
      Remaining tokens rate limit: {rl$ratelimit_tokens_remaining}/{rl$ratelimit_tokens}
      Tokens rate limit reset at: {rl$ratelimit_tokens_reset_time}\n"
    ) |> cat()
  }
  
  # Return the updated LLMMessage object
  llm_copy <- .llm$clone_deep()
  llm_copy$add_message("assistant", assistant_reply, json = FALSE)
  
  return(llm_copy)
}




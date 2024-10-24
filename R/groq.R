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
#' @param .wait If TRUE, waits for rate limits to reset if necessary before sending the request (default: TRUE).
#' @param .min_tokens_reset The number of tokens remaining when the system should wait for token reset (default: 1000L).
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
                 .wait = TRUE,
                 .min_tokens_reset = 1000L,
                 .stream = FALSE,
                 .dry_run = FALSE) {

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
    "Input .wait must be logical" = is.logical(.wait),
    "Input .min_tokens_reset must be an integer" = is_integer_valued(.min_tokens_reset) & .min_tokens_reset >= 0,
    "Input .dry_run must be logical" = is.logical(.dry_run)
  ) |>
    validate_inputs()
  
  # Get formatted message list for Groq models
  messages <- .llm$to_api_format("groq")
  
  # Retrieve API key from environment variables
  api_key <- Sys.getenv("GROQ_API_KEY")
  if (api_key == "") stop("API key is not set. Please set it with: Sys.setenv(GROQ_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  
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
  
  # Wait for the rate-limit if necessary
  if (.wait == TRUE & !is.null(.tidyllm_rate_limit_env[["groq"]])) {
    wait_rate_limit("groq", .min_tokens_reset)
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
  
  # Extract assistant reply and rate limiting info from response headers
  assistant_reply <- response$assistant_reply
  rl <- ratelimit_from_header(response$headers,"groq")
  
  # Initialize an environment to store rate-limit info
  initialize_api_env("groq")
  
  # Update the rate-limit environment with info from rl
  update_rate_limit("groq", rl)
  
  # Show request rate limit info if we are verbose
  if (.verbose == TRUE) {
    glue::glue("Groq API answer received at {rl$this_request_time}.
              Remaining requests rate limit: {rl$ratelimit_requests_remaining}/{rl$ratelimit_requests}
              Requests rate limit reset at: {rl$ratelimit_requests_reset_time} 
              Remaining tokens   rate limit: {rl$ratelimit_tokens_remaining}/{rl$ratelimit_tokens}
              Tokens rate limit reset at: {rl$ratelimit_tokens_reset_time} \n
              ") |> cat()
  }
  
  # Create a deep copy of the LLMMessage object
  llm_copy <- .llm$clone_deep()
  
  # Add model's message to the history of the LLMMessage object
  llm_copy$add_message("assistant", assistant_reply , json = .json)
  
  return(llm_copy)
}






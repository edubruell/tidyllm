#' Send LLM Messages to an OpenAI Chat Completions endpoint on Azure 
#'
#' @description
#' This function sends a message history to the Azure OpenAI Chat Completions API and returns the assistant's reply. 
#' This function is work in progress and not fully tested
#'
#' @param .llm An `LLMMessage` object containing the conversation history.
#' @param .endpoint_url Base URL for the API (default:  Sys.getenv("AZURE_ENDPOINT_URL")).
#' @param .deployment The identifier of the model that is deployed (default: "gpt-4o-mini").
#' @param .api_version Which version of the API is deployed (default: "2024-08-01-preview")
#' @param .max_completion_tokens An upper bound for the number of tokens that can be generated for a completion, including visible output tokens and reasoning tokens.
#' @param .frequency_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far.
#' @param .logit_bias A named list modifying the likelihood of specified tokens appearing in the completion.
#' @param .logprobs Whether to return log probabilities of the output tokens (default: FALSE).
#' @param .top_logprobs An integer between 0 and 20 specifying the number of most likely tokens to return at each token position.
#' @param .presence_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far.
#' @param .seed If specified, the system will make a best effort to sample deterministically.
#' @param .stop Up to 4 sequences where the API will stop generating further tokens.
#' @param .stream If set to TRUE, the answer will be streamed to console as it comes (default: FALSE).
#' @param .temperature What sampling temperature to use, between 0 and 2. Higher values make the output more random.
#' @param .top_p An alternative to sampling with temperature, called nucleus sampling.
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .verbose Should additional information be shown after the API call (default: FALSE).
#' @param .json Should output be in JSON mode (default: FALSE).
#' @param .max_tries Maximum retries to perform request
#' @param .json_schema A JSON schema object as R list to enforce the output structure (If defined has precedence over JSON mode).
#' @param .dry_run If TRUE, perform a dry run and return the request object (default: FALSE).
#'
#'
#' @return A new `LLMMessage` object containing the original messages plus the assistant's response.
#' @examples
#' \dontrun{
#' # Basic usage
#' msg <- llm_message("What is R programming?")
#' result <- azure_openai(msg)
#' 
#' # With custom parameters
#' result2 <- azure_openai(msg, 
#'                  .deployment = "gpt-4o-mini",
#'                  .temperature = 0.7, 
#'                  .max_tokens = 1000)
#' }
#'
#' @export
azure_openai <- function(
    .llm,
    .endpoint_url = Sys.getenv("AZURE_ENDPOINT_URL"),
    .deployment = "gpt-4o-mini",
    .api_version = "2024-08-01-preview",
    .max_completion_tokens = NULL,
    .frequency_penalty = NULL,
    .logit_bias = NULL,
    .logprobs = FALSE,
    .top_logprobs = NULL,
    .presence_penalty = NULL,
    .seed = NULL,
    .stop = NULL,
    .stream = FALSE,
    .temperature = NULL,
    .top_p = NULL,
    .timeout = 60,
    .verbose = FALSE,
    .json = FALSE,
    .json_schema = NULL,
    .dry_run = FALSE,
    .max_tries = 3
) {
  
  #Check enpoint
  if (.endpoint_url == ""& .dry_run==FALSE){
   stop("No valid Azure endpoint defined. Please set it either as input to this function or with: Sys.setenv(AZURE_ENDPOINT_URL = \"https://endpoint.openai.azure.com/\")")
  }
  
  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = inherits(.llm, "LLMMessage"),
    "Input .deployment must be a string" = is.character(.deployment),
    "Input .max_completion_tokens must be NULL or a positive integer" = is.null(.max_completion_tokens) | (is_integer_valued(.max_completion_tokens) & .max_completion_tokens > 0),    ".frequency_penalty must be numeric or NULL" = is.null(.frequency_penalty) | is.numeric(.frequency_penalty),
    "Input .logit_bias must be a list or NULL" = is.null(.logit_bias) | is.list(.logit_bias),
    "Input .logprobs must be logical or NULL" = is.null(.logprobs) | is.logical(.logprobs),
    "Input .top_logprobs must be NULL or an integer between 0 and 20" = is.null(.top_logprobs) | (is_integer_valued(.top_logprobs) & .top_logprobs >= 0 & .top_logprobs <= 20),
    "Input .presence_penalty must be numeric or NULL" = is.null(.presence_penalty) | is.numeric(.presence_penalty),
    "Input .seed must be NULL or an integer" = is.null(.seed) | is_integer_valued(.seed),
    "Input .stop must be NULL or a character vector or string" = is.null(.stop) | is.character(.stop),
    "Input .stream must be logical or NULL" = is.null(.stream) | is.logical(.stream),
    "Input .temperature must be numeric or NULL" = is.null(.temperature) | is.numeric(.temperature),
    "Input .top_p must be numeric or NULL" = is.null(.top_p) | is.numeric(.top_p),
    "Input .timeout must be integer-valued numeric" = is_integer_valued(.timeout),
    "Input .verbose must be logical" = is.logical(.verbose),
    "Input .json must be logical" = is.logical(.json),
    "Input .json_schema must be NULL or a list" = is.null(.json_schema) | is.list(.json_schema),
    "Input .dry_run must be logical" = is.logical(.dry_run)
  ) |> validate_inputs()
  
  #This filters out the system prompt for reasoning models.
  no_system_prompt <- FALSE
  if(.deployment %in% c("o1-preview","o1-mini")){
    message("Note: Reasoning models do not support system prompts")
    no_system_prompt <- TRUE
  }
  messages <- .llm$to_api_format("openai",no_system=no_system_prompt)
  
  # Get the OpenAI API key
  api_key <- Sys.getenv("AZURE_OPENAI_API_KEY")
  if ((api_key == "")& .dry_run==FALSE){
    stop("API key is not set. Please set it with: Sys.setenv(AZURE_OPENAI_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  }
  
  # Handle JSON schema and JSON mode
  response_format <- NULL
  if (!is.null(.json_schema)) {
    .json=TRUE
    response_format <- list(
      type = "json_schema",
      json_schema = .json_schema
    )
  } else if (.json == TRUE) {
    response_format <- list(
      type = "json_object"
    )
  }
  

  # Build the request body
  request_body <- list(
    messages = messages,
    frequency_penalty = .frequency_penalty,
    logit_bias = .logit_bias,
    logprobs = .logprobs,
    top_logprobs = .top_logprobs,
    max_completion_tokens = .max_completion_tokens,
    presence_penalty = .presence_penalty,
    response_format = response_format,
    seed = .seed,
    stop = .stop,
    stream = .stream,
    temperature = .temperature,
    top_p = .top_p
  )
  request_body <- base::Filter(Negate(is.null), request_body)
  
  # Build the request
  request <- httr2::request(.endpoint_url) |>
    httr2::req_url_path(paste0("openai/deployments/", .deployment,"/chat/completions")) |>
    httr2::req_url_query(`api-version` = .api_version) |>
    httr2::req_headers(
      `Content-Type` = "application/json",
      `api-key` = api_key,
    )  |>
    httr2::req_body_json(data = request_body)
  
  # Perform the API request using perform_api_request
  response <- perform_api_request(
    .request = request,
    .api = "azure_openai",
    .stream = .stream,
    .timeout = .timeout,
    .max_tries = .max_tries,
    .parse_response_fn = function(body_json) {
      if ("error" %in% names(body_json)) {
        sprintf("Azure Openai API returned an Error Message: %s",
                body_json$error$message) |>
          stop()
      }
      
      # Check if content is present in the response
      if (!"choices" %in% names(body_json) || length(body_json$choices) == 0) {
        stop("Received empty response from Azure OpenAI API")
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
  rl <- ratelimit_from_header(response$headers,"azure_openai")
  
  # Update the rate-limit environment with info from rl
  update_rate_limit("azure_openai", rl)
  
  # Show rate limit info if verbose
  if (.verbose == TRUE) {
    glue::glue(
      "\nAzure OpenAI API answer received at {rl$this_request_time}.
      Remaining requests rate limit: {rl$ratelimit_requests_remaining}/{rl$ratelimit_requests}
      Requests rate limit reset at: {rl$ratelimit_requests_reset_time}
      Remaining tokens rate limit: {rl$ratelimit_tokens_remaining}/{rl$ratelimit_tokens}
      Tokens rate limit reset at: {rl$ratelimit_tokens_reset_time}\n"
    ) |> cat("\n")
  }
  
  # Create a deep copy of the LLMMessage object and add the assistant's reply
  llm_copy <- .llm$clone_deep()
  llm_copy$add_message("assistant", assistant_reply, json = .json)
  
  return(llm_copy)
}


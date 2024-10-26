#' Send LLM Messages to the OpenAI Chat Completions API
#'
#' @description
#' This function sends a message history to the OpenAI Chat Completions API and returns the assistant's reply.
#'
#' @param .llm An `LLMMessage` object containing the conversation history.
#' @param .model The identifier of the model to use (default: "gpt-4o").
#' @param .max_completion_tokens An upper bound for the number of tokens that can be generated for a completion, including visible output tokens and reasoning tokens.
#' @param .frequency_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far.
#' @param .logit_bias A named list modifying the likelihood of specified tokens appearing in the completion.
#' @param .logprobs Whether to return log probabilities of the output tokens (default: FALSE).
#' @param .top_logprobs An integer between 0 and 20 specifying the number of most likely tokens to return at each token position.
#' @param .presence_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far.
#' @param .seed If specified, the system will make a best effort to sample deterministically.
#' @param .service_tier Specifies the latency tier to use for processing the request (default: "auto").
#' @param .stop Up to 4 sequences where the API will stop generating further tokens.
#' @param .stream If set to TRUE, the answer will be streamed to console as it comes (default: FALSE).
#' @param .temperature What sampling temperature to use, between 0 and 2. Higher values make the output more random.
#' @param .top_p An alternative to sampling with temperature, called nucleus sampling.
#' @param .api_url Base URL for the API (default: "https://api.openai.com/").
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .verbose Should additional information be shown after the API call (default: FALSE).
#' @param .wait Should we wait for rate limits if necessary? (default: TRUE).
#' @param .json Should output be in JSON mode (default: FALSE).
#' @param .json_schema A JSON schema object as R list to enforce the output structure (If defined has precedence over JSON mode).
#' @param .min_tokens_reset How many tokens should be remaining to wait until we wait for token reset? (default: 1000L).
#' @param .dry_run If TRUE, perform a dry run and return the request object (default: FALSE).
#'
#'
#' @return A new `LLMMessage` object containing the original messages plus the assistant's response.
#' @examples
#' \dontrun{
#' # Basic usage
#' msg <- llm_message("What is R programming?")
#' result <- openai(msg)
#' 
#' # With custom parameters
#' result2 <- openai(msg, 
#'                  .model "gpt-4o-mini",
#'                  .temperature = 0.7, 
#'                  .max_tokens = 1000)
#' }
#'
#' @export
openai <- function(
    .llm,
    .model = "gpt-4o",
    .max_completion_tokens = NULL,
    .frequency_penalty = NULL,
    .logit_bias = NULL,
    .logprobs = FALSE,
    .top_logprobs = NULL,
    .presence_penalty = NULL,
    .seed = NULL,
    .service_tier = "auto",
    .stop = NULL,
    .stream = FALSE,
    .temperature = NULL,
    .top_p = NULL,
    .api_url = "https://api.openai.com/",
    .timeout = 60,
    .verbose = FALSE,
    .wait = TRUE,
    .json = FALSE,
    .json_schema = NULL,
    .min_tokens_reset = 1000L,
    .dry_run = FALSE
) {
  
  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = inherits(.llm, "LLMMessage"),
    "Input .model must be a string" = is.character(.model),
    "Input .max_completion_tokens must be NULL or a positive integer" = is.null(.max_completion_tokens) | (is_integer_valued(.max_completion_tokens) & .max_completion_tokens > 0),    ".frequency_penalty must be numeric or NULL" = is.null(.frequency_penalty) | is.numeric(.frequency_penalty),
    "Input .logit_bias must be a list or NULL" = is.null(.logit_bias) | is.list(.logit_bias),
    "Input .logprobs must be logical or NULL" = is.null(.logprobs) | is.logical(.logprobs),
    "Input .top_logprobs must be NULL or an integer between 0 and 20" = is.null(.top_logprobs) | (is_integer_valued(.top_logprobs) & .top_logprobs >= 0 & .top_logprobs <= 20),
    "Input .presence_penalty must be numeric or NULL" = is.null(.presence_penalty) | is.numeric(.presence_penalty),
    "Input .seed must be NULL or an integer" = is.null(.seed) | is_integer_valued(.seed),
    "Input .service_tier must be NULL or a string" = is.null(.service_tier) | is.character(.service_tier),
    "Input .stop must be NULL or a character vector or string" = is.null(.stop) | is.character(.stop),
    "Input .stream must be logical or NULL" = is.null(.stream) | is.logical(.stream),
    "Input .temperature must be numeric or NULL" = is.null(.temperature) | is.numeric(.temperature),
    "Input .top_p must be numeric or NULL" = is.null(.top_p) | is.numeric(.top_p),
    "Input .api_url must be a string" = is.character(.api_url),
    "Input .timeout must be integer-valued numeric" = is_integer_valued(.timeout),
    "Input .verbose must be logical" = is.logical(.verbose),
    "Input .wait must be logical" = is.logical(.wait),
    "Input .json must be logical" = is.logical(.json),
    "Input .json_schema must be NULL or a list" = is.null(.json_schema) | is.list(.json_schema),
    "Input .min_tokens_reset must be integer-valued numeric" = is_integer_valued(.min_tokens_reset),
    "Input .dry_run must be logical" = is.logical(.dry_run)
  ) |> validate_inputs()
  
  # Get messages from llm object
  messages <- .llm$to_api_format("openai")
  
  #This filters out the system prompt for reasoning models. There must be a better way to do this more robustly
  if(.model %in% c("o1-preview","o1-mini")){
    message("Note: Reasoning models do not support system prompts")
    messages <- Filter(function(x){
      if ("role" %in% names(x)) {
        return(x$role %in% c("user","assistant"))
      } else {
        return(FALSE)
      }},messages)
  }
  
  # Get the OpenAI API key
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if ((api_key == "") & .dry_run==FALSE){
    stop("API key is not set. Please set it with: Sys.setenv(OPENAI_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  }
  
  # Wait for the rate-limit if necessary
  if (.wait == TRUE & !is.null(.tidyllm_rate_limit_env[["openai"]])) {
    wait_rate_limit("openai", .min_tokens_reset)
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
    model = .model,
    messages = messages,
    frequency_penalty = .frequency_penalty,
    logit_bias = .logit_bias,
    logprobs = .logprobs,
    top_logprobs = .top_logprobs,
    max_completion_tokens = .max_completion_tokens,
    presence_penalty = .presence_penalty,
    response_format = response_format,
    seed = .seed,
    service_tier = .service_tier,
    stop = .stop,
    stream = .stream,
    temperature = .temperature,
    top_p = .top_p
  )
  request_body <- base::Filter(Negate(is.null), request_body)
  
  # Build the request
  request <- httr2::request(.api_url) |>
    httr2::req_url_path("/v1/chat/completions") |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(data = request_body)
  
  # Perform the API request using perform_api_request
  response <- perform_api_request(
    .request = request,
    .api = "openai",
    .stream = .stream,
    .timeout = .timeout,
    .parse_response_fn = function(body_json) {
      if ("error" %in% names(body_json)) {
        sprintf("Openai API returned an Error:\nType: %s\nMessage: %s",
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
  rl <- ratelimit_from_header(response$headers,"openai")
  
  # Initialize an environment to store rate-limit info
  initialize_api_env("openai")
  
  # Update the rate-limit environment with info from rl
  update_rate_limit("openai", rl)
  
  # Show rate limit info if verbose
  if (.verbose == TRUE) {
    glue::glue(
      "OpenAI API answer received at {rl$this_request_time}.
      Remaining requests rate limit: {rl$ratelimit_requests_remaining}/{rl$ratelimit_requests}
      Requests rate limit reset at: {rl$ratelimit_requests_reset_time}
      Remaining tokens rate limit: {rl$ratelimit_tokens_remaining}/{rl$ratelimit_tokens}
      Tokens rate limit reset at: {rl$ratelimit_tokens_reset_time}\n"
    ) |> cat()
  }
  
  # Create a deep copy of the LLMMessage object and add the assistant's reply
  llm_copy <- .llm$clone_deep()
  llm_copy$add_message("assistant", assistant_reply, json = .json)
  
  return(llm_copy)
}


#' @title ChatGPT Wrapper (Deprecated)
#' 
#' @description Provides a wrapper for the `openai()` function to facilitate
#' migration from the deprecated `chatgpt()` function. This ensures backward
#' compatibility while allowing users to transition to the updated features.
#' 
#' @param .llm An `LLMMessage` (passed directly to the `openai()` function)
#' @param .model A character string specifying the model to use. 
#' @param .max_tokens An integer specifying the maximum number of tokens  (mapped to `.max_completion_tokens` in `openai()`)
#' @param .temperature A numeric value for controlling randomness. This is
#' @param .top_p A numeric value for nucleus sampling, indicating the top
#' @param .top_k Currently unused, as it is not supported by `openai()`.
#' @param .frequency_penalty A numeric value that penalizes new tokens based on
#'   their frequency so far. 
#' @param .presence_penalty A numeric value that penalizes new tokens based on
#'   whether they appear in the text so far.
#' @param .api_url Character string specifying the API URL. Defaults to the
#'   OpenAI API endpoint.
#' @param .timeout An integer specifying the request timeout in seconds. This is
#' @param .verbose Will print additional information about the request (default: false)
#' @param .wait Logical, if `TRUE`, will wait for rate limits to reset (default: true)
#' @param .json Should json-mode be used? (detault: false)
#' @param .min_tokens_reset An integer specifying the minimum tokens before a rate limit reset is taken into account.
#' @param .stream Should the response  be processed as a stream (default: false)
#' @param .dry_run Shouldthe request is constructed but not actually sent. Useful for debugging and testing. (default: false)
#' 
#' @return An `LLMMessage` object with the assistant's reply.
#'
#' 
#' @details
#' This function is deprecated and is now a wrapper around `openai()`. It is
#' recommended to switch to using `openai()` directly in future code. The
#' `chatgpt()` function remains available to ensure backward compatibility for
#' existing projects.
#' 
#' @examples
#' \dontrun{
#' # Using the deprecated chatgpt() function
#' result <- chatgpt(.llm = llm_message(), .prompt = "Hello, how are you?")
#' }
#' 
#' @seealso Use [openai()] instead.
#' 
#' @export
chatgpt <- function(
    .llm,
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
    .dry_run = FALSE
) {
  # Issue deprecation warning
  lifecycle::deprecate_warn(
    when = "0.1.9", 
    what = "chatgpt()",
    with = "openai()"
  )
  
  # Map arguments from chatgpt() to openai()
  .max_completion_tokens <- .max_tokens
  .logprobs <- NULL  # Not used in chatgpt(), set to NULL
  .top_logprobs <- NULL  # Not used in chatgpt(), set to NULL
  .logit_bias <- NULL  # Not used in chatgpt(), set to NULL
  .seed <- NULL  # Not used in chatgpt(), set to NULL
  .service_tier <- NULL  # Not used in chatgpt(), set to NULL
  .stop <- NULL  # Not used in chatgpt(), set to NULL
  .json_schema <- NULL  # Not used in chatgpt(), set to NULL
  .frequency_penalty <- .frequency_penalty
  .presence_penalty <- .presence_penalty
  
  # Call the openai() function with mapped arguments
  result <- openai(
    .llm = .llm,
    .model = .model,
    .max_completion_tokens = .max_completion_tokens,
    .frequency_penalty = .frequency_penalty,
    .logit_bias = .logit_bias,
    .logprobs = .logprobs,
    .top_logprobs = .top_logprobs,
    .presence_penalty = .presence_penalty,
    .seed = .seed,
    .service_tier = .service_tier,
    .stop = .stop,
    .stream = .stream,
    .temperature = .temperature,
    .top_p = .top_p,
    .api_url = .api_url,
    .timeout = .timeout,
    .verbose = .verbose,
    .wait = .wait,
    .json = .json,
    .json_schema = .json_schema,
    .min_tokens_reset = .min_tokens_reset,
    .dry_run = .dry_run
  )
  
  return(result)
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
  if ((api_key == "") & .dry_run==FALSE){
    stop("API key is not set. Please set it with: Sys.setenv(OPENAI_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  }
  
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
  if(.wait==TRUE & !is.null(.tidyllm_rate_limit_env[["openai"]])){
    wait_rate_limit("openai", .min_tokens_reset)
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
    rl <- ratelimit_from_header(response$headers,"openai")
    initialize_api_env("openai")
    update_rate_limit("openai",rl)
    
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




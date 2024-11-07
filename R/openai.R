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
#' @param .stop Up to 4 sequences where the API will stop generating further tokens.
#' @param .stream If set to TRUE, the answer will be streamed to console as it comes (default: FALSE).
#' @param .temperature What sampling temperature to use, between 0 and 2. Higher values make the output more random.
#' @param .top_p An alternative to sampling with temperature, called nucleus sampling.
#' @param .api_url Base URL for the API (default: "https://api.openai.com/").
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .verbose Should additional information be shown after the API call (default: FALSE).
#' @param .json Should output be in JSON mode (default: FALSE).
#' @param .json_schema A JSON schema object as R list to enforce the output structure (If defined has precedence over JSON mode).
#' @param .max_tries Maximum retries to perform request
#' @param .dry_run If TRUE, perform a dry run and return the request object (default: FALSE).
#' @param .compatible If TRUE, skip API and rate-limit checks for OpenAI compatible APIs (default: FALSE).
#' @param .api_path  The path relative to the base `.api_url` for the API (default: "/v1/chat/completions").
#'
#'
#' @return A new `LLMMessage` object containing the original messages plus the assistant's response.
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
    .stop = NULL,
    .stream = FALSE,
    .temperature = NULL,
    .top_p = NULL,
    .api_url = "https://api.openai.com/",
    .timeout = 60,
    .verbose = FALSE,
    .json = FALSE,
    .json_schema = NULL,
    .max_tries = 3,
    .dry_run = FALSE,
    .compatible = FALSE,
    .api_path = "/v1/chat/completions"
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
    "Input .stop must be NULL or a character vector or string" = is.null(.stop) | is.character(.stop),
    "Input .stream must be logical or NULL" = is.null(.stream) | is.logical(.stream),
    "Input .temperature must be numeric or NULL" = is.null(.temperature) | is.numeric(.temperature),
    "Input .top_p must be numeric or NULL" = is.null(.top_p) | is.numeric(.top_p),
    "Input .api_url must be a string" = is.character(.api_url),
    "Input .timeout must be integer-valued numeric" = is_integer_valued(.timeout),
    "Input .verbose must be logical" = is.logical(.verbose),
    "Input .json must be logical" = is.logical(.json),
    "Input .json_schema must be NULL or a list" = is.null(.json_schema) | is.list(.json_schema),
    "Input .max_tries must be integer-valued numeric" = is_integer_valued(.max_tries),
    "Input .dry_run must be logical" = is.logical(.dry_run),
    "Input .compatible must be logical" = is.logical(.compatible),
    "Input .api_path must be a string" = is.character(.api_path)
  ) |> validate_inputs()
  
  #This filters out the system prompt for reasoning models.
  no_system_prompt <- FALSE
  if(.model %in% c("o1-preview","o1-mini")){
    message("Note: Reasoning models do not support system prompts")
    no_system_prompt <- TRUE
  }
  messages <- .llm$to_api_format("openai",no_system=no_system_prompt)
  

  if (!.compatible) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
    if ((api_key == "") & .dry_run == FALSE) {
      stop("API key is not set. Please set it with: Sys.setenv(OPENAI_API_KEY = \"YOUR-KEY-GOES-HERE\")")
    }
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
    stop = .stop,
    stream = .stream,
    temperature = .temperature,
    top_p = .top_p
  )
  request_body <- base::Filter(Negate(is.null), request_body)

  # Build the request, omitting Authorization header if .compatible is TRUE
  request <- httr2::request(.api_url) |>
    httr2::req_url_path(.api_path) |>
    httr2::req_body_json(data = request_body)
  
  if (!.compatible) {
    request <- request |> httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    )
  } else {
    request <- request |> httr2::req_headers(`Content-Type` = "application/json")
  }
  
  
  # Perform the API request using perform_api_request
  response <- perform_api_request(
    .request = request,
    .api = "openai",
    .stream = .stream,
    .timeout = .timeout,
    .max_tries = .max_tries,
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
  
  # Skip rate-limit environment handling if .compatible is set
  if (!.compatible) {
    rl <- ratelimit_from_header(response$headers, "openai")
    initialize_api_env("openai")
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
#' @param .json Should json-mode be used? (detault: false)
#' @param .stream Should the response  be processed as a stream (default: false)
#' @param .dry_run Should the request is constructed but not actually sent. Useful for debugging and testing. (default: false)
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
    .json = FALSE,
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
    .stop = .stop,
    .stream = .stream,
    .temperature = .temperature,
    .top_p = .top_p,
    .api_url = .api_url,
    .timeout = .timeout,
    .verbose = .verbose,
    .json = .json,
    .json_schema = .json_schema,
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
#' @param .max_tries Maximum retry attempts for requests (default: 3).
#' @return A matrix where each column corresponds to the embedding of a message in the message history.
#' @export
openai_embedding <- function(.llm,
                             .model = "text-embedding-3-small",
                             .truncate = TRUE,
                             .timeout = 120,
                             .dry_run = FALSE,
                             .max_tries = 3) {
  
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
    ".dry_run must be logical" = is.logical(.dry_run)
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
  #if(.wait==TRUE & !is.null(.tidyllm_rate_limit_env[["openai"]])){
  #  wait_rate_limit("openai", .min_tokens_reset)
  #}  
  
  
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

#' Send a Batch of Messages to OpenAI Batch API
#'
#' This function creates and submits a batch of messages to the OpenAI Batch API for asynchronous processing.
#'
#' @param .llms A list of LLMMessage objects containing conversation histories.
#' @param .model Character string specifying the OpenAI model version (default: "gpt-4o").
#' @param .max_completion_tokens Integer specifying the maximum tokens per response (default: NULL).
#' @param .frequency_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far.
#' @param .logit_bias A named list modifying the likelihood of specified tokens appearing in the completion.
#' @param .logprobs Whether to return log probabilities of the output tokens (default: FALSE).
#' @param .top_logprobs An integer between 0 and 20 specifying the number of most likely tokens to return at each token position.
#' @param .presence_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far.
#' @param .seed If specified, the system will make a best effort to sample deterministically.
#' @param .stop Up to 4 sequences where the API will stop generating further tokens.
#' @param .temperature What sampling temperature to use, between 0 and 2. Higher values make the output more random.
#' @param .top_p An alternative to sampling with temperature, called nucleus sampling.
#' @param .dry_run Logical; if TRUE, returns the prepared request object without executing it (default: FALSE).
#' @param .overwrite Logical; if TRUE, allows overwriting an existing batch ID associated with the request (default: FALSE).
#' @param .max_tries Maximum number of retries to perform the request (default: 3).
#' @param .timeout Integer specifying the request timeout in seconds (default: 60).
#' @param .json_schema A JSON schema  as R list to enforce the output structure (default: NULL).
#' @param .verbose Logical; if TRUE, additional info about the requests is printed (default: FALSE).
#' @param .json_schema A JSON schema object as R list to enforce the output structure (default: NULL).
#' @param .id_prefix Character string to specify a prefix for generating custom IDs when names in `.llms` are missing (default: "tidyllm_openai_req_").
#' 
#' @return An updated and named list of `.llms` with identifiers that align with batch responses, including a `batch_id` attribute.
#' @export
send_openai_batch <- function(.llms,
                              .model = "gpt-4o",
                              .max_completion_tokens = NULL,
                              .frequency_penalty = NULL,
                              .logit_bias = NULL,
                              .logprobs = FALSE,
                              .top_logprobs = NULL,
                              .presence_penalty = NULL,
                              .seed = NULL,
                              .stop = NULL,
                              .temperature = NULL,
                              .top_p = NULL,
                              .dry_run = FALSE,
                              .overwrite = FALSE,
                              .json_schema = NULL,
                              .max_tries = 3,
                              .timeout = 60,
                              .verbose = FALSE,
                              .id_prefix = "tidyllm_openai_req_") {
  
  # Input validation
  c(
    ".llms must be a list of LLMMessage objects" = is.list(.llms) && all(sapply(.llms, inherits, "LLMMessage")),
    ".max_completion_tokens must be NULL or a positive integer" = is.null(.max_completion_tokens) | (is_integer_valued(.max_completion_tokens) & .max_completion_tokens > 0),
    ".frequency_penalty must be numeric or NULL" = is.null(.frequency_penalty) | is.numeric(.frequency_penalty),
    ".logit_bias must be a list or NULL" = is.null(.logit_bias) | is.list(.logit_bias),
    ".logprobs must be logical or NULL" = is.null(.logprobs) | is.logical(.logprobs),
    ".top_logprobs must be NULL or an integer between 0 and 20" = is.null(.top_logprobs) | (is_integer_valued(.top_logprobs) & .top_logprobs >= 0 & .top_logprobs <= 20),
    ".presence_penalty must be numeric or NULL" = is.null(.presence_penalty) | is.numeric(.presence_penalty),
    ".seed must be NULL or an integer" = is.null(.seed) | is_integer_valued(.seed),
    ".stop must be NULL or a character vector or string" = is.null(.stop) | is.character(.stop),
    ".temperature must be numeric or NULL" = is.null(.temperature) | is.numeric(.temperature),
    ".top_p must be numeric or NULL" = is.null(.top_p) | is.numeric(.top_p),
    ".dry_run must be logical" = is.logical(.dry_run),
    ".verbose must be logical" = is.logical(.verbose),
    ".overwrite must be logical" = is.logical(.overwrite),
    ".id_prefix must be a character vector of length 1" = is.character(.id_prefix),
    ".max_tries must be integer-valued numeric" = is_integer_valued(.max_tries),
    ".timeout must be integer-valued numeric" = is_integer_valued(.timeout)
  ) |> validate_inputs()
  
  # Retrieve API key from environment variables
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "" && !.dry_run) {
    stop("API key is not set. Please set it with: Sys.setenv(OPENAI_API_KEY = \"YOUR-KEY-GOES-HERE\").")
  }
  
  # Check for unique non-missing names (excluding NA and empty strings)
  non_missing_names <- names(.llms)[!(is.na(names(.llms)) | names(.llms) == "")]
  if (anyDuplicated(non_missing_names)) {
    stop("Each specified name in .llms must be unique. Please ensure that all non-missing names are unique.")
  }
  
  # Check for existing batch_id
  if (!is.null(attr(.llms, "batch_id"))) {
    if (.overwrite) {
      warning("Batch ID is already set in the provided list. Overwriting with a new batch ID.")
    } else {
      stop("Batch ID is already set in the provided list. Set .overwrite = TRUE to overwrite.")
    }
  }
  
  #This filters out the system prompt for reasoning models.
  no_system_prompt <- FALSE
  if(.model %in% c("o1-preview","o1-mini")){
    message("Note: Reasoning models do not support system prompts")
    no_system_prompt <- TRUE
  }
  
  # Handle JSON schema and JSON mode
  response_format <- NULL
  if (!is.null(.json_schema)) {
    .json=TRUE
    response_format <- list(
      type = "json_schema",
      json_schema = .json_schema
    )
  } 
  
  # Prepare the request lines
  request_lines <- lapply(seq_along(.llms), function(i) { 
    custom_id <- names(.llms)[i]
    if (is.null(custom_id) || custom_id == "" || is.na(custom_id)) {
      custom_id <- paste0(.id_prefix, i)  # Generate a custom ID if name is missing
      names(.llms)[i] <<- custom_id  # Assign generated ID as the name of .llms
    }
    
    # Get messages from LLMMessage object
    messages <- .llms[[i]]$to_api_format("openai",no_system=no_system_prompt)
    
    # Build the request body
    body <- list(
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
      stop = .stop,
      temperature = .temperature,
      top_p = .top_p
    )
    
    body <- base::Filter(Negate(is.null), body)
    
    # Create the request line as JSON
    request_line <- list(
      custom_id = custom_id,
      method = "POST",
      url = "/v1/chat/completions",
      body = body
    )
    
    # Convert to JSON
    jsonlite::toJSON(request_line, auto_unbox = TRUE)
  })
  
  # Write the request lines to a temporary .jsonl file
  temp_file <- tempfile(fileext = ".jsonl")
  writeLines(unlist(request_lines), con = temp_file)
  
  if (.dry_run) {
    # Return the prepared .jsonl file path
    return(temp_file)
  }
  
  # Upload the .jsonl file via OpenAI's Files API
  upload_request <- httr2::request("https://api.openai.com/v1/files") |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key)
    ) |>
    httr2::req_body_multipart(
      purpose = "batch",
      file = curl::form_file(temp_file) #httr2::req_file(temp_file)
    )
  
  upload_response <- upload_request |>
    httr2::req_timeout(.timeout) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_retry(
      max_tries = .max_tries,
      retry_on_failure = TRUE,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 503)
    ) |>
    httr2::req_perform()
  
  upload_response_body <- httr2::resp_body_json(upload_response)
  
  if(.verbose){message("Batch request file uploaded via files API")}
  
  if ("error" %in% names(upload_response_body)) {
    sprintf("OpenAI API returned an Error during file upload:\nType: %s\nMessage: %s",
            upload_response_body$error$type,
            upload_response_body$error$message) |>
      stop()
  }
  
  input_file_id <- upload_response_body$id
  
  # Now, create the batch
  batch_request_body <- list(
    input_file_id = input_file_id,
    endpoint = "/v1/chat/completions",
    completion_window = "24h"
  )
  
  batch_request <- httr2::request("https://api.openai.com/v1/batches") |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(batch_request_body)
  
  batch_response <- batch_request |>
    httr2::req_timeout(.timeout) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_retry(
      max_tries = .max_tries,
      retry_on_failure = TRUE,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 503)
    ) |>
    httr2::req_perform()
  
  batch_response_body <- httr2::resp_body_json(batch_response)
  
  if(.verbose){message("Batch request for file sent")}
  if ("error" %in% names(batch_response_body)) {
    sprintf("OpenAI API returned an Error during batch creation:\nType: %s\nMessage: %s",
            batch_response_body$error$type,
            batch_response_body$error$message) |>
      stop()
  }
  
  # Attach batch_id as an attribute to .llms
  batch_id <- batch_response_body$id
  attr(.llms, "batch_id") <- batch_id
  attr(.llms, "json") <- .json
  
  
  # Optionally, remove the temporary file
  unlink(temp_file)
  
  return(.llms)
}


#' Check Batch Processing Status for OpenAI Batch API
#'
#' This function retrieves the processing status and other details of a specified OpenAI batch ID
#' from the OpenAI Batch API.
#'
#' @param .llms A list of LLMMessage objects.
#' @param .batch_id A manually set batch ID.
#' @param .dry_run Logical; if TRUE, returns the prepared request object without executing it (default: FALSE).
#' @param .max_tries Maximum retries to perform the request (default: 3).
#' @param .timeout Integer specifying the request timeout in seconds (default: 60).
#' @return A tibble with information about the status of batch processing.
#' @export
check_openai_batch <- function(.llms = NULL,
                               .batch_id = NULL,
                               .dry_run = FALSE,
                               .max_tries = 3,
                               .timeout = 60) {
  # Extract batch_id
  if (is.null(.batch_id)) {
    if (!is.null(.llms)) {
      .batch_id <- attr(.llms, "batch_id")
      if (is.null(.batch_id)) {
        stop("No batch_id attribute found in the provided list.")
      }
    } else {
      stop("Either .llms or .batch_id must be provided.")
    }
  }
  
  # Retrieve API key
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if ((api_key == "") & !.dry_run){
    stop("API key is not set.")
  }
  
  # Build request
  request_url <- paste0("https://api.openai.com/v1/batches/", .batch_id)
  request <- httr2::request(request_url) |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    )
  
  # If .dry_run is TRUE, return the request object for inspection
  if (.dry_run) {
    return(request)
  }
  
  # Perform request with retries and error handling
  response <- request |>
    httr2::req_timeout(.timeout) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_retry(
      max_tries = .max_tries,
      retry_on_failure = TRUE,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 503)
    ) |>
    httr2::req_perform()
  
  # Parse response
  response_body <- httr2::resp_body_json(response)
  if("error" %in% names(response_body)){
    sprintf("OpenAI API returned an Error:\nType: %s\nMessage: %s",
            response_body$error$type,
            response_body$error$message) |>
      stop()
  }
  
  # Create tibble with batch details
  result_tbl <- tibble::tibble(
    batch_id = response_body$id,
    status = response_body$status,
    created_at = lubridate::as_datetime(response_body$created_at),
    expires_at = lubridate::as_datetime(response_body$expires_at),
    total_requests = response_body$request_counts$total,
    completed_requests = response_body$request_counts$completed,
    failed_requests = response_body$request_counts$failed
  )
  
  return(result_tbl)
}

#' List OpenAI Batch Requests
#'
#' Retrieves batch request details from the OpenAI Batch API.
#'
#' @param .limit Maximum number of batches to retrieve (default: 20).
#' @param .max_tries Maximum retry attempts for requests (default: 3).
#' @param .timeout Request timeout in seconds (default: 60).
#'
#' @return A tibble with batch details: batch ID, status, creation time, expiration time, 
#' and request counts (total, completed, failed).
#'
#' @export
list_openai_batches <- function(.limit = 20,
                                .max_tries = 3,
                                .timeout = 60) {
  # Retrieve API key
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") {
    stop("API key is not set. Please set it with: Sys.setenv(OPENAI_API_KEY = \"YOUR-KEY-GOES-HERE\").")
  }
  
  # Set up request URL with query parameters
  request <- httr2::request("https://api.openai.com/v1/batches") |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_url_query(limit = .limit)
  
  # Perform the request with retries and error handling
  response <- request |>
    httr2::req_timeout(.timeout) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_retry(
      max_tries = .max_tries,
      retry_on_failure = TRUE,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 503)
    ) |>
    httr2::req_perform()
  
  response_body <- httr2::resp_body_json(response)
  
  if ("error" %in% names(response_body)) {
    sprintf("OpenAI API returned an Error:\nType: %s\nMessage: %s",
            response_body$error$type,
            response_body$error$message) |>
      stop()  
  }
  
  # Extract batch data and format as tibble
  batch_data <- response_body$data
  batch_tibble <- purrr::map_dfr(batch_data, ~ tibble::tibble(
    batch_id = .x$id,
    status = .x$status,
    created_at = as.POSIXct(.x$created_at, origin = "1970-01-01", tz = "UTC"),
    expires_at = as.POSIXct(.x$expires_at, origin = "1970-01-01", tz = "UTC"),
    request_total = .x$request_counts$total,
    request_completed = .x$request_counts$completed,
    request_failed = .x$request_counts$failed
  ))
  
  return(batch_tibble)
}

#' Fetch Results for an OpenAI Batch
#'
#' This function retrieves the results of a completed OpenAI batch and updates
#' the provided list of `LLMMessage` objects with the responses. It aligns each
#' response with the original request using the `custom_id`s generated in `send_openai_batch()`.
#'
#' @param .llms A list of `LLMMessage` objects that were part of the batch.
#' @param .batch_id Character; the unique identifier for the batch. By default this is NULL
#'                  and the function will attempt to use the `batch_id` attribute from `.llms`.
#' @param .dry_run Logical; if `TRUE`, returns the constructed request without executing it (default: `FALSE`).
#' @param .max_tries Integer; maximum number of retries if the request fails (default: `3`).
#' @param .timeout Integer; request timeout in seconds (default: `60`).
#'
#' @return A list of updated `LLMMessage` objects, each with the assistant's response added if successful.
#' @export
fetch_openai_batch <- function(.llms,
                               .batch_id = NULL,
                               .dry_run = FALSE,
                               .max_tries = 3,
                               .timeout = 60) {
  c(
    ".llms must be a list of LLMMessage objects with names as custom IDs" = is.list(.llms) && all(sapply(.llms, inherits, "LLMMessage")),
    ".batch_id must be a non-empty character string or NULL" = is.null(.batch_id) || (is.character(.batch_id) && nzchar(.batch_id)),
    ".dry_run must be logical" = is.logical(.dry_run),
    ".max_tries must be integer-valued numeric" = is_integer_valued(.max_tries),
    ".timeout must be integer-valued numeric" = is_integer_valued(.timeout)
  ) |> validate_inputs()
  
  # Preserve original names
  original_names <- names(.llms)
  
  # Retrieve batch_id from .llms if not provided
  if (is.null(.batch_id)) {
    .batch_id <- attr(.llms, "batch_id")
    if (is.null(.batch_id)) {
      stop("No batch_id provided and no batch_id attribute found in the provided list.")
    }
  }
  
  .json <- attr(.llms, "json")
  if (is.null(.json)) {.json <- FALSE}
  
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "" && !.dry_run) {
    stop("API key is not set. Please set it with: Sys.setenv(OPENAI_API_KEY = \"YOUR-KEY-GOES-HERE\").")
  }
  
  # Construct request URL to get batch details
  batch_details_url <- paste0("https://api.openai.com/v1/batches/", .batch_id)
  
  request <- httr2::request(batch_details_url) |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    )
  
  # If .dry_run is TRUE, return the request object for inspection
  if (.dry_run) {
    return(request)
  }
  
  response <- request |>
    httr2::req_timeout(.timeout) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_retry(
      max_tries = .max_tries,
      retry_on_failure = TRUE,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 503)
    ) |>
    httr2::req_perform()
  
  response_body <- httr2::resp_body_json(response)
  if ("error" %in% names(response_body)) {
    sprintf("OpenAI API returned an Error:\nType: %s\nMessage: %s",
            response_body$error$type,
            response_body$error$message) |>
      stop()  
  }
  
  # Check if batch has completed processing
  if (response_body$status != "completed") {
    stop("Batch processing has not completed yet. Please check again later.")
  }
  
  # Retrieve the output_file_id
  output_file_id <- response_body$output_file_id
  if (is.null(output_file_id)) {
    stop("No output_file_id found in the batch details.")
  }
  
  # Download the output file
  results_url <- paste0("https://api.openai.com/v1/files/", output_file_id, "/content")
  results_request <- httr2::request(results_url) |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key)
    )
  
  results_response <- results_request |>
    httr2::req_timeout(.timeout) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_retry(
      max_tries = .max_tries,
      retry_on_failure = TRUE,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 503)
    ) |>
    httr2::req_perform()
  
  # Parse JSONL response and map results by custom_id
  results_lines <- strsplit(httr2::resp_body_string(results_response), "\n")[[1]]
  results_list <- lapply(results_lines, function(line) {
    if (nzchar(line)) jsonlite::fromJSON(line) else NULL
  })
  results_list <- Filter(Negate(is.null), results_list)
  
  results_by_custom_id <- purrr::set_names(results_list, sapply(results_list, function(x) x$custom_id))
  
  # Map results back to the original .llms list using names as custom IDs
  updated_llms <- lapply(names(.llms), function(custom_id) {
    result <- results_by_custom_id[[custom_id]]
    
    if (!is.null(result) && is.null(result$error) && result$response$status_code == 200) {
      assistant_reply <- result$response$body$choices$message$content
      llm_copy <- .llms[[custom_id]]$clone_deep()
      llm_copy$add_message("assistant", assistant_reply,json=.json)
      return(llm_copy)
    } else {
      warning(sprintf("Result for custom_id %s was unsuccessful or not found", custom_id))
      return(.llms[[custom_id]])
    }
  })
  
  # Restore original names
  names(updated_llms) <- original_names
  
  # Remove batch_id attribute before returning to avoid reuse conflicts
  attr(updated_llms, "batch_id") <- NULL
  attr(updated_llms, "json") <- NULL
  
  
  return(updated_llms)
}




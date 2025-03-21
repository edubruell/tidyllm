#' The Mistral API provider class
#'
#'Inherit most of the functionality from vanilla OpenAI API
#'
#' @noRd
api_mistral <- new_class("Mistral", api_openai)


#' Extract rate limit info from  Mistral API-Headers
#'
#' @noRd
method(ratelimit_from_header, list(api_mistral,new_S3_class("httr2_headers"))) <- function(.api,.headers){
  request_time <- strptime(.headers[["date"]], format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  
  # Extract tokens remaining and reset time
  ratelimit_tokens_remaining <- as.integer(.headers[["x-ratelimitbysize-remaining-minute"]])
  ratelimit_tokens_limit <- as.integer(.headers[["x-ratelimitbysize-limit-minute"]])
  ratelimit_tokens_reset_dt <- as.integer(.headers[["ratelimitbysize-reset"]])
  ratelimit_tokens_reset_time <- request_time + ratelimit_tokens_reset_dt
  
  # For requests per second limit
  ratelimit_requests_remaining <- 0
  ratelimit_requests_reset_time <- request_time + 1  # Mistral allows 1 request per second
  
  list(
    this_request_time = request_time,
    ratelimit_requests_remaining = ratelimit_requests_remaining,
    ratelimit_requests_reset_time = ratelimit_requests_reset_time,
    ratelimit_tokens_remaining = ratelimit_tokens_remaining,
    ratelimit_tokens_reset_time = ratelimit_tokens_reset_time
  )
}

#' Prepare Mistral API request parameters
#'
#' @description
#' Helper function to prepare common request parameters for Mistral API calls,
#' centralizing duplicated logic between chat and batch operations
#'
#' @param .llm An LLMMessage object containing conversation history
#' @param .model The model identifier to use
#' @param .max_tokens Maximum tokens for completion
#' @param .frequency_penalty Frequency penalty parameter
#' @param .logit_bias Logit bias parameters
#' @param .presence_penalty Presence penalty parameter
#' @param .seed Random seed for reproducibility
#' @param .stop Sequence to stop generation
#' @param .temperature Temperature parameter
#' @param .top_p Top p parameter
#' @param .json_schema JSON schema for structured output
#'
#' @return A list containing prepared request parameters
#' @noRd
prepare_mistral_request <- function(
    .llm,
    .api,
    .model = "mistral-large-latest",
    .min_tokens = NULL,
    .max_tokens = NULL,
    .frequency_penalty = NULL,
    .logit_bias = NULL,
    .presence_penalty = NULL,
    .seed = NULL,
    .stop = NULL,
    .safe_prompt =NULL,
    .temperature = NULL,
    .top_p = NULL,
    .json_schema = NULL
) {
  # Format messages using the API format
  messages <- to_api_format(.llm, .api, TRUE)
  
  # Handle JSON schema and response format
  response_format <- NULL
  json <- FALSE
  if (!is.null(.json_schema)) {
    json <- TRUE
    schema_name <- "empty"
    if (requireNamespace("ellmer", quietly = TRUE)) {
      # Handle ellmer json schema objects
      if (S7_inherits(.json_schema, ellmer::TypeObject)) {
        .json_schema <- to_schema(.json_schema)
        schema_name <- "ellmer_schema"
      }
    }
    if (schema_name != "ellmer_schema") {
      schema_name <- attr(.json_schema, "name")
    }
    
  
    # Ensure additionalProperties is set to FALSE if not already provided
    if (is.null(.json_schema$additionalProperties)) {
      .json_schema$additionalProperties <- FALSE
    }
    
    response_format <- list(
      type = "json_schema",
      json_schema = list(
        schema = .json_schema,
        name = schema_name,
        strict = TRUE  # Mistral API expects this field
      )
    )
  }
  
  # Build common request body parameters
  request_body <- list(
    model = .model,
    messages = messages,
    frequency_penalty = .frequency_penalty,
    logit_bias = .logit_bias,
    max_tokens = .max_tokens,  # Note: This might also need to be max_tokens not max_completion_tokens
    presence_penalty = .presence_penalty,
    response_format = response_format,
    random_seed = .seed,  # Note: Mistral uses random_seed not seed
    safe_prompt = .safe_prompt,
    stop = .stop,
    temperature = .temperature,
    top_p = .top_p
  ) |> purrr::compact()
  
  # Return all prepared elements
  list(
    messages = messages,
    request_body = request_body,
    json = json,
    no_system_prompt = TRUE
  )
}

#' Send LLMMessage to Mistral API
#'
#' @param .llm An `LLMMessage` object.
#' @param .model The model identifier to use (default: `"mistral-large-latest"`). 
#' @param .frequency_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency.
#' @param .logit_bias A named list modifying the likelihood of specified tokens appearing in the completion.
#' @param .presence_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far.
#' @param .seed If specified, the system will make a best effort to sample deterministically.
#' @param .stop Up to 4 sequences where the API will stop generating further tokens.
#' @param .stream If set to TRUE, the answer will be streamed to console as it comes (default: FALSE).
#' @param .temperature What sampling temperature to use, between 0 and 2. Higher values make the output more random.
#' @param .top_p An alternative to sampling with temperature, called nucleus sampling.
#' @param .min_tokens The minimum number of tokens to generate in the completion. Must be `>= 0` (optional).
#' @param .max_tokens An upper bound for the number of tokens that can be generated for a completion.
#' @param .tools Either a single TOOL object or a list of TOOL objects representing the available functions for tool calls.
#' @param .tool_choice A character string specifying the tool-calling behavior; valid values are "none", "auto", or "required".
#' @param .json_schema A JSON schema object provided by tidyllm schema or ellmer schemata.
#' @param .safe_prompt Whether to inject a safety prompt before all conversations (default: `FALSE`).
#' @param .timeout When should our connection time out in seconds (default: `120`).
#' @param .verbose Should additional information be shown after the API call? (default: `FALSE`)
#' @param .dry_run If `TRUE`, perform a dry run and return the request object (default: `FALSE`).
#' @param .max_tries Maximum retries to peform request
#' @return Returns an updated `LLMMessage` object.
#' @export
mistral_chat <- function(.llm,
                         .model = "mistral-large-latest",
                         .frequency_penalty = NULL,
                         .logit_bias = NULL,
                         .presence_penalty = NULL,
                         .seed = NULL,
                         .stop = NULL,
                         .stream = FALSE,
                         .temperature = 0.7,
                         .top_p = 1,
                         .min_tokens = NULL,
                         .max_tokens = NULL,
                         .json_schema = NULL,
                         .safe_prompt = FALSE,
                         .timeout = 120,
                         .max_tries = 3,
                         .dry_run = FALSE,
                         .verbose = FALSE,
                         .tools = NULL,
                         .tool_choice = NULL) {
  
  # Validate the inputs
  c(
    "Input .llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
    "Input .model must be a string or NULL" = is.null(.model) || (is.character(.model) && length(.model) == 1),
    "Input .stream must be logical" = is.logical(.stream) && length(.stream) == 1,
    "Input .temperature must be numeric between 0 and 1.5" = is.null(.temperature) || (is.numeric(.temperature) && .temperature >= 0 && .temperature <= 1.5),
    "Input .top_p must be numeric between 0 and 1" = is.null(.top_p) || (is.numeric(.top_p) && .top_p >= 0 && .top_p <= 1),
    "Input .max_tokens must be NULL or a positive integer" = is.null(.max_tokens) || (is_integer_valued(.max_tokens) && .max_tokens > 0),
    "Input .min_tokens must be integer >= 0 if provided" = is.null(.min_tokens) || (is_integer_valued(.min_tokens) && .min_tokens >= 0),
    "Input .seed must be integer >= 0 if provided" = is.null(.seed) || (is_integer_valued(.seed) && .seed >= 0),
    "Input .stop must be a string or vector of strings if provided" = is.null(.stop) || (is.character(.stop)),
    "Input .frequency_penalty must be numeric or NULL" = is.null(.frequency_penalty) || is.numeric(.frequency_penalty),
    "Input .logit_bias must be a list or NULL" = is.null(.logit_bias) || is.list(.logit_bias),
    "Input .presence_penalty must be numeric or NULL" = is.null(.presence_penalty) || is.numeric(.presence_penalty),
    "Input .safe_prompt must be logical" = is.logical(.safe_prompt) && length(.safe_prompt) == 1,
    "Input .max_tries must be integer-valued numeric" = is_integer_valued(.max_tries),
    "Input .timeout must be integer-valued numeric (seconds till timeout)" = is_integer_valued(.timeout),
    "Input .json_schema must be NULL or a list or an ellmer type object" = is.null(.json_schema) || is.list(.json_schema) || is_ellmer_type(.json_schema),
    "Input .tools must be NULL, a TOOL object, or a list of TOOL objects" = is.null(.tools) || S7_inherits(.tools, TOOL) || (is.list(.tools) && all(purrr::map_lgl(.tools, ~ S7_inherits(.x, TOOL)))),
    "Input .tool_choice must be NULL or a character (one of 'none', 'auto', 'required')" = is.null(.tool_choice) || (is.character(.tool_choice) && .tool_choice %in% c("none", "auto", "required")),
    "Input .dry_run must be logical" = is.logical(.dry_run) && length(.dry_run) == 1,
    "Input .verbose must be logical" = is.logical(.verbose) && length(.verbose) == 1,
    "Streaming is not supported for requests with tool calls" = is.null(.tools) || !isTRUE(.stream)
  ) |>
    validate_inputs()
  
  # Create API object
  api_obj <- api_mistral(
    short_name = "mistral",
    long_name = "Mistral",
    api_key_env_var = "MISTRAL_API_KEY"
  )
  
  # Get API key
  api_key <- get_api_key(api_obj, .dry_run)
  
  # Use the prepare_openai_request helper to set up common parameters
  request_data <- prepare_mistral_request(
    .llm = .llm,
    .api = api_obj,
    .model = .model,
    .max_tokens = .max_tokens,
    .frequency_penalty = .frequency_penalty,
    .logit_bias = .logit_bias,
    .presence_penalty = .presence_penalty,
    .seed = .seed,
    .stop = .stop,
    .safe_prompt = .safe_prompt,
    .min_tokens = .min_tokens,
    .temperature = .temperature,
    .top_p = .top_p,
    .json_schema = .json_schema
  )
  
  # Get components from the request data
  request_body <- request_data$request_body
  json <- request_data$json
  
  # Add streaming if requested
  if (.stream) {
    request_body$stream <- TRUE
  }
  
  # Handle tools
  tools_def <- if (!is.null(.tools)) {
    if (S7_inherits(.tools, TOOL)) list(.tools) else .tools
  } else {
    NULL
  }
  
  # Add tools to request body if provided
  if (!is.null(tools_def)) {
    request_body$tools <- tools_to_api(api_obj, tools_def)
    request_body$tool_choice <- .tool_choice
  }
  

  # Build the request
  request <- httr2::request("https://api.mistral.ai") |>
    httr2::req_url_path("/v1/chat/completions") |>
    httr2::req_headers(
      `Authorization` = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json",
      .redact = "Authorization"
    ) |>
    httr2::req_body_json(request_body)
  
  # Return only the request object in a dry run
  if (.dry_run) {
    return(request)  
  }
  
  # Perform the request
  response <- perform_chat_request(request, api_obj, .stream, .timeout, .max_tries)
  
  # Handle tool calls if any
  if (!is.null(response$raw$content$choices[[1]]$message$tool_calls)) {
    tool_messages <- run_tool_calls(api_obj,
                                    response$raw$content$choices[[1]]$message$tool_calls,
                                    tools_def)
    
    # Append the tool call to API
    request_body$messages <- request_body$messages |> append(tool_messages)
    
    # Update the request and perform it again
    request <- request |> httr2::req_body_json(data = request_body)
    response <- perform_chat_request(request, api_obj, .stream, .timeout, .max_tries)
  }
  
  # Extract assistant reply
  assistant_reply <- response$assistant_reply
  
  # Track rate limit
  track_rate_limit(api_obj, response$headers, .verbose)
  
  # Add model's message to the history of the LLMMessage object
  add_message(
    .llm = .llm,
    .role = "assistant", 
    .content = assistant_reply,
    .json = json,
    .meta = response$meta,
    .logprobs = NULL
  )
}


#' Generate Embeddings Using Mistral API
#'
#' @param .input A character vector of texts to embed or an `LLMMessage` object
#' @param .model The embedding model identifier (default: "mistral-embed").
#' @param .timeout Timeout for the API request in seconds (default: 120).
#' @param .dry_run If TRUE, perform a dry run and return the request object.
#' @param .max_tries Maximum retries to peform request
#' @return A matrix where each column corresponds to the embedding of a message in the message history.
#' @export
mistral_embedding <- function(.input,
                              .model = "mistral-embed",
                              .timeout = 120,
                              .max_tries = 3,
                              .dry_run = FALSE) {

  # Retrieve API key from environment variables
  api_key <- Sys.getenv("MISTRAL_API_KEY")
  if ((api_key == "")& .dry_run==FALSE){
    stop("API key is not set. Please set it with: Sys.setenv(MISTRAL_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  }
  # Validate the inputs
  c(
    "Input .llm must character vector or an LLMMessage object" = S7_inherits(.input, LLMMessage) | is.character(.input),
    "Input .model must be a string" = is.character(.model),
    "Input .timeout must be an integer-valued numeric (seconds till timeout)" = is.numeric(.timeout) && .timeout > 0,
    ".dry_run must be logical" = is.logical(.dry_run)
  ) |> validate_inputs()
  
  input_texts <- parse_embedding_input(.input)
  # Prepare the request body
  request_body <- list(
    model = .model,
    input = input_texts
  )
  
  # Build the request
  request <- httr2::request("https://api.mistral.ai/v1/embeddings") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", api_key),
      .redact = "Authorization"
    ) |>
    httr2::req_body_json(request_body)
  
  # Dry run
  if (.dry_run) {
    return(request)
  }
  
  extract_embeddings_fn <- function(response_content,error,headers){
    if(error){
      paste0("API error response (Code ", response_content$code ,") ", response_content$message)|>
        stop()
    }
    response_content$data |> 
      purrr::map("embedding")  |>
      purrr::map(unlist)
  }
  
  # Perform a standard embedding API request
  perform_embedding_request(.request = request,
                            .timeout = .timeout,
                            .max_tries = .max_tries,
                            .input_texts = input_texts, 
                            .fn_extract_embeddings = extract_embeddings_fn)
}

#' Send a Batch of Requests to the Mistral API
#'
#' This function creates and submits a batch of messages to the Mistral API for asynchronous processing.
#'
#' @param .llms A list of LLMMessage objects containing conversation histories.
#' @param .model The Mistral model version (default: "mistral-small-latest").
#' @param .endpoint The API endpoint (default: "/v1/chat/completions").
#' @param .metadata Optional metadata for the batch.
#' @param .temperature Sampling temperature to use, between `0.0` and `1.5` (default: `0.7`).
#' @param .top_p Nucleus sampling parameter, between `0.0` and `1.0` (default: `1`).
#' @param .max_tokens The maximum number of tokens to generate in the completion (default: `1024`).
#' @param .min_tokens The minimum number of tokens to generate (optional).
#' @param .frequency_penalty Numeric value (or NULL) for frequency penalty.
#' @param .logit_bias A named list modifying the likelihood of specific tokens (or NULL).
#' @param .presence_penalty Numeric value (or NULL) for presence penalty.
#' @param .seed Random seed for deterministic outputs (optional).
#' @param .stop Sequence(s) at which to stop generation (optional).
#' @param .safe_prompt Logical; if TRUE, injects a safety prompt (default: FALSE).
#' @param .json_schema A JSON schema object for structured output (optional).
#' @param .dry_run Logical; if TRUE, returns the prepared request without executing it (default: FALSE).
#' @param .overwrite Logical; if TRUE, allows overwriting existing custom IDs (default: FALSE).
#' @param .max_tries Maximum retry attempts for requests (default: 3).
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .id_prefix Prefix for generating custom IDs (default: "tidyllm_mistral_req_").
#'
#' @return The prepared LLMMessage list with a batch_id attribute.
#' @export
send_mistral_batch <- function(.llms,
                               .model = "mistral-small-latest",
                               .endpoint = "/v1/chat/completions",
                               .metadata = NULL,
                               .temperature = 0.7,
                               .top_p = 1,
                               .max_tokens = 1024,
                               .min_tokens = NULL,
                               .frequency_penalty = NULL,
                               .logit_bias = NULL,
                               .presence_penalty = NULL,
                               .seed = NULL,
                               .stop = NULL,
                               .safe_prompt = FALSE,
                               .json_schema = NULL,
                               .dry_run = FALSE,
                               .overwrite = FALSE,
                               .max_tries = 3,
                               .timeout = 60,
                               .id_prefix = "tidyllm_mistral_req_") {
  
  # Input validation
  c(
    ".llms must be a list of LLMMessage objects" = is.list(.llms) && all(sapply(.llms, S7_inherits, LLMMessage)),
    ".model must be a string" = is.character(.model),
    ".endpoint must be a string" = is.character(.endpoint),
    ".metadata must be NULL or a list" = is.null(.metadata) || is.list(.metadata),
    ".temperature must be numeric between 0.0 and 1.5" = is.numeric(.temperature) && .temperature >= 0 && .temperature <= 1.5,
    ".top_p must be numeric between 0.0 and 1.0" = is.numeric(.top_p) && .top_p >= 0 && .top_p <= 1.0,
    ".max_tokens must be integer >= 0" = is_integer_valued(.max_tokens) && .max_tokens >= 0,
    ".min_tokens must be integer >= 0 if provided" = is.null(.min_tokens) || (is_integer_valued(.min_tokens) && .min_tokens >= 0),
    ".frequency_penalty must be numeric or NULL" = is.null(.frequency_penalty) || is.numeric(.frequency_penalty),
    ".logit_bias must be a list or NULL" = is.null(.logit_bias) || is.list(.logit_bias),
    ".presence_penalty must be numeric or NULL" = is.null(.presence_penalty) || is.numeric(.presence_penalty),
    ".seed must be integer >= 0 if provided" = is.null(.seed) || (is_integer_valued(.seed) && .seed >= 0),
    ".stop must be NULL or character" = is.null(.stop) || is.character(.stop),
    ".safe_prompt must be logical" = is.logical(.safe_prompt) && length(.safe_prompt) == 1,
    ".json_schema must be NULL or a list or an ellmer type object" = is.null(.json_schema) || is.list(.json_schema) || is_ellmer_type(.json_schema),
    ".dry_run must be logical" = is.logical(.dry_run),
    ".overwrite must be logical" = is.logical(.overwrite),
    ".id_prefix must be a string" = is.character(.id_prefix)
  ) |> validate_inputs()
  
  # Initialize API object and get API key
  api_obj <- api_mistral(short_name = "mistral",
                         long_name = "Mistral",
                         api_key_env_var = "MISTRAL_API_KEY")
  api_key <- get_api_key(api_obj, .dry_run)
  
  # Prepare LLMs for the batch
  prepared_llms <- prepare_llms_for_batch(api_obj, 
                                          .llms = .llms, 
                                          .id_prefix = .id_prefix, 
                                          .overwrite = .overwrite)
  
  # Generate request lines for the batch using the common prepare function
  request_lines <- lapply(seq_along(prepared_llms), function(i) {
    custom_id <- names(prepared_llms)[i]
    
    # Prepare common request parameters using prepare_mistral_request
    request_data <- prepare_mistral_request(
      .llm = prepared_llms[[i]],
      .api = api_obj,
      .model = .model,
      .min_tokens = .min_tokens,
      .max_tokens = .max_tokens,
      .frequency_penalty = .frequency_penalty,
      .logit_bias = .logit_bias,
      .presence_penalty = .presence_penalty,
      .seed = .seed,
      .stop = .stop,
      .safe_prompt = .safe_prompt,
      .temperature = .temperature,
      .top_p = .top_p,
      .json_schema = .json_schema
    )
    
    list(
      custom_id = custom_id,
      method = "POST",
      url = .endpoint,
      body = request_data$request_body
    ) |> jsonlite::toJSON(auto_unbox = TRUE)
  })
  
  # Save the batch requests to a temporary .jsonl file
  temp_file <- tempfile(fileext = ".jsonl")
  writeLines(unlist(request_lines), con = temp_file)
  
  if (.dry_run) {
    return(readLines(temp_file))
  }
  
  # Upload the batch file
  upload_request <- httr2::request("https://api.mistral.ai/v1/files") |>
    httr2::req_headers(Authorization = sprintf("Bearer %s", api_key)) |>
    httr2::req_body_multipart(purpose = "batch", file = curl::form_file(temp_file))
  
  upload_response <- upload_request |>
    perform_generic_request(.timeout = .timeout, .max_tries = .max_tries)
  
  input_file_id <- upload_response$content$id
  
  # Create the batch job
  batch_request_body <- list(
    input_files = list(input_file_id),
    model = .model,
    endpoint = .endpoint,
    metadata = .metadata
  )
  
  batch_request <- httr2::request("https://api.mistral.ai/v1/batch/jobs") |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(batch_request_body)
  
  batch_response <- batch_request |>
    perform_generic_request(.timeout = .timeout, .max_tries = .max_tries)
  
  if(batch_response$status == 403) {
    rlang::abort(sprintf("Mistral Batch API error: %s", batch_response$content$detail))
  }
  
  # Attach batch_id as an attribute to the prepared LLMs
  batch_id <- batch_response$content$id
  attr(prepared_llms, "batch_id") <- batch_id
  
  # Optionally remove the temporary file
  unlink(temp_file)
  
  return(prepared_llms)
}


#' Check Batch Processing Status for Mistral Batch API
#'
#' This function retrieves the processing status and other details of a specified Mistral batch ID
#' from the Mistral Batch API.
#'
#' @param .llms A list of LLMMessage objects.
#' @param .batch_id A manually set batch ID.
#' @param .dry_run Logical; if TRUE, returns the prepared request object without executing it (default: FALSE).
#' @param .max_tries Maximum retries to perform the request (default: 3).
#' @param .timeout Integer specifying the request timeout in seconds (default: 60).
#' @return A tibble with information about the status of batch processing.
#' @export
check_mistral_batch <- function(.llms = NULL,
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
  api_key <- Sys.getenv("MISTRAL_API_KEY")
  if ((api_key == "") & !.dry_run){
    stop("API key is not set.")
  }
  
  # Build request
  request_url <- paste0("https://api.mistral.ai/v1/batch/jobs/", .batch_id)
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
    perform_generic_request(.timeout=.timeout,
                            .max_tries = .max_tries)
  
  # Parse response
  response_body <- response$content
  if("error" %in% names(response_body)){
    sprintf("Mistral API returned an Error:\nType: %s\nMessage: %s",
            response_body$error$type,
            response_body$error$message) |>
      stop()
  }
  
  # Extract relevant fields and handle timestamps
  tibble::tibble(
    batch_id = response_body$id,
    status = response_body$status,
    model = response_body$model,
    endpoint = response_body$endpoint,
    created_at = lubridate::as_datetime(response_body$created_at),
    started_at = if (!is.null(response_body$started_at)) lubridate::as_datetime(response_body$started_at) else NA,
    completed_at = if (!is.null(response_body$completed_at)) lubridate::as_datetime(response_body$completed_at) else NA,
    total_requests = response_body$total_requests,
    completed_requests = response_body$completed_requests,
    succeeded_requests = response_body$succeeded_requests,
    failed_requests = response_body$failed_requests,
    output_file = response_body$output_file,
    error_file = response_body$error_file
  )
}



#' List Mistral Batch Requests
#'
#' Retrieves batch request details from the OpenAI Batch API.
#'
#' @param .limit Maximum number of batches to retrieve (default: 20).
#' @param .max_tries Maximum retry attempts for requests (default: 3).
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .status Filter by status. (default: NULL)
#' @param .created_after created after a string specifiying a date-time  (default: NULL)
#'
#' @return A tibble with batch details for all batches fitting the request
#'
#' @export
list_mistral_batches <- function(.limit = 100,
                                 .max_tries = 3,
                                 .timeout = 60,
                                 .status = NULL,
                                 .created_after = NULL
                                 ) {
  # Retrieve API key
  api_key <- Sys.getenv("MISTRAL_API_KEY")
  if (api_key == "") {
    stop("API key is not set. Please set it with: Sys.setenv(MISTRAL_API_KEY = \"YOUR-KEY-GOES-HERE\").")
  }
  
  query_params = list(
    page_size = .limit,
    status    = .status,
    created_after = .created_after
  ) |>
    purrr::compact()
  
  # Set up request URL with query parameters
  request <- httr2::request("https://api.mistral.ai/v1/batch/jobs") |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_url_query(!!!query_params)
  
  
  # Perform the request with retries and error handling
  response <- request |>
    perform_generic_request(.timeout=.timeout,
                            .max_tries = .max_tries)
  
  # Parse response
  response_body <- response$content
  if ("error" %in% names(response_body)) {
    sprintf("Mistral API returned an Error:\nType: %s\nMessage: %s",
            response_body$error$type,
            response_body$error$message) |>
      stop()  
  }
  
  # Extract batch data and format as tibble
  batch_data <- response_body$data
  batch_tibble <- purrr::map_dfr(batch_data, ~tibble::tibble(
    batch_id = .x$id,
    status   = .x$status,
    created_at = as.POSIXct(.x$created_at, origin = "1970-01-01", tz = "UTC"),
    started_at = if (!is.null(.x$started_at)) as.POSIXct(.x$started_at, origin = "1970-01-01", tz = "UTC") else NA,
    completed_at = if (!is.null(.x$completed_at)) as.POSIXct(.x$completed_at, origin = "1970-01-01", tz = "UTC") else NA,
    total_requests = .x$total_requests,
    completed_requests = .x$completed_requests,
    succeeded_requests = .x$succeeded_requests,
    failed_requests = .x$failed_requests,
    input_files = .x$input_files, 
    model = .x$model,
    endpoint = .x$endpoint,
    output_file = .x$output_file,
    error_file = .x$error_file
  ))
  
  return(batch_tibble)
}


#' Fetch Results for an Mistral Batch
#'
#' This function retrieves the results of a completed Mistral batch and updates
#' the provided list of `LLMMessage` objects with the responses. It aligns each
#' response with the original request using the `custom_id`s generated in `send_mistral_batch()`.
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
fetch_mistral_batch <- function(.llms,
                               .batch_id = NULL,
                               .dry_run = FALSE,
                               .max_tries = 3,
                               .timeout = 60) {

  c(
    ".llms must be a list of LLMMessage objects with names as custom IDs" = is.list(.llms) && all(sapply(.llms, S7_inherits, LLMMessage)),
    ".batch_id must be a non-empty character string or NULL" = is.null(.batch_id) || (is.character(.batch_id) && nzchar(.batch_id)),
    ".dry_run must be logical" = is.logical(.dry_run),
    ".max_tries must be integer-valued numeric" = is_integer_valued(.max_tries),
    ".timeout must be integer-valued numeric" = is_integer_valued(.timeout)
  ) |> validate_inputs()
  
  .json <- attr(.llms, "json")
  if (is.null(.json)) {.json <- FALSE}
  
  # Preserve original names
  original_names <- names(.llms)
  
  # Retrieve batch_id from .llms if not provided
  if (is.null(.batch_id)) {
    .batch_id <- attr(.llms, "batch_id")
    if (is.null(.batch_id)) {
      stop("No batch_id provided and no batch_id attribute found in the provided list.")
    }
  }
  
  #Get batch infos
  batch_info <- check_mistral_batch(.batch_id = .batch_id)
  # Check if batch has completed processing
  if (batch_info$status != "SUCCESS") {
    stop("Batch processing has not completed yet. Please check again later.")
  }
  

  api_obj <- api_mistral(short_name = "mistral",
                         long_name  = "Mistral",
                         api_key_env_var = "MISTRAL_API_KEY")
  
  api_key <- get_api_key(api_obj,.dry_run)

  # Download the output file
  results_url <- paste0("https://api.mistral.ai/v1/files/",  batch_info$output_file, "/content")
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
      meta_data <- extract_metadata(api_obj,result$response$body)
      llm <- add_message(.llm = .llms[[custom_id]],
                         .role = "assistant", 
                         .content =  assistant_reply,
                         .json = .json,
                         .meta = meta_data)
      return(llm)
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


#' List Available Models from the Mistral API
#'
#' @param .api_url Base URL for the API (default: "https://api.mistral.ai").
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .max_tries Maximum number of retries for the API request (default: 3).
#' @param .dry_run Logical; if TRUE, returns the prepared request object without executing it.
#' @param .verbose Logical; if TRUE, prints additional information about the request.
#'
#' @return A tibble containing model information (columns include `id` and `created`),
#'   or NULL if no models are found.
#'
#' @export
mistral_list_models <- function(.api_url = "https://api.mistral.ai",
                                .timeout = 60,
                                .max_tries = 3,
                                .dry_run = FALSE,
                                .verbose = FALSE) {
  # Create an API object for Mistral using the tidyllm helper
  api_obj <- api_mistral(short_name = "mistral",
                         long_name  = "Mistral",
                         api_key_env_var = "MISTRAL_API_KEY")
  
  # Retrieve the API key (will error if not set, unless in dry run mode)
  api_key <- get_api_key(api_obj, .dry_run)
  
  # Build the request to the /v1/models endpoint
  request <- httr2::request(.api_url) |>
    httr2::req_url_path("/v1/models") |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    )
  
  # If dry run is requested, return the constructed request object
  if (.dry_run) {
    return(request)
  }
  
  # Perform the request with specified timeout and retry logic
  response <- request |>
    httr2::req_timeout(.timeout) |>
    httr2::req_retry(max_tries = .max_tries) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  
  if (.verbose) {
    message("Retrieved response from Mistral: ", response$object)
  }
  
  # Check if the "data" field exists and contains models
  if (!is.null(response$data)) {
    models <- response$data
    
    # Create a tibble with selected model information
    model_info <- tibble::tibble(
      id = vapply(models, function(model) model$id, character(1)),
      created = vapply(models, function(model) {
        as.character(strptime(
          format(as.POSIXct(model$created, origin = "1970-01-01", tz = "GMT"),
                 format = "%a, %d %b %Y %H:%M:%S"),
          format = "%a, %d %b %Y %H:%M:%S", tz = "GMT"))
      }, character(1))
    )
    
    return(model_info)
  } else {
    return(NULL)
  }
}


#' Mistral Provider Function
#'
#' The `mistral()` function acts as an interface for interacting with the Mistral API 
#' through main `tidyllm` verbs such as `chat()` and `embed()`. 
#' It dynamically routes requests to Mistral-specific functions 
#' like `mistral_chat()` and `mistral_embedding()` based on the context of the call.
#'
#' @param ... Parameters to be passed to the appropriate Mistral-specific function, 
#'   such as model configuration, input text, or API-specific options.
#' @param .called_from An internal argument that specifies which action (e.g., 
#'   `chat`, `embed`, `send_batch`) the function is being invoked from. 
#'   This argument is automatically managed and should not be modified by the user.
#'
#' @return The result of the requested action, depending on the specific function invoked 
#'   (e.g., an updated `LLMMessage` object for `chat()`, or a matrix for `embed()`).
#' 
#' @export
mistral <- create_provider_function(
  .name = "mistral",
  chat  = mistral_chat,
  embed = mistral_embedding,
  send_batch = send_mistral_batch,
  check_batch = check_mistral_batch,
  list_batches = list_mistral_batches,
  fetch_batch = fetch_mistral_batch,
  list_models = mistral_list_models
)
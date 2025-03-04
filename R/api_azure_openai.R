#' The Azure OpenAI API provider class
#'
#'Inherit most of the functionality from vanilla OpenAI API
#'
#' @noRd
api_azure_openai <- new_class("Azure OpenAI", api_openai)


#' Extract rate limit info from  Azure Openai API-Headers
#'
#' @noRd
method(ratelimit_from_header, list(api_azure_openai,new_S3_class("httr2_headers"))) <- function(api,headers){
  request_time <- strptime(headers["date"]$date, 
                           format="%a, %d %b %Y %H:%M:%S", tz="GMT")
  
  # Extract remaining requests and tokens
  ratelimit_requests_remaining <- as.integer(
    headers["x-ratelimit-remaining-requests"]$`x-ratelimit-remaining-requests`)
  ratelimit_tokens_remaining <- as.integer(
    headers["x-ratelimit-remaining-tokens"]$`x-ratelimit-remaining-tokens`)
  
  # Assuming reset occurs every 60 seconds (at least I got minutes in my azure console)
  reset_interval <- 60         
  
  ratelimit_requests_reset_time <- request_time + reset_interval
  ratelimit_tokens_reset_time <- request_time + reset_interval
  
  list(
    this_request_time = request_time,
    ratelimit_requests = NA,
    ratelimit_requests_remaining = ratelimit_requests_remaining,
    ratelimit_requests_reset_time = ratelimit_requests_reset_time,
    ratelimit_tokens = NA,
    ratelimit_tokens_remaining = ratelimit_tokens_remaining,
    ratelimit_tokens_reset_time = ratelimit_tokens_reset_time
  )
}


#' A chat parsing method for Azure Openai to extract the assitant response
#'
#' @noRd
method(parse_chat_function, api_azure_openai) <- function(api) {
  api_label <- api@long_name 
  function(body_json){
    if("error" %in% names(body_json)){
      sprintf("%s returned an Error:\nType: %s\nMessage: %s",
              api_label,
              body_json$error$code,
              body_json$error$message) |>
        stop()
    }
    
    if (length(body_json$choices) == 0) {
      paste0("Received empty response from ",api_label) |>
        stop()
    }
    body_json$choices[[1]]$message$content  
  }
}  


#' Send LLM Messages to an OpenAI Chat Completions endpoint on Azure 
#'
#' @description
#' This function sends a message history to the Azure OpenAI Chat Completions API and returns the assistant's reply. 
#' This function is work in progress and not fully tested
#'
#' @param .llm An `LLMMessage` object containing the conversation history.
#' @param .endpoint_url Base URL for the API (default:  Sys.getenv("AZURE_ENDPOINT_URL")).
#' @param .deployment The identifier of the model that is deployed (default: "gpt-4o-mini").
#' @param .api_version Which version of the API is deployed (default: "2024-10-01-preview")
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
#' @param .max_tries Maximum retries to perform request
#' @param .json_schema A JSON schema object as R list to enforce the output structure (If defined has precedence over JSON mode).
#' @param .dry_run If TRUE, perform a dry run and return the request object (default: FALSE).
#' @param .tools Either a single TOOL object or a list of TOOL objects representing the available functions for tool calls.
#' @param .tool_choice A character string specifying the tool-calling behavior; valid values are "none", "auto", or "required".
#'
#'
#' @return A new `LLMMessage` object containing the original messages plus the assistant's response.
#' @examples
#' \dontrun{
#' # Basic usage
#' msg <- llm_message("What is R programming?")
#' result <- azure_openai_chat(msg)
#' 
#' # With custom parameters
#' result2 <- azure_openai_chat(msg, 
#'                  .deployment = "gpt-4o-mini",
#'                  .temperature = 0.7, 
#'                  .max_tokens = 1000)
#' }
#'
#' @export
azure_openai_chat <- function(
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
    .json_schema = NULL,
    .dry_run = FALSE,
    .max_tries = 3,
    .tools = NULL,
    .tool_choice = NULL
) {
    #Check enpoint
  if (.endpoint_url == ""& .dry_run==FALSE){
   stop("No valid Azure endpoint defined. Please set it either as input to this function or with: Sys.setenv(AZURE_ENDPOINT_URL = \"https://endpoint.openai.azure.com/\")")
  }
  
  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
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
    "Input .json_schema must be NULL or a list or an ellmer type object" = is.null(.json_schema) | is.list(.json_schema) | is_ellmer_type(.json_schema),
    "Input .dry_run must be logical" = is.logical(.dry_run),
    "Input .top_logprobs must be NULL or an integer between 0 and 5" = is.null(.top_logprobs) | (is_integer_valued(.top_logprobs) && .top_logprobs >= 0 && .top_logprobs <= 5),
    "Input .tools must be NULL, a TOOL object, or a list of TOOL objects" = is.null(.tools) || S7_inherits(.tools, TOOL) || (is.list(.tools) && all(purrr::map_lgl(.tools, ~ S7_inherits(.x, TOOL)))),
    "Input .tool_choice must be NULL or a character (one of 'none', 'auto', 'required')" = is.null(.tool_choice) || (is.character(.tool_choice) && .tool_choice %in% c("none", "auto", "required")),
    "Streaming is not supported for requests with tool calls" = is.null(.tools) || !isTRUE(.stream)
  ) |> validate_inputs()
  
  
  api_obj <- api_azure_openai(short_name = "azure_openai",
                              long_name  = "Azure OpenAI",
                              api_key_env_var = "AZURE_OPENAI_API_KEY")
  
  api_key <- get_api_key(api_obj,.dry_run)
  
  #This filters out the system prompt for reasoning models.
  no_system_prompt <- FALSE
  if(.deployment %in% c("o1-preview","o1-mini")){
    message("Note: Reasoning models do not support system prompts")
    no_system_prompt <- TRUE
  }
  
  messages <- to_api_format(llm=.llm,
                            api=api_obj,
                            no_system=no_system_prompt)
  
  #Put a single tool into a list if only one is provided. 
  tools_def <- if (!is.null(.tools)) {
    if (S7_inherits(.tools, TOOL))  list(.tools) else .tools
  } else {
    NULL
  }
  
  # Handle JSON schema
  json <- FALSE
  response_format <- NULL
  if (!is.null(.json_schema)) {
    json=TRUE
    schema_name = "empty"
    if (requireNamespace("ellmer", quietly = TRUE)) {
      #Handle ellmer json schemata Objects
      if(S7_inherits(.json_schema,ellmer::TypeObject)){
        .json_schema = to_schema(.json_schema)
        schema_name = "ellmer_schema"
      } 
    }
    if(schema_name!="ellmer_schema"){schema_name <- attr(.json_schema,"name")}
    response_format <- list(
      type = "json_schema",
      json_schema = list(name = schema_name,
                         schema = .json_schema)
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
    top_p = .top_p,
    logprobs = .logprobs,        
    top_logprobs = .top_logprobs,
    tools = if(!is.null(tools_def)) tools_to_api(api_obj,tools_def) else NULL,
    tool_choice = .tool_choice
  ) |> purrr::compact()

  # Build the request
  request <- httr2::request(.endpoint_url) |>
    httr2::req_url_path_append(paste0("openai/deployments/", .deployment,"/chat/completions")) |>
    httr2::req_url_query(`api-version` = .api_version) |>
    httr2::req_headers(
      `Content-Type` = "application/json",
      `api-key` = api_key,
    )  |>
    httr2::req_body_json(data = request_body)
  
  # Return only the request object in a dry run.
  if (.dry_run) {
    return(request)
  }
  
  response <- perform_chat_request(request,api_obj,.stream,.timeout,.max_tries)
  if(r_has_name(response$raw,"tool_calls")){
    #Tool call logic can go here!
    tool_messages <- run_tool_calls(api_obj,
                                    response$raw$content$choices[[1]]$message$tool_calls,
                                    tools_def)
    ##Append the tool call to API
    request_body$messages <- request_body$messages |> 
      append(tool_messages)
    
    request <- request |>
      httr2::req_body_json(data = request_body)
    
    response <- perform_chat_request(request,api_obj,.stream,.timeout,.max_tries)
  }
  
  
  # Extract assistant reply and rate limiting info from response headers
  assistant_reply <- response$assistant_reply
  
  #Check whether the result has logprobs in it 
  logprobs  <- parse_logprobs(api_obj, response$raw$content$choices[[1]])
  
  track_rate_limit(api_obj,response$headers,.verbose)
  
  add_message(llm     = .llm,
              role    = "assistant", 
              content = assistant_reply , 
              json    = json,
              meta    = response$meta,
              logprobs = logprobs)
}


#' Generate Embeddings Using OpenAI API on Azure
#'
#' @param .input A character vector of texts to embed or an `LLMMesssage`object
#' @param .deployment The embedding model identifier (default: "text-embedding-3-small").
#' @param .endpoint_url Base URL for the API (default:  Sys.getenv("AZURE_ENDPOINT_URL")).
#' @param .truncate Whether to truncate inputs to fit the model's context length (default: TRUE).
#' @param .timeout Timeout for the API request in seconds (default: 120).
#' @param .dry_run If TRUE, perform a dry run and return the request object.
#' @param .max_tries Maximum retry attempts for requests (default: 3).
#' @param .api_version What API-Version othe Azure OpenAI API should be used (default: "2023-05-15")
#' @return A tibble with two columns: `input` and `embeddings`. 
#' The `input` column contains the texts sent to embed, and the `embeddings` column 
#' is a list column where each row contains an embedding vector of the sent input.
#' @export
azure_openai_embedding <- function(.input,
                                   .deployment = "text-embedding-3-small",
                                   .endpoint_url = Sys.getenv("AZURE_ENDPOINT_URL"),
                                   .api_version = "2023-05-15",
                                   .truncate = TRUE,
                                   .timeout = 120,
                                   .dry_run = FALSE,
                                   .max_tries = 3) {

  # Validate the inputs
  c(
    "Input .input must be an LLMMessage object or a character vector" = S7_inherits(.input, LLMMessage) | is.character(.input),
    "Input .deployment must be a string" = is.character(.deployment),
    "Input .truncate must be logical" = is.logical(.truncate),
    "Input .timeout must be an integer-valued numeric (seconds till timeout)" = is.numeric(.timeout) && .timeout > 0,
    ".dry_run must be logical" = is.logical(.dry_run)
  ) |> validate_inputs()
  
  # Get the Azure OpenAI API key
  api_key <- Sys.getenv("AZURE_OPENAI_API_KEY")
  if ((api_key == "")& .dry_run==FALSE){
    stop("API key is not set. Please set it with: Sys.setenv(AZURE_OPENAI_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  }
  
  input_texts <- parse_embedding_input(.input)
  # Prepare the request body
  request_body <- list(
    input = input_texts
  )
  
  # Build the request
  request <- httr2::request(.endpoint_url) |>
    httr2::req_url_path_append(paste0("openai/deployments/", .deployment,"/embeddings")) |>
    httr2::req_url_query(`api-version` = .api_version) |>
    httr2::req_headers(
      `Content-Type` = "application/json",
      `api-key` = api_key,
    )  |>
    httr2::req_body_json(data = request_body)
  
  # Return the request object if it's a dry run
  if (.dry_run) {
    return(request)
  }
  
  extract_embeddings_fn <- function(response_content,error,response_headers){
    if(error){
      paste0("API error response - ", response_content$error$message) |>
        stop()
    }

    response_content$data |> 
      purrr::map("embedding") |>
      purrr::map(unlist)
  }
  
  # Perform a standard embedding API request
  perform_embedding_request(.request = request,
                            .timeout = .timeout,
                            .max_tries = 3,
                            .input_texts = input_texts, 
                            .fn_extract_embeddings = extract_embeddings_fn)
}

#' Send a Batch of Messages to Azure OpenAI Batch API
#'
#' This function creates and submits a batch of messages to the Azure OpenAI Batch API for asynchronous processing.
#'
#' @param .llms An `LLMMessage` object containing the conversation history.
#' @param .endpoint_url Base URL for the API (default:  Sys.getenv("AZURE_ENDPOINT_URL")).
#' @param .deployment The identifier of the model that is deployed (default: "gpt-4o-mini").
#' @param .api_version Which version of the API is deployed (default: "2024-10-01-preview")
#' @param .max_completion_tokens An upper bound for the number of tokens that can be generated for a completion, including visible output tokens and reasoning tokens.
#' @param .frequency_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far.
#' @param .logit_bias A named list modifying the likelihood of specified tokens appearing in the completion.
#' @param .logprobs Whether to return log probabilities of the output tokens (default: FALSE).
#' @param .top_logprobs An integer between 0 and 20 specifying the number of most likely tokens to return at each token position.
#' @param .presence_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far.
#' @param .seed If specified, the system will make a best effort to sample deterministically.
#' @param .stop Up to 4 sequences where the API will stop generating further tokens.
#' @param .temperature What sampling temperature to use, between 0 and 2. Higher values make the output more random.
#' @param .top_p An alternative to sampling with temperature, called nucleus sampling.
#' @param .dry_run If TRUE, perform a dry run and return the request object (default: FALSE).
#' @param .overwrite Logical; if TRUE, allows overwriting an existing batch ID (default: FALSE).
#' @param .max_tries Maximum number of retries to perform the request (default: 3).
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .verbose Logical; if TRUE, additional info about the requests is printed (default: FALSE).
#' @param .json_schema A JSON schema object as R list to enforce the output structure (default: NULL).
#' @param .id_prefix Character string to specify a prefix for generating custom IDs when names in `.llms` are missing (default: "tidyllm_openai_req_").
#'
#' @return An updated and named list of `.llms` with identifiers that align with batch responses, including a `batch_id` attribute.
#' @export
send_azure_openai_batch <- function(.llms,
                              .deployment = 'gpt-4o-mini',
                              .endpoint_url = Sys.getenv("AZURE_ENDPOINT_URL"),
                              .api_version = "2024-10-01-preview",
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
                              .max_tries = 3,
                              .timeout = 60,
                              .verbose = FALSE,
                              .json_schema = NULL,
                              .id_prefix = "tidyllm_azure_openai_req_") {
  #Check enpoint
  if (.endpoint_url == ""& .dry_run==FALSE){
    stop("No valid Azure endpoint defined. Please set it either as input to this function or with: Sys.setenv(AZURE_ENDPOINT_URL = \"https://endpoint.openai.azure.com/\")")
  }
  
  # Input validation
  c(
    ".llms must be a list of LLMMessage objects" = is.list(.llms) && all(sapply(.llms, S7_inherits, LLMMessage)),
    "Input .deployment must be a string" = is.character(.deployment),
    ".max_completion_tokens must be NULL or a positive integer" = is.null(.max_completion_tokens) | (is_integer_valued(.max_completion_tokens) & .max_completion_tokens > 0),
    ".frequency_penalty must be numeric or NULL" = is.null(.frequency_penalty) | is.numeric(.frequency_penalty),
    ".logit_bias must be a list or NULL" = is.null(.logit_bias) | is.list(.logit_bias),
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
  
  api_obj <- api_azure_openai(short_name = "azure_openai",
                              long_name  = "Azure OpenAI",
                              api_key_env_var = "AZURE_OPENAI_API_KEY")
  
  api_key <- get_api_key(api_obj,.dry_run)
  
  #This filters out the system prompt for reasoning models.
  no_system_prompt <- FALSE
  if(.deployment %in% c("o1-preview","o1-mini")){
    message("Note: Reasoning models do not support system prompts")
    no_system_prompt <- TRUE
  }
  
  # Handle JSON schema and JSON mode
  response_format <- NULL
  json <- FALSE
  if (!is.null(.json_schema)) {
    json=TRUE
    schema_name = "empty"
    if (requireNamespace("ellmer", quietly = TRUE)) {
      #Handle ellmer json schemata Objects
      if(S7_inherits(.json_schema,ellmer::TypeObject)){
        .json_schema = to_schema(.json_schema)
        schema_name = "ellmer_schema"
      } 
    }
    if(schema_name!="ellmer_schema"){schema_name <- attr(.json_schema,"name")}
    response_format <- list(
      type = "json_schema",
      json_schema = list(name = schema_name,
                         schema = .json_schema)
    )
  } 
  
  prepared_llms  <- prepare_llms_for_batch(api_obj,
                                           .llms=.llms,
                                           .id_prefix=.id_prefix,
                                           .overwrite = .overwrite)
  
  
  
  # Prepare the request lines
  request_lines <- lapply(seq_along(prepared_llms), function(i) { 
    custom_id <- names(prepared_llms)[i]
    
    # Get messages from each LLMMessage object
    messages <- to_api_format(llm=.llms[[i]],
                              api=api_obj,
                              no_system=no_system_prompt)
    
    # Build the request body
    body <- list(
      model = .deployment, 
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
    ) |> purrr::compact()
    
    
    # Create the request line as JSON
    request_line <- list(
      custom_id = custom_id,
      method = "POST",
      url = "/chat/completions", # ?
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
  upload_request <- httr2::request(.endpoint_url) |> 
    httr2::req_url_path_append("openai/files") |> 
    httr2::req_url_query(`api-version` = .api_version) |>
    httr2::req_headers(
      `Content-Type` = "multipart/form-data",
      `api-key` = api_key,
    ) |>
    httr2::req_body_multipart(
      purpose = "batch",
      file = curl::form_file(temp_file),#httr2::req_file(temp_file)
      type = 'application/json'
    )
  
  upload_response <- upload_request |>
    perform_generic_request(.timeout = .timeout, .max_tries = .max_tries)
  
  if (.verbose) {
    message("Batch request file uploaded via files API")
  }
  
  if ("error" %in% names(upload_response$content)) {
    sprintf("Azure OpenAI API returned an error during batch creation:\nType: %s\nMessage: %s",
            upload_response$content$error$code,
            upload_response$content$error$message) |>
      stop()
  }
  
  input_file_id <- upload_response$content$id
  
  # Now, create the batch
  batch_request_body <- list(
    input_file_id = input_file_id,
    endpoint = "/chat/completions",
    completion_window = "24h"
  )
  
  batch_request <- httr2::request(.endpoint_url) |> 
    httr2::req_url_path("openai/batches") |>
    httr2::req_url_query(`api-version` = .api_version) |>
    httr2::req_headers(
      `Content-Type` = "application/json",
      `api-key` = api_key,
    ) |>
    httr2::req_body_json(batch_request_body)
  
  batch_response <- batch_request |>
    perform_generic_request(.timeout=.timeout,
                            .max_tries = .max_tries)
  
  batch_response_body <- batch_response$content
  
  if(.verbose){message("Batch request for file sent")}
  if ("error" %in% names(batch_response_body)) {
    sprintf("Azure OpenAI API returned an Error during batch creation:\nType: %s\nMessage: %s",
            batch_response_body$error$type,
            batch_response_body$error$message) |>
      stop()
  }
  
  # Attach batch_id as an attribute to .llms
  batch_id <- batch_response_body$id
  attr(prepared_llms, "batch_id") <- batch_id
  attr(prepared_llms, "json") <- json
  
  
  # Optionally, remove the temporary file
  unlink(temp_file)
  
  return(prepared_llms)
}


#' Check Batch Processing Status for Azure OpenAI Batch API
#'
#' This function retrieves the processing status and other details of a specified Azure OpenAI batch ID
#' from the Azure OpenAI Batch API.
#'
#' @param .llms A list of LLMMessage objects.
#' @param .batch_id A manually set batch ID.
#' @param .dry_run Logical; if TRUE, returns the prepared request object without executing it (default: FALSE).
#' @param .max_tries Maximum retries to perform the request (default: 3).
#' @param .endpoint_url Base URL for the API (default:  Sys.getenv("AZURE_ENDPOINT_URL")).
#' @param .timeout Integer specifying the request timeout in seconds (default: 60).
#'
#' @return A tibble with information about the status of batch processing.
#' @export
check_azure_openai_batch <- function(.llms = NULL, 
                                     .endpoint_url = Sys.getenv('AZURE_ENDPOINT_URL'), 
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
  api_key <- Sys.getenv("AZURE_OPENAI_API_KEY")
  if ((api_key == "") & !.dry_run){
    stop("API key is not set.")
  }
  
  # Build request
  request <- httr2::request(.endpoint_url) |> 
    httr2::req_url_path(paste0('openai/batches/', .batch_id)) |> 
    httr2::req_url_query(`api-version` = "2024-10-01-preview") |>
    httr2::req_headers(
      `api-key` = api_key,
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
    sprintf("Azure OpenAI API returned an Error:\nType: %s\nMessage: %s",
            response_body$error$code,
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

#' List Azure OpenAI Batch Requests
#'
#' Retrieves batch request details from the Azure OpenAI Batch API.
#'
#' @param .limit Maximum number of batches to retrieve (default: 20).
#' @param .max_tries Maximum retry attempts for requests (default: 3).
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .endpoint_url Base URL for the API (default:  Sys.getenv("AZURE_ENDPOINT_URL")).
#'
#' @return A tibble with batch details: batch ID, status, creation time, expiration time, 
#' and request counts (total, completed, failed).
#'
#' @export
list_azure_openai_batches <- function(.endpoint_url = Sys.getenv('AZURE_ENDPOINT_URL'), 
                                      .limit = 20,
                                      .max_tries = 3,
                                      .timeout = 60) {
  # Retrieve API key
  api_key <- Sys.getenv("AZURE_OPENAI_API_KEY")
  if (api_key == "") {
    stop("API key is not set. Please set it with: Sys.setenv(AZURE_OPENAI_API_KEY = \"YOUR-KEY-GOES-HERE\").")
  }
  
  # Set up request URL with query parameters
  request <- httr2::request(.endpoint_url) |> 
    httr2::req_url_path('openai/batches') |> 
    httr2::req_url_query(`api-version` = "2024-10-01-preview") |>
    httr2::req_headers(
      `api-key` = api_key
    ) |>
    httr2::req_url_query(limit = .limit)
  
  # Perform the request with retries and error handling
  response <- request |>
    perform_generic_request(.timeout=.timeout,
                            .max_tries = .max_tries)
  
  # Parse response
  response_body <- response$content
  if ("error" %in% names(response_body)) {
    sprintf("OpenAI API returned an Error:\nType: %s\nMessage: %s",
            response_body$error$code,
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

#' Fetch Results for an Azure OpenAI Batch
#'
#' This function retrieves the results of a completed Azure OpenAI batch and updates
#' the provided list of `LLMMessage` objects with the responses. It aligns each
#' response with the original request using the `custom_id`s generated in `send_azure_openai_batch()`.
#'
#' @param .llms A list of `LLMMessage` objects that were part of the batch.
#' @param .batch_id Character; the unique identifier for the batch. By default this is NULL
#'                  and the function will attempt to use the `batch_id` attribute from `.llms`.
#' @param .dry_run Logical; if `TRUE`, returns the constructed request without executing it (default: `FALSE`).
#' @param .max_tries Integer; maximum number of retries if the request fails (default: `3`).
#' @param .timeout Integer; request timeout in seconds (default: `60`).
#' @param .endpoint_url Base URL for the API (default:  Sys.getenv("AZURE_ENDPOINT_URL")).
#'
#' @return A list of updated `LLMMessage` objects, each with the assistant's response added if successful.
#' @export


fetch_azure_openai_batch <- function(.llms,
                               .endpoint_url = Sys.getenv('AZURE_ENDPOINT_URL'),
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
  
  api_obj <- api_azure_openai(short_name = "azure_openai",
                              long_name  = "Azure OpenAI",
                              api_key_env_var = "AZURE_OPENAI_API_KEY")
  
  api_key <- get_api_key(api_obj,.dry_run)
  
  request <- httr2::request(.endpoint_url) |> 
    httr2::req_url_path(paste0('openai/batches/', .batch_id)) |> 
    httr2::req_url_query(`api-version` = "2024-10-01-preview") |>
    httr2::req_headers(
      `api-key` = api_key,
    )
  
  # If .dry_run is TRUE, return the request object for inspection
  if (.dry_run) {
    return(request)
  }
  
  response <- request |>
    perform_generic_request(.timeout=.timeout,
                            .max_tries = .max_tries)
  
  response_body <- response$content
  if ("error" %in% names(response_body)) {
    sprintf("Azure OpenAI API returned an Error:\nType: %s\nMessage: %s",
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
  results_request <- httr2::request(.endpoint_url) |> 
    httr2::req_url_path(paste0('openai/files/', output_file_id, '/content')) |> 
    httr2::req_url_query(`api-version` = "2024-10-01-preview") |>
    httr2::req_headers(
      `api-key` = api_key,
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
      llm <- add_message(llm = .llms[[custom_id]],
                         role = "assistant", 
                         content =  assistant_reply,
                         json = .json,
                         meta = meta_data)
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



#' Azure OpenAI Endpoint Provider Function
#'
#' The `azure_openai()` function acts as an interface for interacting with the Azure OpenAI API 
#' through main `tidyllm` verbs.
#' 
#' `azure_openai()` currently routes messages only to `azure_openai_chat()` when used with `chat()`.
#' 
#' `send_batch()`. It dynamically routes requests to OpenAI-specific functions 
#' like `azure_openai_chat()` and `azure_openai_embedding()` based on the context of the call.
#'
#' @param ... Parameters to be passed to the Azure OpenAI API specific function, 
#'   such as model configuration, input text, or API-specific options.
#' @param .called_from An internal argument that specifies which action (e.g., 
#'   `chat`) the function is being invoked from. 
#'   This argument is automatically managed and should not be modified by the user.
#'
#' @return The result of the requested action, depending on the specific function invoked 
#'   (currently, only an updated `LLMMessage` object for `azure_openai_chat()`).
#'
#' @export
azure_openai <- create_provider_function(
  .name = "azure_openai",
  chat = azure_openai_chat,
  embed = azure_openai_embedding, 
  send_batch = send_azure_openai_batch, 
  check_batch = check_azure_openai_batch,
  list_batches = list_azure_openai_batches,
  fetch_batch = fetch_azure_openai_batch
)





#' The OpenAI API provider class
#'
#' @noRd
api_openai <- new_class("OpenAI", APIProvider)

#' Convert LLMMessage to OpenAI API-Compatible Format
#'
#' Converts the `message_history` of an `LLMMessage` object into the
#' for the OpenAI's Chat Completions API.
#'
#' @noRd
method(to_api_format, list(LLMMessage, api_openai)) <- function(llm, 
                                                                api,
                                                                no_system=FALSE) {
  openai_history <- if (no_system) filter_roles(llm@message_history, c("user", "assistant")) else llm@message_history
  lapply(openai_history, function(m) {
    formatted_message <- format_message(m)
    if (!is.null(formatted_message$image)) {
      list(
        role = m$role,
        content = list(
          list(type = "text", text = formatted_message$content),
          list(type = "image_url", image_url = list(
            url = glue::glue("data:{formatted_message$image$media_type};base64,{formatted_message$image$data}")
          ))
        )
      )
    } else {
      list(role = m$role, content = formatted_message$content)
    }
  })
}


#' Extract rate limit info from  Openai API-Headers
#'
#' @noRd
method(ratelimit_from_header, list(api_openai,new_S3_class("httr2_headers"))) <- function(api,headers){
  request_time <- strptime(headers["date"]$date, format="%a, %d %b %Y %H:%M:%S", tz="GMT")
  
  ratelimit_requests_reset_dt <- parse_duration_to_seconds(
    headers["x-ratelimit-reset-requests"]$`x-ratelimit-reset-requests`)
  ratelimit_tokens_reset_dt <- parse_duration_to_seconds(
    headers["x-ratelimit-reset-tokens"]$`x-ratelimit-reset-tokens`)
  
  list(
    this_request_time = request_time,
    ratelimit_requests = as.integer(
      headers["x-ratelimit-limit-requests"]),
    ratelimit_requests_remaining = as.integer(
      headers["x-ratelimit-remaining-requests"]),
    ratelimit_requests_reset_time = request_time + ratelimit_requests_reset_dt,
    ratelimit_tokens = as.integer(
      headers["x-ratelimit-limit-tokens"]),
    ratelimit_tokens_remaining = as.integer(
      headers["x-ratelimit-remaining-tokens"]),
    ratelimit_tokens_reset_time = request_time + ratelimit_tokens_reset_dt
  )
}

#' A chat parsing method for Openai to extract the assitant response f
#'
#' @noRd
method(parse_chat_function, api_openai) <- function(api) {
  api_label <- api@long_name 
  function(body_json){
    if("error" %in% names(body_json)){
      sprintf("%s returned an Error:\nType: %s\nMessage: %s",
              api_label,
              body_json$error$type,
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

#' A function to get metadata from Openai responses
#'
#' @noRd
method(extract_metadata, list(api_openai,class_list))<- function(api,response) {
  list(
    model             = response$model,
    timestamp         = lubridate::as_datetime(response$created),
    prompt_tokens     = response$usage$prompt_tokens,
    completion_tokens = response$usage$completion_tokens,
    total_tokens      = response$usage$total_tokens,
    specific_metadata = list(
      system_fingerprint        = response$system_fingerprint,
      completion_tokens_details = response$usage$completion_tokens_details,
      prompt_tokens_details     = response$usage$prompt_tokens_details
      ) 
  )
}  

#' A function to get loprobs from Openai responses
#'
#' @noRd
method(parse_logprobs, list(api_openai, class_list)) <- function(api, choices) {
  # Helper to parse each token's logprobs
  parse_token <- function(token_data) {
    list(
      token = token_data$token,
      logprob = token_data$logprob,
      bytes = if (!is.null(token_data$bytes)) unlist(token_data$bytes, use.names = FALSE) else NULL,
      top_logprobs = purrr::map(token_data$top_logprobs, function(tlp) {
        list(
          token = tlp$token,
          logprob = tlp$logprob,
          bytes = if (!is.null(tlp$bytes)) unlist(tlp$bytes, use.names = FALSE) else NULL
        )
      })
    )
  }
  
  # Extract logprobs if available
  if (!is.null(choices$logprobs)) {
    return(purrr::map(choices$logprobs$content, parse_token))
  }
  
  NULL  # Return NULL if no logprobs are found
}



#' A callback function generator for an OpenAI request 
#' request
#'
#' @noRd
method(generate_callback_function,api_openai) <- function(api) {
  callback_fn <- function(.data) {
    # Read the stream content and split into lines
    lines <- .data |>
      rawToChar(multiple = FALSE) |>
      stringr::str_split("\n") |>
      unlist()
    
    # Initialize a flag to control early exit
    continue_processing <- TRUE
    
    # Process lines that start with "data: "
    data_lines <- lines |>
      purrr::keep(~ grepl("^data: ", .x) && .x != "")
    
    # Process data lines
    purrr::walk(data_lines, ~ {
      
      json_part <- sub("^data: ", "", .x)
      
      if (json_part != "[DONE]") {
        # Try to parse the JSON content
        parsed_event <- tryCatch(
          jsonlite::fromJSON(json_part, simplifyVector = FALSE, simplifyDataFrame = FALSE),
          error = function(e) {
            message("Failed to parse JSON: ", e$message)
            return(NULL)
          }
        )
        
        if (!is.null(parsed_event)) {
          if(length(parsed_event$choices)>=1){
            delta_content <- parsed_event$choices[[1]]$delta$content
            if (!is.null(delta_content)) {
              .tidyllm_stream_env$stream <- paste0(.tidyllm_stream_env$stream, delta_content)
              cat(delta_content)
              utils::flush.console()
            }
          }
        }
      } else {
        message("\n---------\nStream finished\n---------\n")
        continue_processing <<- FALSE
      }
    })
    
    return(continue_processing)
  }
}

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
#' @param .presence_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far.
#' @param .seed If specified, the system will make a best effort to sample deterministically.
#' @param .stop Up to 4 sequences where the API will stop generating further tokens.
#' @param .reasoning_effort How long should reasoning models reason (can either be "low","medium" or "high")
#' @param .stream If set to TRUE, the answer will be streamed to console as it comes (default: FALSE).
#' @param .temperature What sampling temperature to use, between 0 and 2. Higher values make the output more random.
#' @param .top_p An alternative to sampling with temperature, called nucleus sampling.
#' @param .api_url Base URL for the API (default: "https://api.openai.com/").
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .verbose Should additional information be shown after the API call (default: FALSE).
#' @param .json_schema A JSON schema object provided by tidyllm schema or ellmer schemata.
#' @param .max_tries Maximum retries to perform request
#' @param .dry_run If TRUE, perform a dry run and return the request object (default: FALSE).
#' @param .compatible If TRUE, skip API and rate-limit checks for OpenAI compatible APIs (default: FALSE).
#' @param .api_path  The path relative to the base `.api_url` for the API (default: "/v1/chat/completions").
#' @param .logprobs If TRUE, get the log probabilities of each output token (default: NULL).
#' @param .top_logprobs If specified, get the top N log probabilities of each output token (0-5, default: NULL).
#'
#'
#' @return A new `LLMMessage` object containing the original messages plus the assistant's response.
#'
#' @export
openai_chat <- function(
    .llm,
    .model = "gpt-4o",
    .max_completion_tokens = NULL,
    .reasoning_effort = NULL,
    .frequency_penalty = NULL,
    .logit_bias = NULL,
    .presence_penalty = NULL,
    .seed = NULL,
    .stop = NULL,
    .stream = FALSE,
    .temperature = NULL,
    .top_p = NULL,
    .api_url = "https://api.openai.com/",
    .timeout = 60,
    .verbose = FALSE,
    .json_schema = NULL,
    .max_tries = 3,
    .dry_run = FALSE,
    .compatible = FALSE,
    .api_path = "/v1/chat/completions",
    .logprobs = NULL,       
    .top_logprobs = NULL
) {
  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = S7_inherits(.llm,LLMMessage),
    "Input .model must be a string" = is.character(.model),
    "Input .max_completion_tokens must be NULL or a positive integer" = is.null(.max_completion_tokens) | (is_integer_valued(.max_completion_tokens) & .max_completion_tokens > 0),   
    "Input .reasoning_effort must be NULL or one of 'low', 'medium', 'high'" = is.null(.reasoning_effort) | (.reasoning_effort %in% c("low", "medium", "high")),
    "Input .frequency_penalty must be numeric or NULL" = is.null(.frequency_penalty) | is.numeric(.frequency_penalty),
    "Input .logit_bias must be a list or NULL" = is.null(.logit_bias) | is.list(.logit_bias),
    "Input .presence_penalty must be numeric or NULL" = is.null(.presence_penalty) | is.numeric(.presence_penalty),
    "Input .seed must be NULL or an integer" = is.null(.seed) | is_integer_valued(.seed),
    "Input .stop must be NULL or a character vector or string" = is.null(.stop) | is.character(.stop),
    "Input .stream must be logical or NULL" = is.null(.stream) | is.logical(.stream),
    "Input .temperature must be numeric or NULL" = is.null(.temperature) | is.numeric(.temperature),
    "Input .top_p must be numeric or NULL" = is.null(.top_p) | is.numeric(.top_p),
    "Input .api_url must be a string" = is.character(.api_url),
    "Input .timeout must be integer-valued numeric" = is_integer_valued(.timeout),
    "Input .verbose must be logical" = is.logical(.verbose),
    "Input .json_schema must be NULL or a list or an ellmer type object" = is.null(.json_schema) | is.list(.json_schema) | is_ellmer_type(.json_schema),
    "Input .max_tries must be integer-valued numeric" = is_integer_valued(.max_tries),
    "Input .dry_run must be logical" = is.logical(.dry_run),
    "Input .compatible must be logical" = is.logical(.compatible),
    "Input .api_path must be a string" = is.character(.api_path),
    "Input .logprobs must be NULL or a logical" = is.null(.logprobs) | is.logical(.logprobs),
    "Input .top_logprobs must be NULL or an integer between 0 and 5" = is.null(.top_logprobs) | (is_integer_valued(.top_logprobs) && .top_logprobs >= 0 && .top_logprobs <= 5)
  ) |> validate_inputs()
  
  
  api_obj <- api_openai(short_name = "openai",
                        long_name  = "OpenAI",
                        api_key_env_var = "OPENAI_API_KEY")
  
  #Filter out the system prompt for reasoning models.
  no_system_prompt <- FALSE
  if(.model %in% c("o1","o1-mini")){
    message("Note: Reasoning models do not support system prompts")
    no_system_prompt <- TRUE
  }
  
  messages <- to_api_format(llm=.llm,
                            api=api_obj,
                            no_system=no_system_prompt)
  
  if (!.compatible) {
    api_key <- get_api_key(api_obj,.dry_run)
  }
  
  # Handle JSON schema and JSON mode
  response_format <- NULL
  json = FALSE
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
    model = .model,
    messages = messages,
    frequency_penalty = .frequency_penalty,
    logit_bias = .logit_bias,
    max_completion_tokens = .max_completion_tokens,
    reasoning_effort = .reasoning_effort,
    presence_penalty = .presence_penalty,
    response_format = response_format,
    seed = .seed,
    stop = .stop,
    stream = .stream,
    temperature = .temperature,
    top_p = .top_p,
    logprobs = .logprobs,        
    top_logprobs = .top_logprobs
  ) |> purrr::compact()
  
  
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
  
  # Return only the request object in a dry run.
  if (.dry_run) {
    return(request)
  }
  
  response <- perform_chat_request(request,api_obj,.stream,.timeout,.max_tries)
  
  # Extract assistant reply and rate limiting info from response headers
  assistant_reply <- response$assistant_reply
  
  #Check whether the result has logprobs in it 
  logprobs  <- parse_logprobs(api_obj, response$raw$content$choices[[1]])
  
  
  #Track the rate limit if we are not using a non-openai api
  if (!.compatible) {track_rate_limit(api_obj,response$headers,.verbose)}
  
  add_message(llm     = .llm,
              role    = "assistant", 
              content = assistant_reply , 
              json    = json,
              meta    = response$meta,
              logprobs = logprobs)
}



#' Generate Embeddings Using OpenAI API
#'
#' @param .input An existing LLMMessage object (or a character vector of texts to embed)
#' @param .model The embedding model identifier (default: "text-embedding-3-small").
#' @param .truncate Whether to truncate inputs to fit the model's context length (default: TRUE).
#' @param .timeout Timeout for the API request in seconds (default: 120).
#' @param .dry_run If TRUE, perform a dry run and return the request object.
#' @param .max_tries Maximum retry attempts for requests (default: 3).
#' @param .verbose Should information about current ratelimits be printed? (default: FALSE)
#' @return A tibble with two columns: `input` and `embeddings`. 
#' The `input` column contains the texts sent to embed, and the `embeddings` column 
#' is a list column where each row contains an embedding vector of the sent input.
#' @export
openai_embedding <- function(.input,
                             .model = "text-embedding-3-small",
                             .truncate = TRUE,
                             .timeout = 120,
                             .dry_run = FALSE,
                             .max_tries = 3,
                             .verbose   = FALSE) {

  # Get the OpenAI API key
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if ((api_key == "") & .dry_run == FALSE) {
    stop("API key is not set. Please set it with: Sys.setenv(OPENAI_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  }
  
  # Validate the inputs
  c(
    "Input .input must be an LLMMessage object or a character vector" = S7_inherits(.input, LLMMessage) | is.character(.input),
    "Input .model must be a string" = is.character(.model),
    "Input .truncate must be logical" = is.logical(.truncate),
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
  
  extract_embeddings_fn <- function(response_content,error,response_headers){
    if(error){
      paste0("API error response - ", response_content$error$message) |>
        stop()
    }
    
    # Parse and update rate limit info from headers
    track_rate_limit(api_openai(short_name = "openai",
                                long_name ="OpenAI"),
                     response_headers,.verbose)
    
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

#' Send a Batch of Messages to OpenAI Batch API
#'
#' This function creates and submits a batch of messages to the OpenAI Batch API for asynchronous processing.
#'
#' @param .llms A list of LLMMessage objects containing conversation histories.
#' @param .model Character string specifying the OpenAI model version (default: "gpt-4o").
#' @param .max_completion_tokens Integer specifying the maximum tokens per response (default: NULL).
#' @param .reasoning_effort How long should reasoning models reason (can either be "low","medium" or "high")
#' @param .frequency_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far.
#' @param .logit_bias A named list modifying the likelihood of specified tokens appearing in the completion.
#' @param .presence_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far.
#' @param .seed If specified, the system will make a best effort to sample deterministically.
#' @param .stop Up to 4 sequences where the API will stop generating further tokens.
#' @param .temperature What sampling temperature to use, between 0 and 2. Higher values make the output more random.
#' @param .top_p An alternative to sampling with temperature, called nucleus sampling.
#' @param .logprobs If TRUE, get the log probabilities of each output token (default: NULL).
#' @param .top_logprobs If specified, get the top N log probabilities of each output token (0-5, default: NULL).
#' @param .dry_run Logical; if TRUE, returns the prepared request object without executing it (default: FALSE).
#' @param .overwrite Logical; if TRUE, allows overwriting an existing batch ID associated with the request (default: FALSE).
#' @param .max_tries Maximum number of retries to perform the request (default: 3).
#' @param .timeout Integer specifying the request timeout in seconds (default: 60).
#' @param .verbose Logical; if TRUE, additional info about the requests is printed (default: FALSE).
#' @param .json_schema A JSON schema object provided by tidyllm_schema or ellmer schemata (default: NULL).
#' @param .id_prefix Character string to specify a prefix for generating custom IDs when names in `.llms` are missing (default: "tidyllm_openai_req_").
#' 
#' @return An updated and named list of `.llms` with identifiers that align with batch responses, including a `batch_id` attribute.
#' @export
send_openai_batch <- function(.llms,
                              .model = "gpt-4o",
                              .max_completion_tokens = NULL,
                              .reasoning_effort = NULL,
                              .frequency_penalty = NULL,
                              .logit_bias = NULL,
                              .presence_penalty = NULL,
                              .seed = NULL,
                              .stop = NULL,
                              .temperature = NULL,
                              .top_p = NULL,
                              .logprobs = NULL,       
                              .top_logprobs = NULL,
                              .dry_run = FALSE,
                              .overwrite = FALSE,
                              .json_schema = NULL,
                              .max_tries = 3,
                              .timeout = 60,
                              .verbose = FALSE,
                              .id_prefix = "tidyllm_openai_req_") {

  # Input validation
  c(
    ".llms must be a list of LLMMessage objects" = is.list(.llms) && all(sapply(.llms, S7_inherits, LLMMessage)),
    ".max_completion_tokens must be NULL or a positive integer" = is.null(.max_completion_tokens) | (is_integer_valued(.max_completion_tokens) & .max_completion_tokens > 0),
    ".reasoning_effort must be NULL or one of 'low', 'medium', 'high'" = is.null(.reasoning_effort) | (.reasoning_effort %in% c("low", "medium", "high")),
    ".frequency_penalty must be numeric or NULL" = is.null(.frequency_penalty) | is.numeric(.frequency_penalty),
    ".logit_bias must be a list or NULL" = is.null(.logit_bias) | is.list(.logit_bias),
    ".presence_penalty must be numeric or NULL" = is.null(.presence_penalty) | is.numeric(.presence_penalty),
    ".seed must be NULL or an integer" = is.null(.seed) | is_integer_valued(.seed),
    ".stop must be NULL or a character vector or string" = is.null(.stop) | is.character(.stop),
    ".temperature must be numeric or NULL" = is.null(.temperature) | is.numeric(.temperature),
    ".top_p must be numeric or NULL" = is.null(.top_p) | is.numeric(.top_p),
    ".logprobs must be NULL or a logical" = is.null(.logprobs) | is.logical(.logprobs),
    ".json_schema must be NULL or a list or an ellmer type object" = is.null(.json_schema) | is.list(.json_schema) | is_ellmer_type(.json_schema),
    ".top_logprobs must be NULL or an integer between 0 and 5" = is.null(.top_logprobs) | (is_integer_valued(.top_logprobs) && .top_logprobs >= 0 && .top_logprobs <= 5),
    ".dry_run must be logical" = is.logical(.dry_run),
    ".verbose must be logical" = is.logical(.verbose),
    ".overwrite must be logical" = is.logical(.overwrite),
    ".id_prefix must be a character vector of length 1" = is.character(.id_prefix),
    ".max_tries must be integer-valued numeric" = is_integer_valued(.max_tries),
    ".timeout must be integer-valued numeric" = is_integer_valued(.timeout)
  ) |> validate_inputs()
  
  api_obj <- api_openai(short_name = "openai",
                        long_name  = "OpenAI",
                        api_key_env_var = "OPENAI_API_KEY")
  
  api_key <- get_api_key(api_obj,.dry_run)


  #This filters out the system prompt for reasoning models.
  no_system_prompt <- FALSE
  if(.model %in% c("o1","o1-mini")){
    message("Note: Reasoning models do not support system prompts")
    no_system_prompt <- TRUE
  }
  
  # Handle JSON schema and JSON mode
  response_format <- NULL
  json = FALSE
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
      model = .model,
      messages = messages,
      frequency_penalty = .frequency_penalty,
      logit_bias = .logit_bias,
      max_completion_tokens = .max_completion_tokens,
      reasoning_effort = .reasoning_effort,
      presence_penalty = .presence_penalty,
      response_format = response_format,
      seed = .seed,
      stop = .stop,
      temperature = .temperature,
      top_p = .top_p,
      logprobs = .logprobs,        
      top_logprobs = .top_logprobs
    ) |> purrr::compact()
    
   
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
    perform_generic_request(.timeout=.timeout,
                            .max_tries = .max_tries)
    
  
  if(.verbose){message("Batch request file uploaded via files API")}
  
  if ("error" %in% names(upload_response$content)) {
    sprintf("OpenAI API returned an Error during file upload:\nType: %s\nMessage: %s",
            upload_response$content$error$type,
            upload_response$content$error$message) |>
      stop()
  }
  
  input_file_id <- upload_response$content$id
  
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
    perform_generic_request(.timeout=.timeout,
                            .max_tries = .max_tries)
  
  batch_response_body <- batch_response$content
  
  if(.verbose){message("Batch request for file sent")}
  if ("error" %in% names(batch_response_body)) {
    sprintf("OpenAI API returned an Error during batch creation:\nType: %s\nMessage: %s",
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
    perform_generic_request(.timeout=.timeout,
                            .max_tries = .max_tries)
  
  # Parse response
  response_body <- response$content
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
    perform_generic_request(.timeout=.timeout,
                            .max_tries = .max_tries)
  
  # Parse response
  response_body <- response$content
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
  
  api_obj <- api_openai(short_name = "openai",
                        long_name  = "OpenAI",
                        api_key_env_var = "OPENAI_API_KEY")
  
  api_key <- get_api_key(api_obj,.dry_run)
  
  # Construct request URL to get batch details
  batch_details_url <- paste0("https://api.openai.com/v1/batches/", .batch_id)
  
  request <- httr2::request(batch_details_url) |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json",
      .redact = "Authorization"
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
      meta_data <- extract_metadata(api_obj,result$response$body)
      logprobs        <- parse_logprobs(api_obj, as.list(result$response$body$choices))
      
      llm <- add_message(llm = .llms[[custom_id]],
                              role = "assistant", 
                              content =  assistant_reply,
                              json = .json,
                              meta = meta_data, 
                              logprobs = logprobs)
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

#' Cancel an In-Progress OpenAI Batch
#'
#' This function cancels an in-progress batch created through the OpenAI API.
#' The batch will be moved to a "cancelling" state and, eventually, "cancelled."
#'
#' @param .batch_id Character; the unique identifier for the batch to cancel.
#' @param .dry_run Logical; if `TRUE`, returns the constructed request without executing it (default: `FALSE`).
#' @param .max_tries Integer; maximum number of retries if the request fails (default: `3`).
#' @param .timeout Integer; request timeout in seconds (default: `60`).
#'
#' @return A list containing the response from the OpenAI API about the cancellation status.
#' @export
cancel_openai_batch <- function(.batch_id,
                                .dry_run = FALSE,
                                .max_tries = 3,
                                .timeout = 60) {
  # Validate inputs
  c(
    ".batch_id must be a non-empty character string" = is.character(.batch_id) && nzchar(.batch_id),
    ".dry_run must be logical" = is.logical(.dry_run),
    ".max_tries must be integer-valued numeric" = is_integer_valued(.max_tries),
    ".timeout must be integer-valued numeric" = is_integer_valued(.timeout)
  ) |> validate_inputs()
  
  # Retrieve API key
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "" && !.dry_run) {
    stop("API key is not set. Please set it with: Sys.setenv(OPENAI_API_KEY = \"YOUR-KEY-GOES-HERE\").")
  }
  
  # Construct the cancellation request URL
  request_url <- paste0("https://api.openai.com/v1/batches/", .batch_id, "/cancel")
  
  # Create the request
  request <- httr2::request(request_url) |>
    httr2::req_headers(
      Authorization = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_method("POST")
  
  # Return the request object if it's a dry run
  if (.dry_run) {
    return(request)
  }
  
  # Perform the request with retries
  response <- request |>
    perform_generic_request(.timeout = .timeout, .max_tries = .max_tries)
  
  # Parse the response
  response_body <- response$content
  if ("error" %in% names(response_body)) {
    sprintf("OpenAI API returned an Error:\nType: %s\nMessage: %s",
            response_body$error$type,
            response_body$error$message) |>
      stop()
  }
  
  tibble::tibble(
    batch_id = response_body$id,
    status = response_body$status,
    created_at = lubridate::as_datetime(response_body$created_at),
    cancelling_at = lubridate::as_datetime(response_body$cancelling_at),
    cancelled_at = lubridate::as_datetime(response_body$cancelled_at),
    expires_at = lubridate::as_datetime(response_body$expires_at),
    total_requests = response_body$request_counts$total,
    completed_requests = response_body$request_counts$completed,
    failed_requests = response_body$request_counts$failed,
    batch_description = response_body$metadata$batch_description
  )
  
}


#' List Available Models from the OpenAI API
#'
#' @param .api_url Base URL for the API (default: "https://api.openai.com").
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .max_tries Maximum number of retries for the API request (default: 3).
#' @param .dry_run Logical; if TRUE, returns the prepared request object without executing it.
#' @param .verbose Logical; if TRUE, prints additional information about the request.
#'
#' @return A tibble containing model information (columns include `id`, `created`, and `owned_by`),
#'   or NULL if no models are found.
#'
#' @export
openai_list_models <- function(.api_url = "https://api.openai.com",
                               .timeout = 60,
                               .max_tries = 3,
                               .dry_run = FALSE,
                               .verbose = FALSE) {
  # Create an API object for OpenAI using the tidyllm helper
  api_obj <- api_openai(short_name = "openai",
                        long_name  = "OpenAI",
                        api_key_env_var = "OPENAI_API_KEY")
  
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
    message("Retrieved response from OpenAI: ", response$object)
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
      }, character(1)),
      owned_by = vapply(models, function(model) model$owned_by, character(1))
    )
    
    return(model_info)
  } else {
    return(NULL)
  }
}


#' OpenAI Provider Function
#'
#' The `openai()` function acts as an interface for interacting with the OpenAI API 
#' through main `tidyllm` verbs such as `chat()`, `embed()`, and 
#' `send_batch()`. It dynamically routes requests to OpenAI-specific functions 
#' like `openai_chat()` and `openai_embedding()` based on the context of the call.
#'
#' @param ... Parameters to be passed to the appropriate OpenAI-specific function, 
#'   such as model configuration, input text, or API-specific options.
#' @param .called_from An internal argument that specifies which action (e.g., 
#'   `chat`, `embed`, `send_batch`) the function is being invoked from. 
#'   This argument is automatically managed and should not be modified by the user.
#'
#' @return The result of the requested action, depending on the specific function invoked 
#'   (e.g., an updated `LLMMessage` object for `chat()`, or a matrix for `embed()`).
#' 
#' @export
openai <- create_provider_function(
  .name = "openai",
  chat = openai_chat,
  embed = openai_embedding,
  send_batch = send_openai_batch,
  check_batch = check_openai_batch,
  list_batches = list_openai_batches,
  fetch_batch = fetch_openai_batch,
  list_models = openai_list_models
)

#' Alias for the OpenAI Provider Function
#'
#' The `chatgpt` function is an alias for the `openai()` provider function. 
#' It provides a convenient way to interact with the OpenAI API for tasks such 
#' as sending chat messages, generating embeddings, and handling batch operations 
#' using `tidyllm` verbs like `chat()`, `embed()`, and `send_batch()`.
#'
#' @param ... Parameters to be passed to the appropriate OpenAI-specific function, 
#'   such as model configuration, input text, or other API-specific options.
#' @param .called_from An internal argument that specifies the context (e.g., 
#'   `chat`, `embed`, `send_batch`) in which the function is being 
#'   invoked. This is automatically managed and should not be modified by the user.
#'
#' @return The result of the requested action, depending on the specific function invoked 
#'   (e.g., an updated `LLMMessage` object for `chat()`, or a matrix for `embed()`).
#' 
#' @export
chatgpt <- openai




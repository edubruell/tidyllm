
#' The Ollama API provider stub
#'
#' At the moment this is just a stub but is needed for methods dispatch
#'
#' @noRd
api_ollama <- new_class("Ollama", APIProvider)


#' Convert LLMMessage to Ollama API-Compatible Format
#'
#' Converts the `message_history` of an `LLMMessage` object into the
#' one needed for the Ollama API.
#'
#' @noRd
method(to_api_format, list(LLMMessage, api_ollama)) <- function(.llm, 
                                                                .api) {
  
  ollama_history <- filter_roles(.llm@message_history, c("user", "assistant"))
  lapply(ollama_history, function(m) {
    formatted_message <- format_message(m)
    if (!is.null(formatted_message$image)) {
      list(
        role = m$role,
        content = formatted_message$content,
        images = list(glue::glue("{formatted_message$image$data}"))
      )
    } else {
      list(role = m$role, content = formatted_message$content)
    }
  })
}

#' A function to get metadata from Ollama responses
#'
#' @noRd
method(extract_metadata, list(api_ollama,class_list))<- function(.api,.response) {
  list(
    model             = .response$model,
    timestamp         = lubridate::as_datetime(.response$created_at),
    prompt_tokens     = .response$prompt_eval_count,
    completion_tokens = .response$eval_count,
    total_tokens      = .response$prompt_eval_count + .response$eval_count,
    stream            = FALSE,
    specific_metadata = list(
      done_reason     = .response$done_reason,
      total_duration_ns  = .response$total_duration,
      load_duration_ns   = .response$load_duration,
      eval_duration_ns   = .response$eval_duration
    ) 
  )
}  

#' A function to get metadata from Openai streaming responses
#'
#' @noRd
method(extract_metadata_stream, list(api_ollama,class_list))<- function(.api,.stream_raw_data) {
  final_stream_chunk <- .stream_raw_data[[length(.stream_raw_data)]]
  
  list(
    model             = final_stream_chunk$model,
    timestamp         = lubridate::as_datetime(final_stream_chunk$created),
    prompt_tokens     = final_stream_chunk$prompt_eval_count,
    completion_tokens = final_stream_chunk$eval_count,
    total_tokens      = final_stream_chunk$prompt_eval_count + final_stream_chunk$eval_count,
    stream            = TRUE,
    specific_metadata = list(
      done_reason     = final_stream_chunk$done_reason,
      total_duration_ns  = final_stream_chunk$total_duration,
      load_duration_ns   = final_stream_chunk$load_duration,
      eval_duration_ns   = final_stream_chunk$eval_duration
    ) 
  )
}  

#' A method to handle streaming requests for ollama
#'
#' @noRd
method(handle_stream,list(api_ollama,new_S3_class("httr2_response"))) <- function(.api,.stream_response) {
  stream_text <- ""
  stream_data <- list()
  repeat {
    #Sleep for a tiny bit because Ollama often hangs on sending complete lines
    Sys.sleep(0.25)
    stream_chunk   <- httr2::resp_stream_lines(.stream_response)
  
    stream_content <- stream_chunk |> 
      jsonlite::fromJSON()
    
    stream_data <- append(stream_data,list(stream_content))
    if (stream_content$done==TRUE) {
      close(.stream_response)
      message("\n---------\nStream finished\n---------\n")
      break
    }

    chunk_text  <- stream_content$message$content
    stream_text <- paste0(stream_text, chunk_text)
    cat(chunk_text)
    utils::flush.console()
    next
  }
  
  list(
    reply = stream_text,
    raw_data = stream_data
  )
}



#' A chat parsing method for Ollama to extract the assitant response 
#'
#' @noRd
method(parse_chat_response, list(api_ollama,class_list)) <- function(.api,.content) {
  api_label <- .api@long_name 
  if("error" %in% names(.content)){
    sprintf("%s returned an Error:\nMessage: %s",
            api_label,
            .content$error$type) |>
      stop()
  }
  
  .content$message$content
}



#' A method to run tool calls on Ollama and create the expected response
#'
#' @noRd
method(run_tool_calls, list(api_ollama, class_list, class_list)) <- function(.api, .tool_calls, .tools) {

  # Iterate over tool calls
  tool_results <- purrr::map(.tool_calls, function(tool_call) {
    tool_name <- tool_call$`function`$name
    tool_args <- tool_call$`function`$arguments
    tool_call_id <- tool_call$id
    

    if (is.null(tool_args)) {
      warning(sprintf("Failed to parse arguments for tool: %s", tool_name))
      return(NULL)
    }
    
    # Find the corresponding tool
    matching_tool <- purrr::keep(.tools, ~ .x@name == tool_name)
    
    if (length(matching_tool) == 0) {
      warning(sprintf("No matching tool found for: %s", tool_name))
      return(NULL)
    }
    
    tool_function <-  matching_tool[[1]]@func
    
    # Execute the function with extracted arguments
    tool_result <- utils::capture.output(
                      do.call(tool_function, as.list(tool_args))
                                         ,file = NULL) |> 
                  stringr::str_c(collapse = "\n")
    
    # Format the response for OpenAI
    list(
      role = "tool",
      tool_call_id = tool_call_id,
      name = tool_name,
      content = tool_result
    )
  })
  
  # Remove NULL results (failed tool executions)
  tool_results <- purrr::compact(tool_results)
  list(list(role="assistant",
            content=" ",
            tool_calls=tool_calls)) |> 
    c(tool_results)
}


#' Interact with local AI models via the Ollama API
#'
#'
#' @param .llm An LLMMessage object containing the conversation history and system prompt.
#' @param .model Character string specifying the Ollama model to use (default: "gemma2")
#' @param .stream Logical; whether to stream the response (default: FALSE)
#' @param .seed Integer; seed for reproducible generation (default: NULL)
#' @param .json_schema A JSON schema object as R list to enforce the output structure (default: NULL)
#' @param .temperature Float between 0-2; controls randomness in responses (default: NULL)
#' @param .num_ctx Integer; sets the context window size (default: 2048)
#' @param .num_predict Integer; maximum number of tokens to predict (default: NULL)
#' @param .top_k Integer; controls diversity by limiting top tokens considered (default: NULL)
#' @param .top_p Float between 0-1; nucleus sampling threshold (default: NULL)
#' @param .min_p Float between 0-1; minimum probability threshold (default: NULL)
#' @param .mirostat Integer (0,1,2); enables Mirostat sampling algorithm (default: NULL)
#' @param .mirostat_eta Float; Mirostat learning rate (default: NULL)
#' @param .mirostat_tau Float; Mirostat target entropy (default: NULL)
#' @param .repeat_last_n Integer; tokens to look back for repetition (default: NULL)
#' @param .repeat_penalty Float; penalty for repeated tokens (default: NULL)
#' @param .tools Either a single TOOL object or a list of TOOL objects representing the available functions for tool calls.
#' @param .tfs_z Float; tail free sampling parameter (default: NULL)
#' @param .stop Character; custom stop sequence(s) (default: NULL)
#' @param .keep_alive Character; How long should the ollama model be kept in memory after request (default: NULL - 5 Minutes)
#' @param .ollama_server String; Ollama API endpoint (default: "http://localhost:11434")
#' @param .timeout Integer; API request timeout in seconds (default: 120)
#' @param .dry_run Logical; if TRUE, returns request object without execution (default: FALSE)
#'
#' @return A new LLMMessage object containing the original messages plus the model's response
#'
#' @details
#' The function provides extensive control over the generation process through various parameters:
#' - Temperature (0-2): Higher values increase creativity, lower values make responses more focused
#' - Top-k/Top-p: Control diversity of generated text
#' - Mirostat: Advanced sampling algorithm for maintaining consistent complexity
#' - Repeat penalties: Prevent repetitive text
#' - Context window: Control how much previous conversation is considered
#'
#' @examples
#' \dontrun{
#' llm_message("user", "Hello, how are you?")
#' response <- ollama_chat(llm, .model = "gemma2", .temperature = 0.7)
#' 
#' # With custom parameters
#' response <- ollama_chat(
#'   llm,
#'   .model = "llama2",
#'   .temperature = 0.8,
#'   .top_p = 0.9,
#'   .num_ctx = 4096
#' )
#' }
#'
#' @export
ollama_chat <- function(.llm,
                   .model = "gemma2",
                   .stream = FALSE,
                   .seed = NULL,
                   .json_schema = NULL,
                   .temperature = NULL,
                   .num_ctx = 2048,
                   .num_predict = NULL,
                   .top_k = NULL,
                   .top_p = NULL,
                   .min_p = NULL,
                   .mirostat = NULL,
                   .mirostat_eta = NULL,
                   .mirostat_tau = NULL,
                   .repeat_last_n = NULL,
                   .repeat_penalty = NULL,
                   .tools = NULL,
                   .tfs_z = NULL,
                   .stop = NULL,
                   .ollama_server = "http://localhost:11434",
                   .timeout = 120,
                   .keep_alive = NULL,
                   .dry_run = FALSE) {

  # Validate the inputs
  c(
    "Input .llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
    "Input .model must be a string" = is.character(.model),
    "Input .stream must be logical if provided" = is.logical(.stream),
    "Input .json_schema must be NULL or a list or an ellmer type object" = is.null(.json_schema) | is.list(.json_schema) | is_ellmer_type(.json_schema),
    "Input .temperature must be numeric between 0 and 2 if provided" = is.null(.temperature) || (is.numeric(.temperature) && .temperature >= 0 && .temperature <= 2),
    "Input .seed must be an integer-valued numeric if provided" = is.null(.seed) || is_integer_valued(.seed),
    "Input .num_ctx must be a positive integer if provided" = is.null(.num_ctx) || (is_integer_valued(.num_ctx) && .num_ctx > 0),
    "Input .num_predict must be an integer if provided" = is.null(.num_predict) || is_integer_valued(.num_predict),
    "Input .top_k must be a positive integer if provided" = is.null(.top_k) || (is_integer_valued(.top_k) && .top_k > 0),
    "Input .top_p must be numeric between 0 and 1 if provided" = is.null(.top_p) || (is.numeric(.top_p) && .top_p >= 0 && .top_p <= 1),
    "Input .min_p must be numeric between 0 and 1 if provided" = is.null(.min_p) || (is.numeric(.min_p) && .min_p >= 0 && .min_p <= 1),
    "Input .mirostat must be 0, 1, or 2 if provided" = is.null(.mirostat) || (.mirostat %in% c(0, 1, 2)),
    "Input .mirostat_eta must be positive numeric if provided" = is.null(.mirostat_eta) || (is.numeric(.mirostat_eta) && .mirostat_eta > 0),
    "Input .mirostat_tau must be positive numeric if provided" = is.null(.mirostat_tau) || (is.numeric(.mirostat_tau) && .mirostat_tau > 0),
    "Input .repeat_last_n must be integer if provided" = is.null(.repeat_last_n) || is_integer_valued(.repeat_last_n),
    "Input .repeat_penalty must be positive numeric if provided" = is.null(.repeat_penalty) || (is.numeric(.repeat_penalty) && .repeat_penalty > 0),
    "Input .tfs_z must be positive numeric if provided" = is.null(.tfs_z) || (is.numeric(.tfs_z) && .tfs_z > 0),
    "Input .stop must be character if provided" = is.null(.stop) || is.character(.stop),
    "Input .timeout must be a positive integer (seconds)" = is_integer_valued(.timeout) && .timeout > 0,
    "Input .keep_alive must be character" =  is.null(.keep_alive) ||  is.character(.keep_alive),
    "Input .tools must be NULL, a TOOL object, or a list of TOOL objects" = is.null(.tools) || S7_inherits(.tools, TOOL) || (is.list(.tools) && all(purrr::map_lgl(.tools, ~ S7_inherits(.x, TOOL)))),
    "Input .dry_run must be logical" = is.logical(.dry_run),
    "Streaming is not supported for requests with tool calls" = is.null(.tools) || !isTRUE(.stream)
  ) |>
    validate_inputs()
  
  api_obj <- api_ollama(short_name = "ollama",long_name = "Ollama")
  # Get formatted message list for ollama models
  ollama_messages <-  to_api_format(.llm,api_obj)
  
  # Handle JSON schema
  json=FALSE
  if (!is.null(.json_schema)) {
    json=TRUE
  } 
  if (requireNamespace("ellmer", quietly = TRUE)) {
    #Handle ellmer json schemata Objects
    if(S7_inherits(.json_schema,ellmer::TypeObject)){
      .json_schema = to_schema(.json_schema)
    } 
  }
  
  #Put a single tool into a list if only one is provided. 
  tools_def <- if (!is.null(.tools)) {
    if (S7_inherits(.tools, TOOL))  list(.tools) else .tools
  } else {
    NULL
  }
  
  ollama_options <- list(
    temperature = .temperature,
    seed = .seed,
    num_ctx = .num_ctx,
    num_predict = .num_predict,
    top_k = .top_k,
    top_p = .top_p,
    min_p = .min_p,
    mirostat = .mirostat,
    mirostat_eta = .mirostat_eta,
    mirostat_tau = .mirostat_tau,
    repeat_last_n = .repeat_last_n,
    repeat_penalty = .repeat_penalty,
    tfs_z = .tfs_z,
    stop = .stop
  ) |> purrr::compact()
  
  ollama_request_body <- list(
    model = .model,
    messages = ollama_messages,
    options = ollama_options,
    stream = .stream,
    format = .json_schema,
    tools = if(!is.null(tools_def)) tools_to_api(api_obj,tools_def) else NULL
  )  |> purrr::compact()
  
  # Add keep_alive to request body only if it's provided
  if (!is.null(.keep_alive)) {
    ollama_request_body$keep_alive <- .keep_alive
  }

  # Build the request
  request <- httr2::request(.ollama_server) |>
    httr2::req_url_path("/api/chat") |>
    httr2::req_body_json(ollama_request_body)
  
  # Return only the request object in a dry run.
  if (.dry_run) {
    return(request)  
  }
  
  # Perform the API request
  response <- perform_chat_request(request,api_obj,.stream,.timeout,3)
  if(r_has_name(response$raw,"tool_calls")){
    #Tool call logic can go here!
    tool_messages <- run_tool_calls(api_obj,
                                    response$raw$content$message$tool_calls,
                                    tools_def)
    ##Append the tool call to API
    ollama_request_body$messages <- ollama_request_body$messages |> 
      append(tool_messages)
    
    request <- request |>
      httr2::req_body_json(data = ollama_request_body)
    
    response <- perform_chat_request(request,api_obj,.stream,.timeout,3)
  }
  
  add_message(.llm     = .llm,
              .role    = "assistant", 
              .content = response$assistant_reply, 
              .json    = json,
              .meta    = response$meta)
}


#' Generate Embeddings Using Ollama API
#'
#' @param .input Aa charachter vector of texts to embed or an `LLMMessage` object
#' @param .model The embedding model identifier (default: "all-minilm").
#' @param .truncate Whether to truncate inputs to fit the model's context length (default: TRUE).
#' @param .ollama_server The URL of the Ollama server to be used (default: "http://localhost:11434").
#' @param .timeout Timeout for the API request in seconds (default: 120).
#' @param .dry_run If TRUE, perform a dry run and return the request object.
#' @return A matrix where each column corresponds to the embedding of a message in the message history.
#' @export
ollama_embedding <- function(.input,
                             .model = "all-minilm",
                             .truncate = TRUE,
                             .ollama_server = "http://localhost:11434",
                             .timeout = 120,
                             .dry_run=FALSE) {

  # Validate the inputs
  c(
    "Input .input must be a character vector or an LLMMessage object" = S7_inherits(.input, LLMMessage) | is.character(.input),
    "Input .model must be a string" = is.character(.model),
    "Input .truncate must be logical" = is.logical(.truncate),
    "Input .timeout must be an integer-valued numeric (seconds till timeout)" = is.numeric(.timeout) && .timeout > 0,
    ".dry_run must be logical"                   = is.logical(.dry_run)
  ) |> validate_inputs()
  
  input_texts <- parse_embedding_input(.input)
  
  # Prepare the request body
  request_body <- list(
    model = .model,
    input = input_texts,
    truncate = .truncate
  )
  
  # Build the request
  request <- httr2::request(.ollama_server) |>
    httr2::req_url_path("/api/embed") |>
    httr2::req_body_json(request_body)
  
  # Return only the request object in a dry run.
  if (.dry_run) {
    return(request)  
  }
  
  extract_embeddings_fn <- function(response_content,error,headers){
    if(error){
      paste0("API error response - ", response_content$error) |>
        stop()
    }
    response_content$embeddings |>
      purrr::map(unlist)
  }
  
  # Perform a standard embedding API request
 perform_embedding_request(.request = request,
                           .timeout = .timeout,
                           .max_tries = 3,
                           .input_texts = input_texts, 
                           .fn_extract_embeddings = extract_embeddings_fn)
}


#' Send a Batch of Messages to Ollama API
#'
#' This function creates and submits a batch of messages to the Ollama API
#' Contrary to other batch functions, this functions waits for the batch to finish and receives requests.
#' The advantage compared to sending single messages via `chat()` is that Ollama handles large parallel
#' requests quicker than many individual chat requests.
#'
#' @param .llms A list of LLMMessage objects containing conversation histories.
#' @param .model Character string specifying the Ollama model to use (default: "gemma2")
#' @param .stream Logical; whether to stream the response (default: FALSE)
#' @param .seed Integer; seed for reproducible generation (default: NULL)
#' @param .json_schema A JSON schema object as R list to enforce the output structure (default: NULL)
#' @param .temperature Float between 0-2; controls randomness in responses (default: NULL)
#' @param .num_ctx Integer; sets the context window size (default: 2048)
#' @param .num_predict Integer; maximum number of tokens to predict (default: NULL)
#' @param .top_k Integer; controls diversity by limiting top tokens considered (default: NULL)
#' @param .top_p Float between 0-1; nucleus sampling threshold (default: NULL)
#' @param .min_p Float between 0-1; minimum probability threshold (default: NULL)
#' @param .mirostat Integer (0,1,2); enables Mirostat sampling algorithm (default: NULL)
#' @param .mirostat_eta Float; Mirostat learning rate (default: NULL)
#' @param .mirostat_tau Float; Mirostat target entropy (default: NULL)
#' @param .repeat_last_n Integer; tokens to look back for repetition (default: NULL)
#' @param .repeat_penalty Float; penalty for repeated tokens (default: NULL)
#' @param .tfs_z Float; tail free sampling parameter (default: NULL)
#' @param .stop Character; custom stop sequence(s) (default: NULL)
#' @param .keep_alive Character; How long should the ollama model be kept in memory after request (default: NULL - 5 Minutes)
#' @param .ollama_server String; Ollama API endpoint (default: "http://localhost:11434")
#' @param .timeout Integer; API request timeout in seconds (default: 120)
#' @param .dry_run Logical; if TRUE, returns request object without execution (default: FALSE)
#'
#' @return A list of updated `LLMMessage` objects, each with the assistant's response added if successful.
#'
#' @details
#' The function provides extensive control over the generation process through various parameters:
#' - Temperature (0-2): Higher values increase creativity, lower values make responses more focused
#' - Top-k/Top-p: Control diversity of generated text
#' - Mirostat: Advanced sampling algorithm for maintaining consistent complexity
#' - Repeat penalties: Prevent repetitive text
#' - Context window: Control how much previous conversation is considered
#'
#' @export
send_ollama_batch <- function(.llms,
                        .model = "gemma2",
                        .stream = FALSE,
                        .seed = NULL,
                        .json_schema = NULL,
                        .temperature = NULL,
                        .num_ctx = 2048,
                        .num_predict = NULL,
                        .top_k = NULL,
                        .top_p = NULL,
                        .min_p = NULL,
                        .mirostat = NULL,
                        .mirostat_eta = NULL,
                        .mirostat_tau = NULL,
                        .repeat_last_n = NULL,
                        .repeat_penalty = NULL,
                        .tfs_z = NULL,
                        .stop = NULL,
                        .ollama_server = "http://localhost:11434",
                        .timeout = 120,
                        .keep_alive = NULL,
                        .dry_run = FALSE) {

  # Validate the inputs
  c(
    ".llms must be a list of LLMMessage objects" = is.list(.llms) && all(sapply(.llms, S7_inherits, LLMMessage)),
    "Input .model must be a string" = is.character(.model),
    "Input .stream must be logical if provided" = is.logical(.stream),
    "Input .json_schema must be NULL or a list" = is.null(.json_schema) | is.list(.json_schema),
    "Input .temperature must be numeric between 0 and 2 if provided" = is.null(.temperature) || (is.numeric(.temperature) && .temperature >= 0 && .temperature <= 2),
    "Input .seed must be an integer-valued numeric if provided" = is.null(.seed) || is_integer_valued(.seed),
    "Input .num_ctx must be a positive integer if provided" = is.null(.num_ctx) || (is_integer_valued(.num_ctx) && .num_ctx > 0),
    "Input .num_predict must be an integer if provided" = is.null(.num_predict) || is_integer_valued(.num_predict),
    "Input .top_k must be a positive integer if provided" = is.null(.top_k) || (is_integer_valued(.top_k) && .top_k > 0),
    "Input .top_p must be numeric between 0 and 1 if provided" = is.null(.top_p) || (is.numeric(.top_p) && .top_p >= 0 && .top_p <= 1),
    "Input .min_p must be numeric between 0 and 1 if provided" = is.null(.min_p) || (is.numeric(.min_p) && .min_p >= 0 && .min_p <= 1),
    "Input .mirostat must be 0, 1, or 2 if provided" = is.null(.mirostat) || (.mirostat %in% c(0, 1, 2)),
    "Input .mirostat_eta must be positive numeric if provided" = is.null(.mirostat_eta) || (is.numeric(.mirostat_eta) && .mirostat_eta > 0),
    "Input .mirostat_tau must be positive numeric if provided" = is.null(.mirostat_tau) || (is.numeric(.mirostat_tau) && .mirostat_tau > 0),
    "Input .repeat_last_n must be integer if provided" = is.null(.repeat_last_n) || is_integer_valued(.repeat_last_n),
    "Input .repeat_penalty must be positive numeric if provided" = is.null(.repeat_penalty) || (is.numeric(.repeat_penalty) && .repeat_penalty > 0),
    "Input .tfs_z must be positive numeric if provided" = is.null(.tfs_z) || (is.numeric(.tfs_z) && .tfs_z > 0),
    "Input .stop must be character if provided" = is.null(.stop) || is.character(.stop),
    "Input .timeout must be a positive integer (seconds)" = is_integer_valued(.timeout) && .timeout > 0,
    "Input .keep_alive must be character" =  is.null(.keep_alive) ||  is.character(.keep_alive),
    "Input .dry_run must be logical" = is.logical(.dry_run)
  ) |>
    validate_inputs()
  
  api_obj <- api_ollama(short_name = "ollama",long_name = "Ollama")
  
  # Handle JSON schema
  json=FALSE
  if (!is.null(.json_schema)) {
    json=TRUE
  } 
  if (requireNamespace("ellmer", quietly = TRUE)) {
    #Handle ellmer json schemata Objects
    if(S7_inherits(.json_schema,ellmer::TypeObject)){
      .json_schema = to_schema(.json_schema)
    } 
  }
  
  # Prepare the request lines
  ollama_requests <- lapply(seq_along(.llms), function(i) { 

    # Get messages from each LLMMessage object
    ollama_messages <- to_api_format(.llm = .llms[[i]],
                              .api=api_obj)
    

    ollama_options <- list(
      temperature = .temperature,
      seed = .seed,
      num_ctx = .num_ctx,
      num_predict = .num_predict,
      top_k = .top_k,
      top_p = .top_p,
      min_p = .min_p,
      mirostat = .mirostat,
      mirostat_eta = .mirostat_eta,
      mirostat_tau = .mirostat_tau,
      repeat_last_n = .repeat_last_n,
      repeat_penalty = .repeat_penalty,
      tfs_z = .tfs_z,
      stop = .stop
    ) |> purrr::compact()
    
    ollama_request_body <- list(
      model = .model,
      messages = ollama_messages,
      options = ollama_options,
      stream = .stream,
      format = .json_schema
    )  |> purrr::compact()
    
    # Add keep_alive to request body only if it's provided
    if (!is.null(.keep_alive)) {
      ollama_request_body$keep_alive <- .keep_alive
    }
    
    # Build the request
    request <- httr2::request(.ollama_server) |>
      httr2::req_url_path("/api/chat") |>
      httr2::req_body_json(ollama_request_body)
  })
  

  # Return only the request object in a dry run.
  if (.dry_run) {
    return(ollama_requests)  
  }
  
  ollama_responses <- ollama_requests |>
    httr2::req_perform_parallel()
  
  updated_llms <- lapply(seq_along(.llms), function(i) {
    response_content <- httr2::resp_body_json(ollama_responses[[i]])
    
    meta <- extract_metadata(api_obj,response_content)
    chat_content <- parse_chat_response(api_obj,response_content)
    add_message(.llm     = .llms[[i]],
                .role    = "assistant", 
                .content = chat_content, 
                .json    = json,
                .meta    = meta)
  })
  
  return(updated_llms)
}


#' Retrieve and return model information from the Ollama API
#' 
#' This function connects to the Ollama API and retrieves information 
#' about available models, returning it as a tibble.
#' @return A tibble containing model information, or NULL if no models are found.
#' @param .ollama_server The URL of the ollama server to be used
#' @export
ollama_list_models <- function(.ollama_server = "http://localhost:11434") {
  
  # Perform the request and save the response
  response <- httr2::request(.ollama_server) |>
    httr2::req_url_path("/api/tags") |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  
  # Check if 'models' key exists in the response
  if (!is.null(response$models)) {
    models <- response$models
    
    # Create a tibble with model information
    model_info <- tibble::tibble(
      name = sapply(models, function(model) model$name),
      modified_at = sapply(models, function(model) model$modified_at),
      size = sapply(models, function(model) model$size),
      format = sapply(models, function(model) model$details$format),
      family = sapply(models, function(model) model$details$family),
      parameter_size = sapply(models, function(model) model$details$parameter_size),
      quantization_level = sapply(models, function(model) model$details$quantization_level)
    )
    
    return(model_info)
    
  } else {
    # Return NULL if no models are found
    return(NULL)
  }
}


#' Download a model from the Ollama API
#'
#' This function sends a request to the Ollama API to download a specified model 
#' from Ollama's large online library of models. 
#' 
#' @param .model The name of the model to download.
#' @param .ollama_server The base URL of the Ollama API (default is "http://localhost:11434").
#' @return NULL
#' @export
ollama_download_model <- function(.model, .ollama_server = "http://localhost:11434") {
  
  # Initialize a progress bar
  progress_bar <- cli::cli_progress_bar(auto_terminate = FALSE, type = "download")
  
  # Prepare the callback function for handling stream data
  callback_modeldownload_stream <- function(.stream, progress_bar) {
    line <- rawToChar(.stream)
    line <- trimws(line)
    
    # Ignore empty lines
    if (nchar(line) == 0) {
      return(TRUE)
    }
    
    # Parse the JSON with error handling
    stream_content <- tryCatch(
      jsonlite::fromJSON(line),
      error = function(e) NULL
    )
    
    if (is.null(stream_content)) {
      return(TRUE)
    }
    
    # Show a progress bar for the download 
    if (stringr::str_detect(stream_content$status, "pulling")) {
      if (!is.null(stream_content$total) && !is.null(stream_content$completed) &&
          stream_content$total > 0 && stream_content$completed >= 0) {
        # Update the progress bar
        cli::cli_progress_update(set = stream_content$completed,
                                 total = stream_content$total,
                                 id = progress_bar)
      }
    }
    TRUE
  }
  
  # Build and perform the request
  request <- httr2::request(.ollama_server) |>
    httr2::req_url_path("/api/pull") |>
    httr2::req_body_json(list(name = .model)) |>
    httr2::req_perform_stream(function(.stream) callback_modeldownload_stream(.stream, progress_bar),
                              buffer_kb = 0.05, round = "line")
  
  cli::cli_progress_done(id = progress_bar)
  invisible(NULL)
}

#' Delete a model from the Ollama API
#'
#' This function sends a DELETE request to the Ollama API to remove a specified model.
#'
#' @param .model The name of the model to delete.
#' @param .ollama_server The base URL of the Ollama API (default is "http://localhost:11434").
#' @return NULL
#' @export
ollama_delete_model <- function(.model, .ollama_server = "http://localhost:11434") {
  

  # Perform DELETE request to remove the model
  httr2::request(.ollama_server) |>
    httr2::req_url_path_append("/api/delete") |>
    httr2::req_method("DELETE") |>
    httr2::req_body_json(list(name = .model)) |>
    httr2::req_error(body = function(resp) httr2::resp_body_json(resp)$error) |>
    httr2::req_perform()
  
  # Run ollama_list_models() to show available models after deletion
  warning(paste0("Model ", .model," deleted"))
  
  ollama_list_models(.ollama_server)
}



#' Ollama API Provider Function
#'
#' The `ollama()` function acts as an interface for interacting with local AI models via the Ollama API.
#' It integrates seamlessly with the main `tidyllm` verbs such as `chat()` and `embed()`.
#' 
#' Some functionalities, like `ollama_download_model()` or `ollama_list_models()`
#' are unique to the Ollama API and do not have a general verb counterpart. 
#' These functions can be only accessed directly.
#'
#' @param ... Parameters to be passed to the appropriate Ollama-specific function, 
#'   such as model configuration, input text, or API-specific options.
#' @param .called_from An internal argument specifying the verb (e.g., `chat`, `embed`) 
#'   the function is invoked from. This argument is automatically managed by `tidyllm` and 
#'   should not be set by the user.
#'
#' @return The result of the requested action:
#'   - For `chat()`: An updated `LLMMessage` object containing the model's response.
#'   - For `embed()`: A matrix where each column corresponds to an embedding.
#'
#' @details
#' Supported Verbs:
#' - **`chat()`**: Sends a message to an Ollama model and retrieves the model's response.
#' - **`embed()`**: Generates embeddings for input texts using an Ollama model.
#' - **`send_batch()`**: Behaves different than the other `send_batch()` verbs since it immediately processes the answers
#' @export
ollama <- create_provider_function(
  .name = "ollama",
  chat = ollama_chat,
  embed = ollama_embedding,
  send_batch = send_ollama_batch,
  list_models = ollama_list_models   
)

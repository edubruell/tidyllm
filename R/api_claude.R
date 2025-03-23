#' The Claude API provider class
#'
#' @noRd
api_claude <- new_class("Claude", APIProvider)


#' Convert LLMMessage to Claude API-Compatible Format
#'
#' Converts the `message_history` of an `LLMMessage` object into the
#' one needed for the Anthropic Claude API.
#'
#' @noRd
method(to_api_format, list(LLMMessage, api_claude)) <- function(.llm, 
                                                                .api) {
  
  claude_history <- filter_roles(.llm@message_history, c("user", "assistant"))
  lapply(claude_history, function(m) {
    formatted_message <- format_message(m)
    if (!is.null(formatted_message$image)) {
      list(role = m$role, content = list(
        list(type = "image", source = formatted_message$image),
        list(type = "text", text = formatted_message$content)
      ))
    } else {
      list(role = m$role, content = list(
        list(type = "text", text = formatted_message$content)
      ))
    }
  })
}

#' Extract rate limit info from  Claude API-Headers
#'
#' @noRd
method(ratelimit_from_header, list(api_claude,new_S3_class("httr2_headers"))) <- function(.api,.headers){
  list(
    this_request_time = strptime(.headers["date"], 
                                 format="%a, %d %b %Y %H:%M:%S", tz="GMT"),
    ratelimit_requests = as.integer(
      .headers["anthropic-ratelimit-requests-limit"]),
    ratelimit_requests_remaining = as.integer(
      .headers["anthropic-ratelimit-requests-remaining"]),
    ratelimit_requests_reset_time = as.POSIXct(
      .headers["anthropic-ratelimit-requests-reset"]$`anthropic-ratelimit-requests-reset`,
      format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
    ratelimit_tokens = as.integer(
      .headers["anthropic-ratelimit-tokens-limit"]),
    ratelimit_tokens_remaining = as.integer(
      .headers["anthropic-ratelimit-tokens-remaining"]),
    ratelimit_tokens_reset_time = as.POSIXct(
      .headers["anthropic-ratelimit-tokens-reset"]$`anthropic-ratelimit-tokens-reset`,
      format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
  )
}

#' A chat parsing method for Openai to extract the assitant response 
#'
#' @noRd
method(parse_chat_response, list(api_claude,class_list)) <- function(.api,.content) {
  api_label <- .api@long_name 
  if("error" %in% names(.content)){
    sprintf("%s returned an Error:\nType: %s\nMessage: %s",
            api_label,
            .content$error$type,
            .content$error$message) |>
      stop()
  }
  
  if (!"content" %in% names(.content) || length(.content$content) == 0) {
    paste0("Received empty response from ",api_label) |>
      stop()
  }
  
  if(r_has_name(.content,"thinking")) return(.content$content[[2]]$text)
  .content$content[[1]]$text
}


#' A function to get metadata from Claude responses
#'
#' @noRd
method(extract_metadata, list(api_claude,class_list))<- function(.api,.response) {
  list(
    model             = .response$model,
    timestamp         = lubridate::as_datetime(lubridate::now()),
    prompt_tokens     = .response$usage$input_tokens,
    completion_tokens = .response$usage$output_tokens,
    total_tokens      = .response$usage$input_tokens + .response$usage$output_tokens,
    stream            = FALSE,
    specific_metadata = list(
      stop_reason    = .response$stop_reason,
      id             = .response$id,
      stop_sequence  = .response$stop_sequence,
      thinking       = if(r_has_name(.response,"thinking")) .response$content[[1]]$thinking else NULL,
      signature      = if(r_has_name(.response,"thinking")) .response$content[[1]]$signature else NULL
    ) 
  )
}  

#' A function to get metadata from Openai streaming responses
#'
#' @noRd
method(extract_metadata_stream, list(api_claude,class_list))<- function(.api,.stream_raw_data) {
  start_message <- .stream_raw_data |> 
    purrr::keep(~.x$type=="message_start") |>
    unlist(recursive = FALSE)
  
  last_message <- .stream_raw_data[[length(.stream_raw_data) - 1]] |> 
    unlist(recursive = FALSE)

  list(
    model             = start_message$message$model,
    timestamp         = lubridate::as_datetime(lubridate::now()),
    prompt_tokens     = start_message$message$usage$input_tokens,
    completion_tokens = last_message$usage.output_tokens,
    total_tokens      = start_message$message$usage$input_tokens + last_message$usage.output_tokens,
    stream            = TRUE,
    specific_metadata = list(warning="Specific Metadata is not yet implemented for Claude streaming requests") 
  )
}  


#Claude-specific method to format tool calls for the API
method(tools_to_api, list(api_claude, class_list)) <- function(.api, .tools) {
  purrr::map(.tools, function(tool) {
    list(
      name = tool@name,
      description = tool@description,
      input_schema = list(
        type = "object",
        properties = purrr::map(tool@input_schema, function(param) {
          list(
            type = param@type,
            description = param@description
          )
        }),
        required = as.list(names(tool@input_schema)) # Assume all are required
      )
    )
  })
}

#' A method to run tool calls on Claude and create the expected response
#'
#' @noRd
method(run_tool_calls, list(api_claude, class_list, class_list)) <- function(.api, .tool_calls, .tools) {
  # Iterate over tool calls
  tool_results <- purrr::map(.tool_calls, function(tool_call) {
    # Extract name, input, and id from the tool call
    tool_name    <- tool_call$name
    tool_args    <- tool_call$input
    tool_call_id <- tool_call$id
    
    # Find the corresponding tool in the tools list
    matching_tool <- purrr::keep(.tools, ~ .x@name == tool_name)
    if (length(matching_tool) == 0) {
      warning(sprintf("No matching tool found for: %s", tool_name))
      return(NULL)
    }
    
    # Get the function for the tool and execute it with the provided arguments
    tool_function <- matching_tool[[1]]@func
    
    tool_result <-  utils::capture.output(
                        do.call(tool_function, as.list(tool_args))
                        , file = NULL) |> 
      stringr::str_c(collapse = "\n")
    
    
    # Return a content block as required by Claude:
    # type "tool_result", the tool_use_id, and the output (as a JSON string)
    list(
      type = "tool_result",
      tool_use_id = tool_call_id,
      content = jsonlite::toJSON(tool_result, auto_unbox = TRUE)
    )
  })
  
  # Remove any NULLs for tool calls that did not have matching tools
  tool_results <- purrr::compact(tool_results)
  
  # Wrap the tool results in a Claude message with role "user"
  list(
    role = "user",
    content = tool_results
  )
}

#' A method to handle streaming requests for Anthropic
#' request
#'
#' @noRd
method(handle_stream,list(api_claude,new_S3_class("httr2_response"))) <- function(.api,.stream_response) {
  stream_text <- ""
  stream_data <- list()
  repeat {
    stream_chunk <- httr2::resp_stream_sse(.stream_response)
    
    # Skip empty chunks
    if (is.null(stream_chunk$data) || !nzchar(stream_chunk$data)) {
      next  
    }
    

    # Try to parse the JSON content
    parsed_event <- tryCatch(
      jsonlite::fromJSON(stream_chunk$data, simplifyVector = FALSE, simplifyDataFrame = FALSE),
      error = function(e) {
        message("Failed to parse JSON: ", e$message)
        return(NULL)
      }
    )
    
    if (!is.null(parsed_event)) {
      stream_data <- append(stream_data,list(parsed_event))
      if (parsed_event$type == "message_stop") {
        close(.stream_response)
        message("\n---------\nStream finished\n---------\n")
        break
      }
      
      delta_content <- parsed_event$delta$text
      if (!is.null(delta_content)) {
          stream_text <- paste0(stream_text, delta_content)
          cat(delta_content)
          utils::flush.console()
        }
      }
  }
  
  list(
    reply = stream_text,
    raw_data = stream_data
  )
}

#' A small helper function to handle schemata or tool requests in claude
#'
#' @noRd
claude_process_tools<- function(.api,
                                .response,
                                .tools_def,
                                .request_body,
                                .request,
                                .timeout,
                                .max_tries){
    if(.response$raw$content$stop_reason == "tool_use"){
      
      tool_calls <- .response$raw$content$content |>
        purrr::keep(~{.x$type=="tool_use"})
      
      #Tool call logic can go here!
      tool_messages <- run_tool_calls(.api,
                                      tool_calls,
                                      .tools_def)
      ##Append the tool call to API
      .request_body$messages <- .request_body$messages |> 
        append(list(list(role="assistant", content = .response$raw$content$content))) |>
        append(list(tool_messages))
      
      .request <- .request |>
        httr2::req_body_json(data = .request_body)
      
      response <- perform_chat_request(.request,.api,FALSE,.timeout,.max_tries)
      return(response)
    }
 return(.response)
}


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
#' @param .json_schema A schema to enforce an output structure
#' @param .api_url Base URL for the Anthropic API (default: "https://api.anthropic.com/").
#' @param .verbose Logical; if TRUE, displays additional information about the API call (default: FALSE).
#' @param .max_tries Maximum retries to peform request
#' @param .timeout Integer specifying the request timeout in seconds (default: 60).
#' @param .stream Logical; if TRUE, streams the response piece by piece (default: FALSE).
#' @param .dry_run Logical; if TRUE, returns the prepared request object without executing it (default: FALSE).
#' @param .thinking Logical; if TRUE, enables Claude's thinking mode for complex reasoning tasks (default: FALSE).
#' @param .thinking_budget Integer specifying the maximum tokens Claude can spend on thinking (default: 1024). Must be at least 1024.
#'
#' @return A new LLMMessage object containing the original messages plus Claude's response.
#' @examples
#' \dontrun{
#' # Basic usage
#' msg <- llm_message("What is R programming?")
#' result <- claude_chat(msg)
#' 
#' # With custom parameters
#' result2 <- claude_chat(msg, 
#'                  .temperature = 0.7, 
#'                  .max_tokens = 1000)
#' }
#'
#' @export
claude_chat <- function(.llm,
                        .model = "claude-3-7-sonnet-20250219",
                        .max_tokens = 2048,
                        .temperature = NULL,
                        .top_k = NULL,
                        .top_p = NULL,
                        .metadata = NULL,
                        .stop_sequences = NULL,
                        .tools = NULL,
                        .json_schema = NULL,
                        .api_url = "https://api.anthropic.com/",
                        .verbose = FALSE,
                        .max_tries = 3,
                        .timeout = 60,
                        .stream = FALSE,
                        .dry_run = FALSE,
                        .thinking = FALSE,
                        .thinking_budget = 1024) {  

  # Validate inputs to the Claude function
  c(
    ".llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
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
    ".tools must be NULL, a TOOL object, or a list of TOOL objects" = is.null(.tools) || S7_inherits(.tools, TOOL) || (is.list(.tools) && all(purrr::map_lgl(.tools, ~ S7_inherits(.x, TOOL)))),
    ".verbose must be logical" = is.logical(.verbose),
    ".stream must be logical" = is.logical(.stream),
    ".max_tries must be integer-valued numeric" = is_integer_valued(.max_tries),
    ".dry_run must be logical" = is.logical(.dry_run),
    "Streaming is not supported for requests with tool calls" = is.null(.tools) || !isTRUE(.stream),
    "For claude, .json_schema is implement a tool use. Only one can be used at a time" = is.null(.tools) || is.null(.json_schema),
    ".json_schema must be NULL or a list or an ellmer type object" = is.null(.json_schema) | is.list(.json_schema) | is_ellmer_type(.json_schema),
    "Streaming is not supported for requests with structured outputs" = is.null(.json_schema) || !isTRUE(.stream),
    ".thinking must be logical" = is.logical(.thinking),
    ".thinking_budget must be a positive integer larger than 1024" = is_integer_valued(.thinking_budget) && .thinking_budget >= 1024
  ) |>
    validate_inputs()
  
  json <- FALSE
  tools_def_schema <- NULL
  if(!is.null(.json_schema)){
    json <- TRUE
    if (requireNamespace("ellmer", quietly = TRUE)) {
      #Handle ellmer json schemata Objects
      if(S7_inherits(.json_schema,ellmer::TypeObject)){
        .json_schema = to_schema(.json_schema)
      }
    }
    tools_def_schema <- list(list(
      name = "claude_json_extractor",
      description = "Formulates a claude answer in a prespecified schema",
      input_schema = .json_schema # Assume all are required
    ))
  }
  
  api_obj <- api_claude(short_name = "claude",
                        long_name  = "Anthropic Claude",
                        api_key_env_var = "ANTHROPIC_API_KEY")
  
  api_key <- get_api_key(api_obj,.dry_run)
  
  # Format message list for Claude model
  messages <- to_api_format(.llm,api_obj)
  
  #Put a single tool into a list if only one is provided. 
  tools_def <- if (!is.null(.tools)) {
    if (S7_inherits(.tools, TOOL))  list(.tools) else .tools
  } else {
    NULL
  }
  
  request_body <- list(
    model = .model,
    max_tokens = .max_tokens,
    messages = messages,
    system = .llm@system_prompt,
    temperature = .temperature,
    top_k = .top_k,
    top_p = .top_p,
    metadata = .metadata,
    stop_sequences = .stop_sequences,
    stream = .stream,
    tools = if(!is.null(tools_def)) tools_to_api(api_obj,tools_def) else tools_def_schema,
    thinking = if(.thinking) list(type = "enabled", budget_tokens = .thinking_budget) else NULL,
    tool_choice =  if(!is.null(.json_schema)) list(type= "tool", name = "claude_json_extractor") else NULL
  ) |> purrr::compact()
  
  # Build request with httr2
  request <- httr2::request(.api_url) |>
    httr2::req_url_path("/v1/messages") |>
    httr2::req_headers(
      `x-api-key` = Sys.getenv("ANTHROPIC_API_KEY"),
      `anthropic-version` = "2023-06-01",
      `content-type` = "application/json; charset=utf-8",
      .redact = "x-api-key"
    ) |>
    httr2::req_body_json(data = request_body)  
  
  # Return only the request object 
  if (.dry_run) {
    return(request)  
  }
  
  response <- perform_chat_request(request,api_obj,.stream,.timeout)
  if(.stream == FALSE & is.null(.json_schema)){
  response <- claude_process_tools(.api=api_obj,
                                  .response=response,
                                  .tools_def=tools_def,
                                  .request_body=request_body,
                                  .request=request,
                                  .timeout=.timeout,
                                  .max_tries=.max_tries)
  }
  
  #Build the assistant reply for structured outputs
  if(!is.null(.json_schema)){
    #Write the assistant reply as json to be consistent with the output from other APIs
    assistant_reply <-  response$raw$content$content[[1]]$input |> 
      jsonlite::toJSON(auto_unbox = TRUE,pretty = TRUE)
  } else {
    assistant_reply <- response$assistant_reply
  }
    
  # Extract the assistant reply and headers from response
  track_rate_limit(api_obj,response$headers,.verbose)
  
  # Return the updated LLMMessage object
  add_message(.llm     = .llm, 
              .role    = "assistant", 
              .content = assistant_reply, 
              .json    = json,
              .meta    = response$meta)
}

#' Send a Batch of Messages to Claude API
#'
#' This function creates and submits a batch of messages to the Claude API for asynchronous processing.
#'
#' @param .llms A list of LLMMessage objects containing conversation histories.
#' @param .model Character string specifying the Claude model version (default: "claude-3-5-sonnet-20241022").
#' @param .max_tokens Integer specifying the maximum tokens per response (default: 1024).
#' @param .temperature Numeric between 0 and 1 controlling response randomness.
#' @param .top_k Integer for diversity by limiting the top K tokens.
#' @param .top_p Numeric between 0 and 1 for nucleus sampling.
#' @param .stop_sequences Character vector of sequences that halt response generation.
#' @param .api_url Base URL for the Claude API (default: "https://api.anthropic.com/").
#' @param .verbose Logical; if TRUE, prints a message with the batch ID (default: FALSE).
#' @param .overwrite Logical; if TRUE, allows overwriting an existing batch ID associated with the request (default: FALSE).
#' @param .max_tries Maximum number of retries to perform the request.
#' @param .timeout Integer specifying the request timeout in seconds (default: 60).
#' @param .dry_run Logical; if TRUE, returns the prepared request object without executing it (default: FALSE).
#' @param .id_prefix Character string to specify a prefix for generating custom IDs when names in `.llms` are missing.
#' @param .json_schema A schema to enforce an output structure
#' @param .thinking Logical; if TRUE, enables Claude's thinking mode for complex reasoning tasks (default: FALSE).
#' @param .thinking_budget Integer specifying the maximum tokens Claude can spend on thinking (default: 1024). Must be at least 1024.
#'
#'   Defaults to "tidyllm_claude_req_".
#' 
#' @return An updated and named list of `.llms` with identifiers that align with batch responses, including a `batch_id` attribute.
#' @export
send_claude_batch <- function(.llms, 
                              .model = "claude-3-5-sonnet-20241022", 
                              .max_tokens = 1024, 
                              .temperature = NULL, 
                              .top_k = NULL, 
                              .top_p = NULL, 
                              .stop_sequences = NULL, 
                              .json_schema = NULL,
                              .thinking = FALSE,
                              .thinking_budget = 1024,
                              .api_url = "https://api.anthropic.com/", 
                              .verbose = FALSE,
                              .dry_run = FALSE,
                              .overwrite = FALSE,
                              .max_tries = 3,
                              .timeout = 60,
                              .id_prefix = "tidyllm_claude_req_"
                              ) {
  # Input validation
  c(
    ".llms must be a list of LLMMessage objects" = is.list(.llms) && all(sapply(.llms, S7_inherits, LLMMessage)),
    ".max_tokens must be an integer" = is_integer_valued(.max_tokens),
    ".temperature must be numeric between 0 and 1 if provided" = is.null(.temperature) || (.temperature >= 0 && .temperature <= 1),
    ".top_k must be a positive integer if provided" = is.null(.top_k) || (is.numeric(.top_k) && .top_k > 0 && floor(.top_k) == .top_k),
    ".top_p must be numeric between 0 and 1 if provided" = is.null(.top_p) || (.top_p >= 0 && .top_p <= 1),
    "Only one of .temperature or .top_p should be specified" = is.null(.temperature) || is.null(.top_p),
    ".stop_sequences must be a character vector" = is.null(.stop_sequences) || is.character(.stop_sequences),
    ".verbose must be logical" = is.logical(.verbose),
    ".dry_run must be logical" = is.logical(.dry_run),
    ".overwrite must be logical" = is.logical(.overwrite),
    ".json_schema must be NULL or a list or an ellmer type object" = is.null(.json_schema) | is.list(.json_schema) | is_ellmer_type(.json_schema),
    ".id_prefix must be a character vector of length 1" = is.character(.id_prefix),
    ".max_tries must be integer-valued numeric" = is_integer_valued(.max_tries),
    ".timeout must be an integer" = is_integer_valued(.timeout)
  ) |> validate_inputs()
  
  
  api_obj <- api_claude(short_name = "claude",
                        long_name  = "Anthropic Claude",
                        api_key_env_var = "ANTHROPIC_API_KEY")
  
  api_key <- get_api_key(api_obj,.dry_run)
  
  prepared_llms  <- prepare_llms_for_batch(api_obj,
                                           .llms=.llms,
                                           .id_prefix=.id_prefix,
                                           .overwrite = .overwrite)
  
  json <- FALSE
  if(!is.null(.json_schema)){
    json <- TRUE
    if (requireNamespace("ellmer", quietly = TRUE)) {
      #Handle ellmer json schemata Objects
      if(S7_inherits(.json_schema,ellmer::TypeObject)){
        .json_schema = to_schema(.json_schema)
      }
    }
    tools_def_schema <- list(list(
      name = "claude_json_extractor",
      description = "Formulates a claude answer in a prespecified schema",
      input_schema = .json_schema # Assume all are required
    ))
  }
  
  requests_list <- lapply(seq_along(prepared_llms), function(i) { 
    
    # Get messages from each LLMMessage object
    messages <- to_api_format(.llms[[i]],api_obj)
    
    custom_id <- names(prepared_llms)[i]
    list(
      custom_id = custom_id,
      params = list(
        model = .model,
        max_tokens = .max_tokens,
        messages = messages,
        temperature = .temperature,
        top_k = .top_k,
        top_p = .top_p,
        system = .llms[[i]]@system_prompt,
        stop_sequences = .stop_sequences,
        thinking = if(.thinking) list(type = "enabled", budget_tokens = .thinking_budget) else NULL,
        tools = if(!is.null(.json_schema)) tools_def_schema else NULL,
        tool_choice =  if(!is.null(.json_schema)) list(type= "tool", name = "claude_json_extractor") else NULL
        
      ) |> purrr::compact()  # Remove NULL values
    )
  })
  
  request <- httr2::request(.api_url) |>
    httr2::req_url_path("/v1/messages/batches") |>
    httr2::req_headers(
      `x-api-key` = api_key,
      `anthropic-version` = "2023-06-01",
      `anthropic-beta` = "message-batches-2024-09-24",
      `content-type` = "application/json; charset=utf-8",
      .redact = "x-api-key"
    ) |>
    httr2::req_body_json(list(requests = requests_list))
  
  if (.dry_run) {
    return(request)
  }
  
  response <- request |>
    perform_generic_request(.timeout=.timeout,
                            .max_tries = .max_tries)
  

  if("error" %in% names(response$content)){
    sprintf("Anthropic API returned an Error:\nType: %s\nMessage: %s",
            response$content$error$type,
            response$content$error$message) |>
      stop()
  }
  
  # Attach batch_id as an attribute to .llms
  batch_id <- response$content$id
  attr(prepared_llms, "batch_id") <- batch_id
  attr(prepared_llms, "json") <- json
  
  
  if (.verbose) {
    message("Batch submitted successfully. Batch ID: ", batch_id)
  }
  
  return(prepared_llms)
}


#' Check Batch Processing Status for Claude API
#'
#' This function retrieves the processing status and other details of a specified Claude batch ID
#' from the Claude API.
#'
#' @param .llms A list of LLMMessage objects
#' @param .batch_id A manually set batchid
#' @param .api_url Character; base URL of the Claude API (default: "https://api.anthropic.com/").
#' @param .max_tries Maximum retries to peform request
#' @param .timeout Integer specifying the request timeout in seconds (default: 60).
#' @param .dry_run Logical; if TRUE, returns the prepared request object without executing it (default: FALSE).
#' @return A tibble with information about the status of batch processing 
#' @export
check_claude_batch <- function(.llms = NULL, 
                               .batch_id = NULL, 
                               .api_url = "https://api.anthropic.com/",
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
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if ((api_key == "") & !.dry_run){
    stop("API key is not set.")
  }
  
  # Build request
  request_url <- paste0(.api_url, "v1/messages/batches/", .batch_id)
  request <- httr2::request(request_url) |>
    httr2::req_headers(
      `x-api-key` = api_key,
      `anthropic-version` = "2023-06-01",
      `anthropic-beta` = "message-batches-2024-09-24",
      .redact = "x-api-key"
    )
  
  # Perform request with retries and error handling
  response <- request  |>
    perform_generic_request(.timeout=.timeout,
                            .max_tries = .max_tries)
  
  # Parse response
  response_body <- response$content
  if("error" %in% names(response_body)){
    sprintf("Anthropic API returned an Error:\nType: %s\nMessage: %s",
            response_body$error$type,
            response_body$error$message) |>
      stop()
  }
  
  # Create tibble with batch details
  tibble::tibble(
    batch_id = response_body$id,
    status = response_body$processing_status,
    created_at = lubridate::ymd_hms(response_body$created_at, tz = "UTC"),
    expires_at = lubridate::ymd_hms(response_body$expires_at, tz = "UTC"),
    req_succeeded  = response_body$request_counts$succeeded,
    req_errored    = response_body$request_counts$errored,
    req_expired    = response_body$request_counts$expired,
    req_canceled   = response_body$request_counts$canceled
  )
}


#' Fetch Results for a Claude Batch
#'
#' This function retrieves the results of a completed Claude batch and updates
#' the provided list of `LLMMessage` objects with the responses. It aligns each
#' response with the original request using the `custom_id`s generated in `send_claude_batch()`.
#'
#' @param .llms A list of `LLMMessage` objects that were part of the batch. The list should
#'              have names (custom IDs) set by `send_claude_batch()` to ensure correct alignment.
#' @param .batch_id Character; the unique identifier for the batch. By default this is NULL
#'                  and the function will attempt to use the `batch_id` attribute from `.llms`.
#' @param .api_url Character; the base URL for the Claude API (default: "https://api.anthropic.com/").
#' @param .dry_run Logical; if `TRUE`, returns the constructed request without executing it (default: `FALSE`).
#' @param .max_tries Integer; maximum number of retries if the request fails (default: `3`).
#' @param .timeout Integer; request timeout in seconds (default: `60`).
#'
#' @return A list of updated `LLMMessage` objects, each with the assistant's response added if successful.
#' @export
fetch_claude_batch <- function(.llms, 
                               .batch_id = NULL, 
                               .api_url = "https://api.anthropic.com/",
                               .dry_run = FALSE,
                               .max_tries = 3,
                               .timeout = 60) {
  c(
    ".llms must be a list of LLMMessage objects with names as custom IDs" = is.list(.llms) && all(sapply(.llms, S7_inherits, LLMMessage)),
    ".batch_id must be a non-empty character string or NULL" = is.null(.batch_id) || (is.character(.batch_id) && nzchar(.batch_id)),
    ".api_url must be a non-empty character string" = is.character(.api_url) && nzchar(.api_url),
    ".dry_run must be logical" = is.logical(.dry_run),
    ".max_tries must be an integer" = is_integer_valued(.max_tries),
    ".timeout must be an integer" = is_integer_valued(.timeout)
  ) |> validate_inputs()
  
  # Preserve original names
  original_names <- names(.llms)
  if(!is.null(attr(.llms,"json"))) json <- attr(.llms,"json") else json <- FALSE
  
  # Retrieve batch_id from .llms if not provided
  if (is.null(.batch_id)) {
    .batch_id <- attr(.llms, "batch_id")
    if (is.null(.batch_id)) {
      stop("No batch_id provided and no batch_id attribute found in the provided list.")
    }
  }
  
  api_obj <- api_claude(short_name = "claude",
                        long_name  = "Anthropic Claude",
                        api_key_env_var = "ANTHROPIC_API_KEY")
  
  api_key <- get_api_key(api_obj,.dry_run)
  
  # Construct request URL to get batch details
  batch_details_url <- paste0(.api_url, "v1/messages/batches/", .batch_id)
  
  request <- httr2::request(batch_details_url) |>
    httr2::req_headers(
      `x-api-key` = api_key,
      `anthropic-version` = "2023-06-01",
      `anthropic-beta` = "message-batches-2024-09-24",
      .redact = "x-api-key"
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
    sprintf("Anthropic API returned an Error:\nType: %s\nMessage: %s",
            response_body$error$type,
            response_body$error$message) |>
      stop()  
  }
  
  # Check if batch has completed processing
  if (response_body$processing_status != "ended") {
    stop("Batch processing has not ended yet. Please check again later.")
  }
  
  # Retrieve the results from results_url
  results_url <- response_body$results_url
  results_request <- httr2::request(results_url) |>
    httr2::req_headers(
      `x-api-key` = api_key,
      `anthropic-version` = "2023-06-01",
      `anthropic-beta` = "message-batches-2024-09-24",
      .redact = "x-api-key"
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
    
    if (!is.null(result) && result$result$type == "succeeded") {
      if(json==FALSE){
        assistant_reply <- result$result$message$content$text
      }
      if(json==TRUE){
        assistant_reply <-  result$result$message$content$input |> 
          jsonlite::toJSON(auto_unbox = TRUE,pretty = TRUE)
      }
      llm <- add_message(.llm = .llms[[custom_id]],
                         .role = "assistant", 
                         .content = assistant_reply,
                         .json = json,
                         .meta = extract_metadata(api_obj,result$result$message))
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

#' List Claude Batch Requests
#'
#' Retrieves batch request details from the Claude API.
#'
#' @param .api_url Base URL for the Claude API (default: "https://api.anthropic.com/").
#' @param .limit Maximum number of batches to retrieve (default: 20).
#' @param .max_tries Maximum retry attempts for requests (default: 3).
#' @param .timeout Request timeout in seconds (default: 60).
#'
#' @return A tibble with batch details: batch ID, status, creation time, expiration time, 
#' and request counts (succeeded, errored, expired, canceled).
#'
#' @export
list_claude_batches <- function(.api_url = "https://api.anthropic.com/", 
                                .limit = 20, 
                                .max_tries = 3,
                                .timeout = 60) {
  # Retrieve API key
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (api_key == "") {
    stop("API key is not set. Please set it with: Sys.setenv(ANTHROPIC_API_KEY = \"YOUR-KEY-GOES-HERE\").")
  }
  
  # Set up request URL with query parameters
  request <- httr2::request(.api_url) |>
    httr2::req_url_path("/v1/messages/batches") |>
    httr2::req_headers(
      `x-api-key` = api_key,
      `anthropic-version` = "2023-06-01",
      `anthropic-beta` = "message-batches-2024-09-24",
      .redact = "x-api-key"
    ) |>
    httr2::req_url_query(limit = .limit)
  
  # Perform the request with retries and error handling
  response <- request |>
    perform_generic_request(.timeout=.timeout,
                            .max_tries = .max_tries)
  
  # Parse response and handle any errors
  if ("error" %in% names(response$content)) {
    stop(sprintf("Anthropic API Error: %s - %s", response$content$error$type, response$content$error$message))
  }
  
  # Extract batch list details with parsed dates
  batch_list <- purrr::map_dfr(response$content$data, function(batch) {
    tibble::tibble(
      batch_id = batch$id,
      status = batch$processing_status,
      created_at = lubridate::ymd_hms(batch$created_at, tz = "UTC"),
      expires_at = lubridate::ymd_hms(batch$expires_at, tz = "UTC"),
      req_succeeded = batch$request_counts$succeeded,
      req_errored = batch$request_counts$errored,
      req_expired = batch$request_counts$expired,
      req_canceled = batch$request_counts$canceled
    )
  })
  

  return(batch_list)
}

#' List Available Models from the Anthropic Claude API
#'
#' @param .api_url Base URL for the API (default: "https://api.anthropic.com").
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .max_tries Maximum number of retries for the API request (default: 3).
#' @param .dry_run Logical; if TRUE, returns the prepared request object without executing it.
#' @param .verbose Logical; if TRUE, prints additional information about the request.
#'
#' @return A tibble containing model information (columns include `type`,`id`, `display_name`, and `created_at`),
#'   or NULL if no models are found.
#'
#' @export
claude_list_models <- function(.api_url = "https://api.anthropic.com",
                               .timeout = 60,
                               .max_tries = 3,
                               .dry_run = FALSE,
                               .verbose = FALSE) {

  # Create an API object for Claude using the tidyllm helper
  api_obj <- api_claude(short_name = "claude",
                        long_name  = "Anthropic Claude",
                        api_key_env_var = "ANTHROPIC_API_KEY")
  
  # Retrieve the API key (will error if not set, unless in dry run mode)
  api_key <- get_api_key(api_obj, .dry_run)
  
  # Build the request to the /v1/models endpoint
  request <- httr2::request(.api_url) |>
    httr2::req_url_path("/v1/models") |>
    httr2::req_headers(
      `x-api-key` = api_key,
      `anthropic-version` = "2023-06-01",
      `content-type` = "application/json; charset=utf-8"
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
    message("Retrieved response from Anthropic Claude: ", response$object)
  }
  
  # Check if the "data" field exists and contains models
  if (!is.null(response$data)) {
    models <- response$data
    
    # Create a tibble with selected model information
    model_info <- models |> 
      purrr::map_dfr(tibble::as_tibble)
    
    return(model_info)
  } else {
    return(NULL)
  }
}

#' Provider Function for Claude models on the Anthropic API
#'
#' The `claude()` function acts as an interface for interacting with the Anthropic API 
#' through main `tidyllm` verbs such as `chat()`, `embed()`, and 
#' `send_batch()`. It dynamically routes requests to Claude-specific functions 
#' like `claude_chat()` and `send_claude_batch()` based on the context of the call.
#'
#' @param ... Parameters to be passed to the appropriate OpenAI-specific function, 
#'   such as model configuration, input text, or API-specific options.
#' @param .called_from An internal argument that specifies which action (e.g., 
#'   `chat`, `send_batch`) the function is being invoked from. 
#'   This argument is automatically managed and should not be modified by the user.
#'
#' @return The result of the requested action, depending on the specific function invoked 
#'   (e.g., an updated `LLMMessage` object for `chat()`, or a matrix for `embed()`).
#' 
#' @export
claude <- create_provider_function(
  .name = "claude",
  chat = claude_chat,
  send_batch = send_claude_batch,
  check_batch = check_claude_batch,
  list_batches = list_claude_batches,
  fetch_batch = fetch_claude_batch,
  list_models = claude_list_models
)




#' Interact with local AI models via the Ollama API
#'
#'
#' @param .llm An LLMMessage object containing the conversation history and system prompt.
#' @param .model Character string specifying the Ollama model to use (default: "gemma2")
#' @param .stream Logical; whether to stream the response (default: FALSE)
#' @param .seed Integer; seed for reproducible generation (default: NULL)
#' @param .json Logical; whether to format response as JSON (default: FALSE)
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
#' response <- ollama(llm, .model = "gemma2", .temperature = 0.7)
#' 
#' # With custom parameters
#' response <- ollama(
#'   llm,
#'   .model = "llama2",
#'   .temperature = 0.8,
#'   .top_p = 0.9,
#'   .num_ctx = 4096
#' )
#' }
#'
#' @export
ollama <- function(.llm,
                   .model = "gemma2",
                   .stream = FALSE,
                   .seed = NULL,
                   .json = FALSE,
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
    "Input .llm must be an LLMMessage object" = inherits(.llm, "LLMMessage"),
    "Input .model must be a string" = is.character(.model),
    "Input .stream must be logical if provided" = is.logical(.stream),
    "Input .json must be logical if provided" = is.logical(.json),
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
  
  # Get formatted message list for ollama models
  ollama_messages <- .llm$to_api_format("ollama")
  
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
  )
  
  ollama_options <- base::Filter(Negate(is.null), ollama_options)
  
  ollama_request_body <- list(
    model = .model,
    messages = ollama_messages,
    options = ollama_options,
    stream = .stream
  )
  
  # Add keep_alive to request body only if it's provided
  if (!is.null(.keep_alive)) {
    ollama_request_body$keep_alive <- .keep_alive
  }
  
  # Add format to request body if applicable
  if (.json == TRUE) {
    ollama_request_body$format <- "json"
  }
  
  # Build the request
  request <- httr2::request(.ollama_server) |>
    httr2::req_url_path("/api/chat") |>
    httr2::req_body_json(ollama_request_body)
  
  # Perform the API request
  response <- perform_api_request(
    .request = request,
    .api = "ollama",
    .stream = .stream,
    .timeout = .timeout,
    .parse_response_fn = function(body_json) {
      if ("error" %in% names(body_json)) {
        stop(sprintf("Error response from Ollama API: %s", body_json$error))
      }  
      assistant_reply <- body_json$message$content
      return(assistant_reply)
    },
    .dry_run = .dry_run
  )
  
  # Return only the request object in a dry run.
  if (.dry_run) {
    return(response)  
  }
  
  assistant_reply <- response$assistant_reply
  
  # Create a deep copy of the LLMMessage object
  llm_copy <- .llm$clone_deep()
  
  # Add model's message to the history of the LLMMessage object
  llm_copy$add_message("assistant", assistant_reply,json=.json)
  
  return(llm_copy)
}


#' Generate Embeddings Using Ollama API
#'
#' @param .llm An existing LLMMessage object (or a charachter vector of texts to embed)
#' @param .model The embedding model identifier (default: "all-minilm").
#' @param .truncate Whether to truncate inputs to fit the model's context length (default: TRUE).
#' @param .ollama_server The URL of the Ollama server to be used (default: "http://localhost:11434").
#' @param .timeout Timeout for the API request in seconds (default: 120).
#' @param .dry_run If TRUE, perform a dry run and return the request object.
#' @return A matrix where each column corresponds to the embedding of a message in the message history.
#' @export
ollama_embedding <- function(.llm,
                             .model = "all-minilm",
                             .truncate = TRUE,
                             .ollama_server = "http://localhost:11434",
                             .timeout = 120,
                             .dry_run=FALSE) {
  
  # Validate the inputs
  c(
    "Input .llm must be an LLMMessage object or a character vector" = inherits(.llm, "LLMMessage") | is.character(.llm),
    "Input .model must be a string" = is.character(.model),
    "Input .truncate must be logical" = is.logical(.truncate),
    "Input .timeout must be an integer-valued numeric (seconds till timeout)" = is.numeric(.timeout) && .timeout > 0,
    ".dry_run must be logical"                   = is.logical(.dry_run)
  ) |> validate_inputs()
  
  if(!is.character(.llm)){
    ollama_history <- Filter(function(x){
      if ("role" %in% names(x)) {
        return(x$role %in% c("user","assistant"))
      } else {
        return(FALSE)
      }},.llm$message_history)
    
    # Extract messages and combine content and text media
    message_texts <- lapply(ollama_history, function(m) {
      # The basic text content supplied with the message
      base_content <- m$content
      
      # Get the relevant media for the current message
      media_list <- m$media
      
      # Extract the text content from media
      text_media <- extract_media(media_list, "text")
      text_media_combined <- paste(unlist(text_media), collapse = " ")
      
      # Combine base content and text media
      combined_text <- paste(base_content, text_media_combined, sep = " ")
      combined_text
    })
  }
  
  if(is.character(.llm)){message_texts <- .llm}
  
  # Prepare the request body
  request_body <- list(
    model = .model,
    input = message_texts,
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
  
  # Perform the API request
  response <- request |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_timeout(.timeout) |>
    httr2::req_perform()
  
  # Check for API errors
  tryCatch({
    # Check for HTTP errors
    if (httr2::resp_is_error(response)) {
      # Try to parse the JSON response body
      error_message <- tryCatch({
        json_content <- httr2::resp_body_json(response)
        if (!is.null(json_content)) {
          paste0("API error response - ", json_content$error)
        } else {
          "Unknown error occurred"
        }
      }, error = function(e) {
        paste("HTTP error:", httr2::resp_status(response), "- Unable to parse error message")
      })
      
      stop(error_message)
    }    
    # Parse the response and extract embeddings
    response_content <- httr2::resp_body_json(response)
    embeddings <- response_content$embeddings
    
    # Check if embeddings are present
    if (is.null(embeddings)) {
      stop("No embeddings returned in the response.")
    }
    
    embeddings <- purrr::map(embeddings,~purrr::flatten_dbl(.x))
    embedding_matrix <- do.call(cbind, embeddings)
    
    # Return the embeddings
    return(embedding_matrix)
    
  }, error = function(e) {
    stop("An error occurred during the API request - ", e$message)
  })
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
      quantization_level = sapply(models, function(model) model$details$quantization_level),
      stringsAsFactors = FALSE
    )
    
    return(model_info)
    
  } else {
    # Return NULL if no models are found
    return(NULL)
  }
}


#' Download a model from the Ollama API
#'
#' This function sends a request to the Ollama API to download a specified model.
#' It can operate in a streaming mode where it provides live updates of the download status
#' and progress, or a single response mode.
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


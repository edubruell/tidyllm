
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


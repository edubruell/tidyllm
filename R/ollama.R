#' Retrieve and print model information from the Ollama API
#' 
#' This function connects to the Ollama API and retrieves and print information 
#' about available models.
#' @return NULL
#' @export
ollama_list_models <- function(.ollama_server = "http://localhost:11434") {

  #Perform the request and save the response
  response <- httr2::request(.ollama_server) |>
    httr2::req_url_path("/api/tags") |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  
  #What models are available
  models <-  response$models
  
  # Check if 'models' key exists in the response
  if (!is.null(response$models)) {
      lapply(models, function(model){
        cat("Model Name:",model$name,"\n")
        cat("Modified At:", model$modified_at, "\n")
        cat("Size:", model$size, "bytes\n")
        cat("Format:", model$details$format, "\n")
        cat("Family:", model$details$family, "\n")
        cat("Parameter Size:", model$details$parameter_size, "\n")
        cat("Quantization Level:", model$details$quantization_level, "\n")
        cat("--------------------------------------------------\n")
      })
    } else {
      cat("No model information found in the response.\n")
    }
  invisible(NULL)
}

#' Download a model from the Ollama API
#'
#' This function sends a request to the Ollama API to download a specified model.
#' It can operate in a streaming mode where it provides live updates of the download status
#' and progress, or a single response mode.
#'
#' @param model_name The name of the model to download.
#' @param stream Boolean indicating whether to stream the download progress or not (default TRUE).
#' @param .ollama_server The base URL of the Ollama API (default is "http://localhost:11434").
#' @return NULL
#' @export
ollama_download_model <- function(.model, .ollama_server = "http://localhost:11434") {
  
  # Initialize a progress bar
 progress_bar <- cli::cli_progress_bar(auto_terminate = FALSE,type="download")
  
  # Prepare the callback function for handling stream data
  callback_modeldownload_stream <- function(.stream) {
    browser()
    stream_content <- rawToChar(.stream, multiple = FALSE) |>
      jsonlite::fromJSON()
    #Show a progress bar for the download 
    if(stringr::str_detect(stream_content$status,"pulling")){
        if (!is.null(stream_content$total) && stream_content$total > 0) {
        #Update the progress bar
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
    httr2::req_perform_stream(callback_modeldownload_stream, buffer_kb = 0.05, round="line")
  
  cli::cli_progress_done(id = progress_bar)
  invisible(NULL)
}

#A minimal test function to test how streaming works with httr2::httr2::req_perform_stream()
ollama_test_streaming <- function(.model,
                   .prompt,
                   .stream =TRUE){
  
  
  #Build the request
  ollama_api <- httr2::request("http://localhost:11434/") |>
    httr2::req_url_path("/api/generate") 
  
  #Do we want to stream the response and show the message when single bits arrive?
  if(.stream==TRUE){
    #Initialize the streaming environment variable
    .tidyllm_stream_env$stream <- ""
    
    #A callback function to stream responses from ollama 
    callback_ollama_stream <- function(.stream){
      stream_content <- rawToChar(.stream, multiple = FALSE) |> 
        jsonlite::fromJSON()
      
      stream_response <- stream_content$response 
      .tidyllm_stream_env$stream <- glue::glue("{.tidyllm_stream_env$stream}{stream_response}") |> as.character()
      cat(stream_response)
      flush.console()
      TRUE
    }
    ollama_api |>
      httr2::req_body_json(list(model = .model, prompt = .prompt)) |> 
      httr2::req_perform_stream(callback_ollama_stream, buffer_kb = 0.05, round="line")
    
    resp <- .tidyllm_stream_env$stream
    return(resp)
  }
  
  if(.stream==FALSE){
    resp <- ollama_api |>
      httr2::req_body_json(list(model = .model, prompt = .prompt, stream=FALSE)) |> 
      httr2::req_perform() |> 
      httr2::resp_body_json()
    
    return(resp$response)
  }
}




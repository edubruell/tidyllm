#' Retrieve and print model information from the Ollama API
#' 
#' This function connects to the Ollama API and retrieves information about available models.
#' 
#' @return NULL
#' @export
ollama_list_models <- function(.ollama_server = "http://localhost:11434") {
  #Ususally the default API is assumed
  ollama_api <- glue::glue("{.ollama_server}/api/tags")
  
  # Send GET request to the API
  response <- httr::GET(ollama_api)
  
  # Check if request was successful
  if (httr::http_type(response) == "application/json") {
    # Parse JSON response
    models <-  httr::content(response, "parsed")
    
    # Check if 'models' key exists in the response
    if ("models" %in% names(models)) {
      # Extract model information
      model_info <- models$models
      
      # Print model info
      for (model in model_info) {
        cat("Model Name:", model$name, "\n")
        cat("Modified At:", model$modified_at, "\n")
        cat("Size:", model$size, "bytes\n")
        cat("Digest:", model$digest, "\n")
        cat("Format:", model$details$format, "\n")
        cat("Family:", model$details$family, "\n")
        cat("Parameter Size:", model$details$parameter_size, "\n")
        cat("Quantization Level:", model$details$quantization_level, "\n\n")
        cat("--------------------------------------------------\n")
      }
    } else {
      cat("No model information found in the response.\n")
    }
  } else {
    cat("Error: Failed to retrieve data from the API.\n")
  }
}

ollama_list_models()

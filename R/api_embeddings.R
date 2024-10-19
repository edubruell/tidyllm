
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
  
  # Check for HTTP errors
  if (httr2::resp_is_error(response)) {
    stop("HTTP error: ", httr2::resp_status(response))
  }
  
  # Parse the response
  response_content <- httr2::resp_body_json(response)
  
  # Check for errors in the response
  if (!is.null(response_content$error)) {
    stop("API error: ", response_content$error)
  }
  
  # Get the embeddings
  embeddings <- response_content$embeddings
  
  # Check if embeddings are present
  if (is.null(embeddings)) {
    stop("No embeddings returned in the response.")
  }
  
  embeddings <- purrr::map(embeddings,~purrr::flatten_dbl(.x))
  
  # Convert embeddings to a matrix
  embedding_matrix <- do.call(cbind, embeddings) 
  
  # Return the embeddings
  return(embedding_matrix)
}

#' Generate Embeddings Using OpenAI API
#'
#' @param .llm An existing LLMMessage object (or a character vector of texts to embed)
#' @param .model The embedding model identifier (default: "text-embedding-3-small").
#' @param .truncate Whether to truncate inputs to fit the model's context length (default: TRUE).
#' @param .openai_api_key Your OpenAI API key for authentication.
#' @param .timeout Timeout for the API request in seconds (default: 120).
#' @param .dry_run If TRUE, perform a dry run and return the request object.
#' @return A matrix where each column corresponds to the embedding of a message in the message history.
#' @export
openai_embedding <- function(.llm,
                             .model = "text-embedding-3-small",
                             .truncate = TRUE,
                             .openai_api_key = Sys.getenv("OPENAI_API_KEY"),
                             .timeout = 120,
                             .dry_run = FALSE) {
  
  # Validate the inputs
  c(
    "Input .llm must be an LLMMessage object or a character vector" = inherits(.llm, "LLMMessage") | is.character(.llm),
    "Input .model must be a string" = is.character(.model),
    "Input .truncate must be logical" = is.logical(.truncate),
    "Input .timeout must be an integer-valued numeric (seconds till timeout)" = is.numeric(.timeout) && .timeout > 0,
    ".dry_run must be logical" = is.logical(.dry_run),
    ".openai_api_key must be provided" = nzchar(.openai_api_key)
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
  
  # Build the request
  request <- httr2::request("https://api.openai.com/v1/embeddings") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", .openai_api_key)
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
    httr2::req_perform()
  
  # Check for HTTP errors
  if (httr2::resp_is_error(response)) {
    stop("HTTP error: ", httr2::resp_status(response))
  }
  
  # Parse the response
  response_content <- httr2::resp_body_json(response)
  
  # Check for API errors
  if (!is.null(response_content$error)) {
    stop("API error: ", response_content$error$message)
  }
  
  # Extract the embeddings
  embeddings <- response_content$data |> purrr::map("embedding")
  
  # Convert embeddings to a matrix
  embedding_matrix <- do.call(cbind, embeddings)
  
  # Return the embeddings
  return(embedding_matrix)
}

#' Generate Embeddings Using Mistral API
#'
#' @param .llm An existing LLMMessage object (or a character vector of texts to embed)
#' @param .model The embedding model identifier (default: "mistral-7b").
#' @param .mistral_api_key Your Mistral API key for authentication.
#' @param .timeout Timeout for the API request in seconds (default: 120).
#' @param .dry_run If TRUE, perform a dry run and return the request object.
#' @return A matrix where each column corresponds to the embedding of a message in the message history.
#' @export
mistral_embedding <- function(.llm,
                              .model = "mistral-7b",
                              .mistral_api_key = Sys.getenv("MISTRAL_API_KEY"),
                              .timeout = 120,
                              .dry_run = FALSE) {
  
  # Validate the inputs
  c(
    "Input .llm must be an LLMMessage object or a character vector" = inherits(.llm, "LLMMessage") | is.character(.llm),
    "Input .model must be a string" = is.character(.model),
    "Input .timeout must be an integer-valued numeric (seconds till timeout)" = is.numeric(.timeout) && .timeout > 0,
    ".dry_run must be logical" = is.logical(.dry_run),
    ".mistral_api_key must be provided" = nzchar(.mistral_api_key)
  ) |> validate_inputs()
  
  if (!is.character(.llm)) {
    mistral_history <- Filter(function(x) {
      if ("role" %in% names(x)) {
        return(x$role %in% c("user", "assistant"))
      } else {
        return(FALSE)
      }
    }, .llm$message_history)
    
    # Extract messages and combine content and text media
    message_texts <- lapply(mistral_history, function(m) {
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
  
  # Build the request
  request <- httr2::request("https://api.mistral.ai/v1/embeddings") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", .mistral_api_key)
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
    httr2::req_perform()
  
  # Check for HTTP errors
  if (httr2::resp_is_error(response)) {
    stop("HTTP error: ", httr2::resp_status(response))
  }
  
  # Parse the response
  response_content <- httr2::resp_body_json(response)
  
  # Check for API errors
  if (!is.null(response_content$error)) {
    stop("API error: ", response_content$error$message)
  }
  
  # Extract the embeddings
  embeddings <- response_content$data |> purrr::map("embedding")
  
  # Convert embeddings to a matrix
  embedding_matrix <- do.call(cbind, embeddings)
  
  # Return the embeddings
  return(embedding_matrix)
}



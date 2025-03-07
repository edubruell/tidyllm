

#' Generate Embeddings Using Voyage AI API
#'
#' This function creates embedding vectors from text or multimodal inputs (text and images)
#' using the Voyage AI API. It supports three types of input:
#' 
#' 1. Character vector: Embeds each text string separately
#' 2. LLMMessage object: Extracts and embeds text content from messages
#' 3. List of mixed content: Processes a combination of text strings and image objects created with `img()`
#'
#' For multimodal inputs, the function automatically switches to Voyage's multimodal API
#' and formats the response with appropriate labels (e.g., \code{"[IMG] image.png"}) for images.
#'
#' @param .input Input to embed. Can be:
#'   - A character vector of texts
#'   - An `LLMMessage` object (all textual components will be embedded)
#'   - A list containing a mix of character strings and `tidyllm_image` objects created with `img()`
#' @param .model The embedding model identifier. For text-only: "voyage-3" (default).
#'               For multimodal inputs: "voyage-multimodal-3" is used automatically.
#' @param .timeout Timeout for the API request in seconds (default: 120).
#' @param .dry_run If TRUE, perform a dry run and return the request object without sending.
#' @param .max_tries Maximum retry attempts for requests (default: 3).
#' @param .verbose Should information about current rate limits be printed? (default: FALSE).
#'
#' @return A tibble with two columns: `input` and `embeddings`.
#'   - The `input` column contains the input texts or image labels
#'   - The `embeddings` column is a list column where each row contains an embedding vector
#'
#' @examples
#' \dontrun{
#' # Text embeddings
#' voyage_embedding("How does photosynthesis work?")
#'
#' # Multimodal embeddings
#' list("A banana", img("banana.jpg"), "Yellow fruit") |>
#'   voyage_embedding()
#' }
#' @export
voyage_embedding <- function(.input,
                             .model = "voyage-3",
                             .timeout = 120,
                             .dry_run = FALSE,
                             .max_tries = 3,
                             .verbose   = FALSE) {

  # Get the Voyage API key
  api_key <- Sys.getenv("VOYAGE_API_KEY")
  if ((api_key == "") & .dry_run == FALSE) {
    stop("API key is not set. Please set it with: Sys.setenv(VOYAGE_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  }
  
  # Determine input type
  is_message <- S7::S7_inherits(.input, LLMMessage)
  is_character <- is.character(.input)
  is_list <- is.list(.input) && !is_message
  
  # Validate the inputs
  c(
    "Input .input must be an LLMMessage object, a character vector, or a list with texts and images" = 
      is_message | is_character | is_list,
    "Input .model must be a string" = is.character(.model),
    "Input .timeout must be an integer-valued numeric (seconds till timeout)" = is.numeric(.timeout) && .timeout > 0,
    ".dry_run must be logical" = is.logical(.dry_run)
  ) |> validate_inputs()
  
  # Process inputs based on type
  if (is_list) {
    # Handle multimodal list input
    has_images <- any(sapply(.input, function(x) S7::S7_inherits(x, tidyllm_image)))
    
    if (has_images) {
      # Multimodal API
      model_to_use <- if (grepl("multimodal", .model)) .model else "voyage-multimodal-3"
      
      # Prepare the multimodal content
      content_items <- lapply(.input, function(item) {
        if (S7::S7_inherits(item, tidyllm_image)) {
          list(
            type = "image_base64",
            image_base64 = item@imagebase64
          )
        } else if (is.character(item)) {
          list(
            type = "text",
            text = item
          )
        } else {
          stop("Unsupported item type in list input")
        }
      })
      
      # Create input labels
      input_labels <- sapply(.input, function(item) {
        if (S7::S7_inherits(item, tidyllm_image)) {
          paste0("[IMG] ", item@imagename)
        } else if (is.character(item)) {
          item
        } else {
          "Unknown item"
        }
      })
      
      # Prepare multimodal request body
      request_body <- list(
        inputs = list(
          list(content = content_items)
        ),
        model = model_to_use
      )
      
      # Build multimodal request
      request <- httr2::request("https://api.voyageai.com/v1/multimodalembeddings") |>
        httr2::req_headers(
          "Content-Type" = "application/json",
          "Authorization" = paste("Bearer", api_key)
        ) |>
        httr2::req_body_json(request_body)
      
      # Return the request object if it's a dry run
      if (.dry_run) {
        return(request)
      }
      
      # Extract embeddings function for multimodal
      extract_embeddings_fn <- function(response_content, error, response_headers) {
        if (error) {
          paste0("API error response - ", response_content$error$message) |> stop()
        }
        
        response_content$data |> 
          purrr::map("embedding") |>
          purrr::map(unlist)
      }
      
      # Perform request
      return(perform_embedding_request(
        .request = request,
        .timeout = .timeout,
        .max_tries = .max_tries,
        .input_texts = input_labels,
        .fn_extract_embeddings = extract_embeddings_fn
      ))
    } else {
      # No images, treat as regular text input
      .input <- unlist(.input)
      is_character <- TRUE
    }
  }
  
  # Handle standard text or LLMMessage input
  if (is_character || is_message) {
    input_texts <- parse_embedding_input(.input)
    
    # Prepare the request body
    request_body <- list(
      model = .model,
      input = input_texts
    )
    
    # Build the request
    request <- httr2::request("https://api.voyageai.com/v1/embeddings") |>
      httr2::req_headers(
        "Content-Type" = "application/json",
        "Authorization" = paste("Bearer", api_key)
      ) |>
      httr2::req_body_json(request_body)
    
    # Return the request object if it's a dry run
    if (.dry_run) {
      return(request)
    }
    
    extract_embeddings_fn <- function(response_content, error, response_headers) {
      if (error) {
        paste0("API error response - ", response_content$error$message) |> stop()
      }
      
      response_content$data |> 
        purrr::map("embedding") |>
        purrr::map(unlist)
    }
    
    # Perform a standard embedding API request
    perform_embedding_request(
      .request = request,
      .timeout = .timeout,
      .max_tries = .max_tries,
      .input_texts = input_texts, 
      .fn_extract_embeddings = extract_embeddings_fn
    )
  }
}

#' Voyage Provider Function
#'
#' The `voyage()` function acts as a provider interface for interacting with the Voyage.ai API 
#' through `tidyllm`'s verbs.
#' It dynamically routes requests to voyage-specific functions. At the moment this is only
#' `voyage_embed()`
#'
#' @param ... Parameters to be passed to the appropriate Voyage-specific function, 
#'   such as model configuration, input text, or API-specific options.
#' @param .called_from An internal argument specifying which action (e.g., 
#'  `embed`) the function is invoked from. 
#'   This argument is automatically managed by the `tidyllm` verbs and should not be modified by the user.
#'
#' @return The result of the requested action, depending on the specific function invoked 
#'  
#'
#' @export
voyage <- create_provider_function(
  .name = "voyage",
  embed = voyage_embedding
)  
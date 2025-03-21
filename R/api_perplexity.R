
#' The Perplexity API provider class (inherits from OpenAI)
#'
#' @noRd
api_perplexity <- new_class("Perplexity", api_openai)


#' A function to get metadata from Perplexity responses
#'
#' @noRd
method(extract_metadata, list(api_perplexity,class_list))<- function(.api,.response) {
  list(
    model             = .response$model,
    timestamp         = lubridate::as_datetime(.response$created),
    prompt_tokens     = .response$usage$prompt_tokens,
    completion_tokens = .response$usage$completion_tokens,
    total_tokens      = .response$usage$total_tokens,
    stream            = FALSE,
    specific_metadata = list(
      id        = .response$id,
      citations = .response$citations
    ) 
  )
}  

#' A function to get metadata from Openai streaming responses
#'
#' @noRd
method(extract_metadata_stream, list(api_perplexity,class_list))<- function(.api,.stream_raw_data) {
  final_stream_chunk <- .stream_raw_data[[length(.stream_raw_data)]]
  list(
    model             = final_stream_chunk$model,
    timestamp         = lubridate::as_datetime(final_stream_chunk$created),
    prompt_tokens     = final_stream_chunk$usage$prompt_tokens,
    completion_tokens = final_stream_chunk$usage$completion_tokens,
    total_tokens      = final_stream_chunk$usage$total_tokens,
    stream            = TRUE,
    specific_metadata = list(
      id        = final_stream_chunk$id,
      citations = final_stream_chunk$citations
    ) 
  )
}  


#' A method to handle streaming requests 
#' Perplixity is different than vanilla openai here. 
#'
#' @noRd
method(handle_stream,list(api_perplexity,new_S3_class("httr2_response"))) <- function(.api,.stream_response) {
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
      if(length(parsed_event$choices) >= 1) {
        stream_data <- append(stream_data,list(parsed_event))
        if(!is.null(parsed_event$choices[[1]]$finish_reason)){
          finished <- parsed_event$choices[[1]]$finish_reason
          if (finished == "stop") {
            close(.stream_response)
            message("\n---------\nStream finished\n---------\n")
            break
          }
          
        }
        
        delta_content <- parsed_event$choices[[1]]$delta$content
        if (!is.null(delta_content)) {
          stream_text <- paste0(stream_text, delta_content)
          cat(delta_content)
          utils::flush.console()
        }
      }
    }
  }
  
  list(
    reply = stream_text,
    raw_data = stream_data
  )
}


#' Send LLM Messages to the Perplexity Chat API
#'
#' @description
#' This function sends a message history to the Perplexity Chat API and returns the assistant's reply.
#'
#' @param .llm An `LLMMessage` object containing the conversation history.
#' @param .model The identifier of the model to use (default: "sonar").
#' @param .max_tokens The maximum number of tokens that can be generated in the response (default: 1024).
#' @param .temperature Controls the randomness in the model's response. Values between 0 (exclusive) and 2 (exclusive) are allowed, where higher values increase randomness (optional).
#' @param .top_p Nucleus sampling parameter that controls the proportion of probability mass considered. Values between 0 (exclusive) and 1 (exclusive) are allowed (optional).
#' @param .frequency_penalty Number greater than 0. Values > 1.0 penalize repeated tokens, reducing the likelihood of repetition (optional).
#' @param .presence_penalty Number between -2.0 and 2.0. Positive values encourage new topics by penalizing tokens that have appeared so far (optional).
#' @param .stop One or more sequences where the API will stop generating further tokens. Can be a string or a list of strings (optional).
#' @param .search_domain_filter A vector of domains to limit or exclude from search results. For exclusion, prefix domains with a "-" (optional, currently in closed beta).
#' @param .return_images Logical; if TRUE, enables returning images from the model's response (default: FALSE, currently in closed beta).
#' @param .search_recency_filter Limits search results to a specific time interval (e.g., "month", "week", "day", or "hour"). Only applies to online models (optional).
#' @param .api_url Base URL for the Perplexity API (default: "https://api.perplexity.ai/").
#' @param .json Whether the response should be structured as JSON (default: FALSE).
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .stream Logical; if TRUE, streams the response piece by piece (default: FALSE).
#' @param .verbose If TRUE, displays additional information after the API call, including rate limit details (default: FALSE).
#' @param .max_tries Maximum retries to perform the request (default: 3).
#' @param .dry_run If TRUE, performs a dry run and returns the constructed request object without executing it (default: FALSE).
#'
#' @return A new `LLMMessage` object containing the original messages plus the assistant's response.
#'
#' @export
perplexity_chat <- function(.llm,
                            .model = "sonar",
                            .max_tokens = 1024,
                            .temperature = NULL,
                            .top_p = NULL,
                            .frequency_penalty = NULL,
                            .presence_penalty = NULL,
                            .stop = NULL,
                            .search_domain_filter = NULL,
                            .return_images = FALSE,
                            .search_recency_filter = NULL,
                            .api_url = "https://api.perplexity.ai/",
                            .json = FALSE,
                            .timeout = 60,
                            .verbose = FALSE,
                            .stream = FALSE,
                            .dry_run = FALSE,
                            .max_tries = 3) {

  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
    "Input .max_tokens must be an integer" = is_integer_valued(.max_tokens) & .max_tokens > 0,
    "Input .model must be a non-empty string" = is.character(.model) & nzchar(.model),
    "Input .api_url must be a valid URL" = is.character(.api_url) & nzchar(.api_url),
    "Input .timeout must be an integer-valued numeric greater than 0" = is_integer_valued(.timeout) & .timeout > 0,
    "Input .temperature must be numeric between 0 (exclusive) and 2 (exclusive) if provided" = is.null(.temperature) | (.temperature > 0 & .temperature < 2),
    "Input .top_p must be numeric between 0 (exclusive) and 1 (exclusive) if provided" = is.null(.top_p) | (.top_p > 0 & .top_p < 1),
    "Input .frequency_penalty must be greater than 0 if provided" = is.null(.frequency_penalty) | (.frequency_penalty > 0),
    "Input .presence_penalty must be numeric between -2 and 2 if provided" = is.null(.presence_penalty) | (.presence_penalty >= -2 & .presence_penalty <= 2),
    "Input .stop must be a character vector or a list of character vectors, or NULL" = is.null(.stop) | is.character(.stop) | is.list(.stop),
    "Input .search_domain_filter must be NULL or a character vector" = is.null(.search_domain_filter) | is.character(.search_domain_filter),
    "Input .return_images must be logical" = is.logical(.return_images),
    "Input .search_recency_filter must be NULL or a valid time interval string" = is.null(.search_recency_filter) | is.character(.search_recency_filter),
    "Input .json must be logical" = is.logical(.json),
    "Input .verbose must be logical" = is.logical(.verbose),
    "Input .max_tries must be integer-valued numeric" = is_integer_valued(.max_tries),
    "Input .dry_run must be logical" = is.logical(.dry_run)
  ) |>
    validate_inputs()
  
  api_obj <- api_perplexity(short_name = "perplexity",
                      long_name  = "Perplexity",
                      api_key_env_var = "PERPLEXITY_API_KEY")
  
  # Get formatted message list for Perplexity models
  messages <- to_api_format(.llm,api_obj, TRUE)
  
  api_key <- get_api_key(api_obj, .dry_run)
  
  if(!is.null(.search_domain_filter)){.search_domain_filter <- as.list(.search_domain_filter)}
  
  # Fill the request body
  request_body <- list(
    model = .model,
    max_tokens = .max_tokens,
    messages = messages,
    temperature = .temperature,
    top_p = .top_p,
    frequency_penalty = .frequency_penalty,
    presence_penalty = .presence_penalty,
    stop = .stop,
    search_domain_filter = .search_domain_filter,
    return_images = .return_images,
    search_recency_filter = .search_recency_filter,
    stream = .stream
  ) |> purrr::compact()
  
  # Handle JSON mode
  if (.json == TRUE) {
    request_body$response_format <- list(type = "json_object")
  }
  
  request <- httr2::request(.api_url) |>
    httr2::req_url_path("/chat/completions") |>
    httr2::req_headers(
      `Authorization` = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(data = request_body)
  
  # Return only the request object in a dry run.
  if (.dry_run) {
    return(request)  
  }
  
  response <- perform_chat_request(request, api_obj, .stream, .timeout, .max_tries)
  
  # Extract assistant reply and rate limiting info from response headers
  assistant_reply <- response$assistant_reply

  # Add model's message to the history of the LLMMessage object
  add_message(.llm     = .llm,
              .role    = "assistant", 
              .content = assistant_reply, 
              .json    = .json,
              .meta    = response$meta)
}

#' Perplexity Provider Function
#'
#' The `perplexity()` function acts as a provider interface for interacting with the Perplexity API 
#' through `tidyllm`'s `chat()` verb.
#' It dynamically routes requests to Perplxeity-specific function. At the moment this is only
#' `perplexity_chat()`
#'
#' @param ... Parameters to be passed to the appropriate Perplexity-specific function, 
#'   such as model configuration, input text, or API-specific options.
#' @param .called_from An internal argument specifying which action (e.g., 
#'   `chat`, `embed`) the function is invoked from. 
#'   This argument is automatically managed by the `tidyllm` verbs and should not be modified by the user.
#'
#' @return The result of the requested action, depending on the specific function invoked 
#'   (e.g., an updated `LLMMessage` object for `chat()`).
#'
#' @export
perplexity <- create_provider_function(
  .name = "perplexity",
  chat = perplexity_chat
)

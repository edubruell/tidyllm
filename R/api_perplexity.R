
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


#' Send LLM Messages to the Perplexity Chat API (All Features, No .json Option)
#'
#' @description
#' Sends a chat message history to the Perplexity Chat API, supporting all documented API parameters as of July 2025.
#'
#' @param .llm An `LLMMessage` object containing the conversation history.
#' @param .model Model name to use (default: "sonar").
#' @param .max_tokens Max completion tokens (default: 1024).
#' @param .temperature Controls response randomness (0 < x < 2).
#' @param .top_p Nucleus sampling threshold (0 < x < 1).
#' @param .frequency_penalty Number > 0. Penalizes frequent tokens.
#' @param .presence_penalty Numeric between -2 and 2. Penalizes present tokens.
#' @param .stop Stop sequence(s), string or character vector/list.
#' @param .search_domain_filter Domains to allowlist/denylist for search (max 10, "-domain" for denylist).
#' @param .return_images Logical; if TRUE, returns images from search.
#' @param .search_recency_filter Restrict search to recent ("hour","day","week","month").
#' @param .search_mode "web" (default) or "academic" (prioritize scholarly sources).
#' @param .reasoning_effort Reasoning level: "low", "medium" (default), "high" (for deep research models).
#' @param .return_related_questions Logical; if TRUE, returns related questions.
#' @param .search_after_date_filter Only content published after date (mm/dd/yyyy).
#' @param .search_before_date_filter Only content published before date (mm/dd/yyyy).
#' @param .last_updated_after_filter Only content updated after date (mm/dd/yyyy).
#' @param .last_updated_before_filter Only content updated before date (mm/dd/yyyy).
#' @param .top_k Top-k token sampling (integer, 0 disables).
#' @param .web_search_options Named list with search config (e.g. list(search_context_size = "high")).
#' @param .api_url API endpoint (default: "https://api.perplexity.ai/").
#' @param .timeout Timeout in seconds (default: 60).
#' @param .stream If TRUE, streams response.
#' @param .verbose If TRUE, prints additional info.
#' @param .max_tries Max request retries (default: 3).
#' @param .dry_run If TRUE, returns constructed request instead of sending.
#'
#' @return An updated `LLMMessage` object with the assistant's reply and metadata, including citations and search_results.
#' @export
perplexity_chat <- function(
    .llm,
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
    .search_mode = "web",
    .reasoning_effort = NULL,
    .return_related_questions = FALSE,
    .search_after_date_filter = NULL,
    .search_before_date_filter = NULL,
    .last_updated_after_filter = NULL,
    .last_updated_before_filter = NULL,
    .top_k = NULL,
    .web_search_options = NULL,
    .api_url = "https://api.perplexity.ai/",
    .timeout = 60,
    .stream = FALSE,
    .verbose = FALSE,
    .max_tries = 3,
    .dry_run = FALSE
) {
  c(
    "Input .llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
    "Input .max_tokens must be a positive integer" = is_integer_valued(.max_tokens) & .max_tokens > 0,
    "Input .model must be a non-empty string" = is.character(.model) & nzchar(.model),
    "Input .api_url must be a non-empty string" = is.character(.api_url) & nzchar(.api_url),
    "Input .timeout must be positive integer" = is_integer_valued(.timeout) & .timeout > 0,
    "Input .temperature must be NULL or 0 < x < 2" = is.null(.temperature) | (.temperature > 0 & .temperature < 2),
    "Input .top_p must be NULL or 0 < x < 1" = is.null(.top_p) | (.top_p > 0 & .top_p < 1),
    "Input .frequency_penalty must be NULL or > 0" = is.null(.frequency_penalty) | (.frequency_penalty > 0),
    "Input .presence_penalty must be NULL or -2 <= x <= 2" = is.null(.presence_penalty) | (.presence_penalty >= -2 & .presence_penalty <= 2),
    "Input .stop must be NULL, character, or list" = is.null(.stop) | is.character(.stop) | is.list(.stop),
    "Input .search_domain_filter must be NULL or character vector" = is.null(.search_domain_filter) | is.character(.search_domain_filter),
    "If .search_domain_filter provided, must be <= 10 domains" = is.null(.search_domain_filter) | (length(.search_domain_filter) <= 10),
    "Input .return_images must be logical" = is.logical(.return_images),
    "Input .return_related_questions must be logical" = is.logical(.return_related_questions),
    "Input .search_recency_filter must be NULL or character" = is.null(.search_recency_filter) | is.character(.search_recency_filter),
    "Input .search_mode must be 'web' or 'academic'" = .search_mode %in% c("web", "academic"),
    "Input .reasoning_effort must be NULL or one of 'low', 'medium', 'high'" = is.null(.reasoning_effort) | .reasoning_effort %in% c("low", "medium", "high"),
    "Input .search_after_date_filter must be NULL or character" = is.null(.search_after_date_filter) | is.character(.search_after_date_filter),
    "Input .search_before_date_filter must be NULL or character" = is.null(.search_before_date_filter) | is.character(.search_before_date_filter),
    "Input .last_updated_after_filter must be NULL or character" = is.null(.last_updated_after_filter) | is.character(.last_updated_after_filter),
    "Input .last_updated_before_filter must be NULL or character" = is.null(.last_updated_before_filter) | is.character(.last_updated_before_filter),
    "Input .top_k must be NULL or a single non-negative integer" = is.null(.top_k) | (is.numeric(.top_k) & length(.top_k) == 1 & .top_k >= 0),
    "Input .web_search_options must be NULL or named list" = is.null(.web_search_options) | (is.list(.web_search_options) & !is.null(names(.web_search_options))),
    "Input .verbose must be logical" = is.logical(.verbose),
    "Input .max_tries must be positive integer" = is_integer_valued(.max_tries) & .max_tries > 0,
    "Input .dry_run must be logical" = is.logical(.dry_run)
  ) |>
    validate_inputs()
  
  api_obj <- api_perplexity(short_name = "perplexity",
                            long_name  = "Perplexity",
                            api_key_env_var = "PERPLEXITY_API_KEY")
  
  messages <- to_api_format(.llm, api_obj, TRUE)
  api_key <- get_api_key(api_obj, .dry_run)
  
  # Convert to list for API if provided
  search_domain_filter <- if(!is.null(.search_domain_filter)) as.list(.search_domain_filter) else NULL
  
  # --- Build Request Body ---
  request_body <- list(
    model = .model,
    messages = messages,
    max_tokens = .max_tokens,
    temperature = .temperature,
    top_p = .top_p,
    frequency_penalty = .frequency_penalty,
    presence_penalty = .presence_penalty,
    stop = .stop,
    search_domain_filter = search_domain_filter,
    return_images = .return_images,
    search_recency_filter = .search_recency_filter,
    search_mode = .search_mode,
    reasoning_effort = .reasoning_effort,
    return_related_questions = .return_related_questions,
    search_after_date_filter = .search_after_date_filter,
    search_before_date_filter = .search_before_date_filter,
    last_updated_after_filter = .last_updated_after_filter,
    last_updated_before_filter = .last_updated_before_filter,
    top_k = .top_k,
    web_search_options = .web_search_options,
    stream = .stream
  ) |> purrr::compact()
  
  request <- httr2::request(.api_url) |>
    httr2::req_url_path("/chat/completions") |>
    httr2::req_headers(
      `Authorization` = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(data = request_body)
  
  # Dry run returns request object for inspection
  if (.dry_run) return(request)
  
  # --- Perform Request (may stream) ---
  response <- perform_chat_request(request, api_obj, .stream, .timeout, .max_tries)
  assistant_reply <- response$assistant_reply
  
  # --- Extract metadata including search_results if present ---
  meta <- response$meta
  if (!is.null(response$search_results)) {
    meta$specific_metadata$search_results <- response$search_results
  }
  
  add_message(.llm     = .llm,
              .role    = "assistant", 
              .content = assistant_reply, 
              .meta    = meta,
              .json    = FALSE)
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

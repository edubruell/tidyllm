
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
#' Sends a chat message history to the Perplexity Chat API, supporting all documented API parameters.
#'
#' @param .llm An `LLMMessage` object containing the conversation history.
#' @param .model Model name to use (default: "sonar").
#' @param .max_tokens Max completion tokens (default: 1024).
#' @param .temperature Controls response randomness (0 < x < 2).
#' @param .top_p Nucleus sampling threshold (0 < x < 1).
#' @param .frequency_penalty Number > 0. Penalizes frequent tokens.
#' @param .presence_penalty Numeric between -2 and 2. Penalizes present tokens.
#' @param .stop Stop sequence(s), string or character vector/list.
#' @param .search_domain_filter Character vector of domains to allowlist/denylist (max 10; prefix with "-" to denylist).
#' @param .search_language_filter ISO 639-1 language code to restrict search results (e.g. "en", "de", "fr").
#' @param .language_preference ISO 639-1 code for preferred response language (e.g. "en").
#' @param .return_images Logical; if TRUE, returns images from search.
#' @param .image_domain_filter Character vector of domains to restrict image results to.
#' @param .image_format_filter Character vector of image formats to include (e.g. c("png", "jpg")).
#' @param .search_recency_filter Restrict search to recent results: "hour", "day", "week", "month", or "year".
#' @param .search_mode Search index to use: "web" (default), "academic", or "sec".
#' @param .search_after_date_filter Only include content published after this date (MM/DD/YYYY).
#' @param .search_before_date_filter Only include content published before this date (MM/DD/YYYY).
#' @param .last_updated_after_filter Only include content last updated after this date (MM/DD/YYYY).
#' @param .last_updated_before_filter Only include content last updated before this date (MM/DD/YYYY).
#' @param .disable_search Logical; if TRUE, disables web search entirely (default: FALSE).
#' @param .enable_search_classifier Logical; if TRUE, lets the model decide whether to search (default: FALSE).
#' @param .reasoning_effort Reasoning level: "low", "medium", or "high".
#' @param .return_related_questions Logical; if TRUE, returns related questions.
#' @param .user_location Named list for geographic search personalisation. Accepted fields:
#'   \code{country} (ISO 3166-1 alpha-2), \code{city}, \code{region}, \code{latitude}, \code{longitude}.
#'   Example: \code{list(country = "DE", city = "Berlin")}.
#' @param .search_context_size Amount of search context to include: "low", "medium" (default), or "high".
#' @param .search_type Search quality preference inside \code{web_search_options}: "fast", "pro", or "auto".
#' @param .stream_mode Response format: "full" (default) or "concise".
#' @param .top_k Top-k token sampling (integer, 0 disables).
#' @param .web_search_options Named list with raw web_search_options overrides. Values set here take
#'   precedence over the dedicated parameters above.
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
    .search_language_filter = NULL,
    .language_preference = NULL,
    .return_images = FALSE,
    .image_domain_filter = NULL,
    .image_format_filter = NULL,
    .search_recency_filter = NULL,
    .search_mode = "web",
    .search_after_date_filter = NULL,
    .search_before_date_filter = NULL,
    .last_updated_after_filter = NULL,
    .last_updated_before_filter = NULL,
    .disable_search = FALSE,
    .enable_search_classifier = FALSE,
    .reasoning_effort = NULL,
    .return_related_questions = FALSE,
    .user_location = NULL,
    .search_context_size = NULL,
    .search_type = NULL,
    .stream_mode = NULL,
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
    "Input .search_language_filter must be NULL or a single string" = is.null(.search_language_filter) | (is.character(.search_language_filter) & length(.search_language_filter) == 1),
    "Input .language_preference must be NULL or a single string" = is.null(.language_preference) | (is.character(.language_preference) & length(.language_preference) == 1),
    "Input .return_images must be logical" = is.logical(.return_images),
    "Input .image_domain_filter must be NULL or character vector" = is.null(.image_domain_filter) | is.character(.image_domain_filter),
    "Input .image_format_filter must be NULL or character vector" = is.null(.image_format_filter) | is.character(.image_format_filter),
    "Input .return_related_questions must be logical" = is.logical(.return_related_questions),
    "Input .search_recency_filter must be NULL or one of 'hour','day','week','month','year'" = is.null(.search_recency_filter) | .search_recency_filter %in% c("hour", "day", "week", "month", "year"),
    "Input .search_mode must be 'web', 'academic', or 'sec'" = .search_mode %in% c("web", "academic", "sec"),
    "Input .disable_search must be logical" = is.logical(.disable_search),
    "Input .enable_search_classifier must be logical" = is.logical(.enable_search_classifier),
    "Input .reasoning_effort must be NULL or one of 'low', 'medium', 'high'" = is.null(.reasoning_effort) | .reasoning_effort %in% c("low", "medium", "high"),
    "Input .search_after_date_filter must be NULL or character" = is.null(.search_after_date_filter) | is.character(.search_after_date_filter),
    "Input .search_before_date_filter must be NULL or character" = is.null(.search_before_date_filter) | is.character(.search_before_date_filter),
    "Input .last_updated_after_filter must be NULL or character" = is.null(.last_updated_after_filter) | is.character(.last_updated_after_filter),
    "Input .last_updated_before_filter must be NULL or character" = is.null(.last_updated_before_filter) | is.character(.last_updated_before_filter),
    "Input .user_location must be NULL or a named list" = is.null(.user_location) | (is.list(.user_location) & !is.null(names(.user_location))),
    "Input .search_context_size must be NULL or one of 'low', 'medium', 'high'" = is.null(.search_context_size) | .search_context_size %in% c("low", "medium", "high"),
    "Input .search_type must be NULL or one of 'fast', 'pro', 'auto'" = is.null(.search_type) | .search_type %in% c("fast", "pro", "auto"),
    "Input .stream_mode must be NULL or one of 'full', 'concise'" = is.null(.stream_mode) | .stream_mode %in% c("full", "concise"),
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

  search_domain_filter  <- if (!is.null(.search_domain_filter))  as.list(.search_domain_filter)  else NULL
  image_domain_filter   <- if (!is.null(.image_domain_filter))   as.list(.image_domain_filter)   else NULL
  image_format_filter   <- if (!is.null(.image_format_filter))   as.list(.image_format_filter)   else NULL

  web_search_options <- c(
    list(search_context_size = .search_context_size,
         search_type         = .search_type,
         user_location       = .user_location),
    .web_search_options
  ) |> purrr::compact()
  web_search_options <- if (length(web_search_options) == 0) NULL else web_search_options

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
    search_language_filter = .search_language_filter,
    language_preference = .language_preference,
    return_images = .return_images,
    image_domain_filter = image_domain_filter,
    image_format_filter = image_format_filter,
    search_recency_filter = .search_recency_filter,
    search_mode = .search_mode,
    search_after_date_filter = .search_after_date_filter,
    search_before_date_filter = .search_before_date_filter,
    last_updated_after_filter = .last_updated_after_filter,
    last_updated_before_filter = .last_updated_before_filter,
    disable_search = if (.disable_search) TRUE else NULL,
    enable_search_classifier = if (.enable_search_classifier) TRUE else NULL,
    reasoning_effort = .reasoning_effort,
    return_related_questions = .return_related_questions,
    stream_mode = .stream_mode,
    top_k = .top_k,
    web_search_options = web_search_options,
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


#' Submit a Deep Research Request to Perplexity
#'
#' @param .llm An `LLMMessage` object containing the research question.
#' @param .background Logical; if TRUE, returns a `tidyllm_research_job` immediately without waiting (default: FALSE).
#' @param .reasoning_effort Reasoning level: "low", "medium" (default), or "high".
#' @param .search_context_size Amount of search context: "low", "medium" (default), or "high".
#' @param .search_domain_filter Character vector of domains to allowlist/denylist (max 10; prefix with "-" to denylist).
#' @param .search_language_filter ISO 639-1 language code to restrict search results (e.g. "en", "de").
#' @param .language_preference ISO 639-1 code for preferred response language.
#' @param .search_recency_filter Restrict search to recent results: "hour", "day", "week", "month", or "year".
#' @param .search_mode Search index to use: "web" (default), "academic", or "sec".
#' @param .search_after_date_filter Only include content published after this date (MM/DD/YYYY).
#' @param .search_before_date_filter Only include content published before this date (MM/DD/YYYY).
#' @param .last_updated_after_filter Only include content last updated after this date (MM/DD/YYYY).
#' @param .last_updated_before_filter Only include content last updated before this date (MM/DD/YYYY).
#' @param .user_location Named list for geographic search personalisation (fields: country, city, region, latitude, longitude).
#' @param .idempotency_key Optional string; unique key to prevent duplicate submissions.
#' @param .api_key Character; Perplexity API key (default: from environment variable).
#' @param .timeout Integer; request timeout in seconds for blocking polling (default: 300).
#' @param .max_tries Integer; maximum retries (default: 3).
#'
#' @return If `.background = FALSE`, an updated `LLMMessage` with the research reply.
#'   If `.background = TRUE`, a `tidyllm_research_job` object.
#' @export
perplexity_deep_research <- function(.llm,
                                     .background = FALSE,
                                     .reasoning_effort = "medium",
                                     .search_context_size = "medium",
                                     .search_domain_filter = NULL,
                                     .search_language_filter = NULL,
                                     .language_preference = NULL,
                                     .search_recency_filter = NULL,
                                     .search_mode = NULL,
                                     .search_after_date_filter = NULL,
                                     .search_before_date_filter = NULL,
                                     .last_updated_after_filter = NULL,
                                     .last_updated_before_filter = NULL,
                                     .user_location = NULL,
                                     .idempotency_key = NULL,
                                     .api_key = Sys.getenv("PERPLEXITY_API_KEY"),
                                     .timeout = 300,
                                     .max_tries = 3) {
  c(
    "Input .llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
    "Input .background must be logical" = is.logical(.background),
    "Input .reasoning_effort must be 'low', 'medium', or 'high'" = .reasoning_effort %in% c("low", "medium", "high"),
    "Input .search_context_size must be 'low', 'medium', or 'high'" = .search_context_size %in% c("low", "medium", "high"),
    "Input .search_domain_filter must be NULL or character vector" = is.null(.search_domain_filter) | is.character(.search_domain_filter),
    "If .search_domain_filter provided, must be <= 10 domains" = is.null(.search_domain_filter) | (length(.search_domain_filter) <= 10),
    "Input .search_language_filter must be NULL or a single string" = is.null(.search_language_filter) | (is.character(.search_language_filter) & length(.search_language_filter) == 1),
    "Input .language_preference must be NULL or a single string" = is.null(.language_preference) | (is.character(.language_preference) & length(.language_preference) == 1),
    "Input .search_recency_filter must be NULL or one of 'hour','day','week','month','year'" = is.null(.search_recency_filter) | .search_recency_filter %in% c("hour", "day", "week", "month", "year"),
    "Input .search_mode must be NULL or one of 'web', 'academic', 'sec'" = is.null(.search_mode) | .search_mode %in% c("web", "academic", "sec"),
    "Input .search_after_date_filter must be NULL or character" = is.null(.search_after_date_filter) | is.character(.search_after_date_filter),
    "Input .search_before_date_filter must be NULL or character" = is.null(.search_before_date_filter) | is.character(.search_before_date_filter),
    "Input .last_updated_after_filter must be NULL or character" = is.null(.last_updated_after_filter) | is.character(.last_updated_after_filter),
    "Input .last_updated_before_filter must be NULL or character" = is.null(.last_updated_before_filter) | is.character(.last_updated_before_filter),
    "Input .user_location must be NULL or a named list" = is.null(.user_location) | (is.list(.user_location) & !is.null(names(.user_location))),
    "Input .idempotency_key must be NULL or a single string" = is.null(.idempotency_key) | (is.character(.idempotency_key) & length(.idempotency_key) == 1),
    "Input .api_key must be non-empty" = nzchar(.api_key),
    "Input .timeout must be a positive integer" = is_integer_valued(.timeout) && .timeout > 0,
    "Input .max_tries must be a positive integer" = is_integer_valued(.max_tries) && .max_tries > 0
  ) |> validate_inputs()

  api_obj <- api_perplexity(short_name = "perplexity",
                            long_name  = "Perplexity",
                            api_key_env_var = "PERPLEXITY_API_KEY")

  messages <- to_api_format(.llm, api_obj, TRUE)

  search_domain_filter <- if (!is.null(.search_domain_filter)) as.list(.search_domain_filter) else NULL

  web_search_options <- list(
    search_context_size = .search_context_size,
    user_location       = .user_location
  ) |> purrr::compact()

  inner_request <- list(
    model = "sonar-deep-research",
    messages = messages,
    reasoning_effort = .reasoning_effort,
    search_domain_filter = search_domain_filter,
    search_language_filter = .search_language_filter,
    language_preference = .language_preference,
    search_recency_filter = .search_recency_filter,
    search_mode = .search_mode,
    search_after_date_filter = .search_after_date_filter,
    search_before_date_filter = .search_before_date_filter,
    last_updated_after_filter = .last_updated_after_filter,
    last_updated_before_filter = .last_updated_before_filter,
    web_search_options = web_search_options
  ) |> purrr::compact()

  request_body <- list(
    request = inner_request,
    idempotency_key = .idempotency_key
  ) |> purrr::compact()

  response <- httr2::request("https://api.perplexity.ai") |>
    httr2::req_url_path("/async/chat/completions") |>
    httr2::req_headers(
      `Authorization` = sprintf("Bearer %s", .api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(data = request_body) |>
    httr2::req_retry(max_tries = .max_tries) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  job_id <- response$id
  job <- structure(list(job_id = job_id, message = .llm), class = "tidyllm_research_job")

  if (.background) return(job)

  perplexity_fetch_research(perplexity_poll_research(job, .api_key, .timeout, .max_tries))
}


#' Poll a Perplexity Deep Research Job Until Completion
#'
#' @param .job A `tidyllm_research_job` object.
#' @param .api_key Character; Perplexity API key.
#' @param .timeout Integer; total seconds to wait before giving up (default: 300).
#' @param .max_tries Integer; maximum retries per poll (default: 3).
#' @return An updated `tidyllm_research_job` with status information.
#' @noRd
perplexity_poll_research <- function(.job, .api_key = Sys.getenv("PERPLEXITY_API_KEY"),
                                     .timeout = 300, .max_tries = 3) {
  start_time <- proc.time()[["elapsed"]]
  repeat {
    status_response <- httr2::request("https://api.perplexity.ai") |>
      httr2::req_url_path(sprintf("/async/chat/completions/%s", .job$job_id)) |>
      httr2::req_headers(`Authorization` = sprintf("Bearer %s", .api_key)) |>
      httr2::req_retry(max_tries = .max_tries) |>
      httr2::req_perform() |>
      httr2::resp_body_json()

    if (identical(status_response$status, "COMPLETED")) {
      .job$response <- status_response
      return(.job)
    }

    elapsed <- proc.time()[["elapsed"]] - start_time
    if (elapsed > .timeout) {
      stop(sprintf("Perplexity deep research job '%s' did not complete within %d seconds.", .job$job_id, .timeout))
    }
    Sys.sleep(5)
  }
}


#' Check the Status of a Perplexity Deep Research Job
#'
#' @param .job A `tidyllm_research_job` object returned by `perplexity_deep_research(.background = TRUE)`.
#' @param .api_key Character; Perplexity API key (default: from environment).
#' @param .max_tries Integer; maximum retries (default: 3).
#' @return An updated `tidyllm_research_job` with a `$status` field and `$response` if completed.
#' @export
perplexity_check_research <- function(.job,
                                      .api_key = Sys.getenv("PERPLEXITY_API_KEY"),
                                      .max_tries = 3) {
  c(
    "Input .job must be a tidyllm_research_job" = inherits(.job, "tidyllm_research_job"),
    "Input .api_key must be non-empty" = nzchar(.api_key)
  ) |> validate_inputs()

  status_response <- httr2::request("https://api.perplexity.ai") |>
    httr2::req_url_path(sprintf("/async/chat/completions/%s", .job$job_id)) |>
    httr2::req_headers(`Authorization` = sprintf("Bearer %s", .api_key)) |>
    httr2::req_retry(max_tries = .max_tries) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  .job$status <- status_response$status
  if (identical(status_response$status, "COMPLETED")) {
    .job$response <- status_response
  }
  .job
}


#' Fetch Results from a Completed Perplexity Deep Research Job
#'
#' @param .job A `tidyllm_research_job` object. Must have status "completed" or will poll once.
#' @param .api_key Character; Perplexity API key (default: from environment).
#' @param .max_tries Integer; maximum retries (default: 3).
#' @return An updated `LLMMessage` with the research reply appended.
#' @export
perplexity_fetch_research <- function(.job,
                                      .api_key = Sys.getenv("PERPLEXITY_API_KEY"),
                                      .max_tries = 3) {
  c(
    "Input .job must be a tidyllm_research_job" = inherits(.job, "tidyllm_research_job"),
    "Input .api_key must be non-empty" = nzchar(.api_key)
  ) |> validate_inputs()

  if (is.null(.job$response)) {
    .job <- perplexity_check_research(.job, .api_key, .max_tries)
    if (!identical(.job$status, "COMPLETED")) {
      stop(sprintf("Perplexity deep research job '%s' is not yet completed (status: %s).", .job$job_id, .job$status))
    }
  }

  api_obj <- api_perplexity(short_name = "perplexity",
                            long_name  = "Perplexity",
                            api_key_env_var = "PERPLEXITY_API_KEY")

  response_content <- .job$response$response
  assistant_reply <- parse_chat_response(api_obj, response_content)
  meta <- extract_metadata(api_obj, response_content)

  add_message(.llm     = .job$message,
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
  chat = perplexity_chat,
  deep_research = perplexity_deep_research
)

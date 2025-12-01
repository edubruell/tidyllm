# Skip file if ellmer is not installed
if (!requireNamespace("ellmer", quietly = TRUE)) return(invisible(NULL))

api_ellmer <- new_class("Ellmer", APIProvider)

method(to_api_format, list(LLMMessage, api_ellmer)) <- function(.llm, 
                                                                .api) {
  filtered_history <- filter_roles(.llm@message_history, c("user", "assistant", "system"))
  
  lapply(filtered_history, function(m) {
    formatted_message <- format_message(m)
    
    if (m$role == "system") {
      NULL
    } else if (m$role == "user") {
      contents <- list(ellmer::ContentText(formatted_message$content))
      
      if (!is.null(formatted_message$image)) {
        image_content <- ellmer::ContentImageInline(
          type = formatted_message$image$media_type,
          data = formatted_message$image$data
        )
        contents <- c(contents, list(image_content))
      }
      
      ellmer::Turn(role = "user", contents = contents)
    } else if (m$role == "assistant") {
      ellmer::Turn(role = "assistant", contents = list(ellmer::ContentText(formatted_message$content)))
    }
  }) |> 
    Filter(Negate(is.null), x = _)
}


method(parse_chat_response, list(api_ellmer, class_list)) <- function(.api, .content) {
  if (!is.character(.content) || length(.content) == 0) {
    stop("Received empty response from ellmer chat object")
  }
  .content
}


method(extract_metadata, list(api_ellmer, class_list)) <- function(.api, .response) {
  tokens <- if (!is.null(.response$tokens)) {
    .response$tokens
  } else {
    c(input = NA_integer_, output = NA_integer_, cached_input = NA_integer_)
  }
  
  list(
    model             = .response$model %||% "unknown",
    timestamp         = lubridate::as_datetime(lubridate::now()),
    prompt_tokens     = as.integer(tokens["input"] %||% NA),
    completion_tokens = as.integer(tokens["output"] %||% NA),
    total_tokens      = as.integer((tokens["input"] %||% 0) + (tokens["output"] %||% 0)),
    stream            = FALSE,
    specific_metadata = list(
      cost = .response$cost,
      duration = .response$duration
    ) 
  )
}


#' Send LLM Messages to Ellmer Chat Object
#'
#' @description
#' This function converts an `LLMMessage` to the turns ellmer chat object.
#' The ellmer object is cloned and its turn history is cleared internatlly.
#' This maintains tidyllms stateless style. 
#'
#' @param .llm An `LLMMessage` object containing the conversation history.
#' @param .ellmer_chat An ellmer chat object (e.g., from `ellmer::chat_anthropic()`).
#' @param .stream Logical; if TRUE, streams the response piece by piece (default: FALSE).
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .verbose If TRUE, displays additional information about the chat (default: FALSE).
#' @param .dry_run If TRUE, returns the constructed request object without executing it (default: FALSE).
#' @param .tools Dummy for error message if called from chat with tools (default: NULL).
#' @param .max_tries Maximum retries to perform the request (default: 3).
#'
#' @return A new `LLMMessage` object containing the original messages plus the assistant's response.
#'
#' @export
chat_ellmer <- function(.llm,
                        .ellmer_chat,
                        .stream = FALSE,
                        .timeout = 60,
                        .verbose = FALSE,
                        .dry_run = FALSE,
                        .tools  = NULL,
                        .max_tries = 3) {
  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
    "Input .ellmer_chat must be an ellmer chat object" = inherits(.ellmer_chat, "Chat"),
    "Input .stream must be logical" = is.logical(.stream),
    "Input .timeout must be a positive integer" = is_integer_valued(.timeout) & .timeout > 0,
    "Input .verbose must be logical" = is.logical(.verbose),
    "Input .dry_run must be logical" = is.logical(.dry_run),
    "Tidyllm tools are not supported in the ellmer backend. Set ellmer tools directly in the ellmer chat object instead" = is.null(.tools),
    "Input .max_tries must be integer-valued numeric" = is_integer_valued(.max_tries)
  ) |> validate_inputs()
  
  api_obj <- api_ellmer(short_name = "ellmer",
                        long_name  = "Ellmer")
  
  if (.dry_run) {
    return(list(
      action = "ellmer_chat",
      llm_message = .llm,
      ellmer_object = .ellmer_chat
    ))
  }
  
  turn_data <- to_api_format(.llm, api_obj)
  
  cloned_ellmer <- .ellmer_chat$clone()
  cloned_ellmer[[".__enclos_env__"]][["private"]][[".turns"]] <- turn_data
  
  last_user_turn <- turn_data[[length(turn_data)]]
  last_user_text <- ellmer::contents_text(last_user_turn)
  
  assistant_reply <- cloned_ellmer$chat(last_user_text,echo=FALSE)
  
  last_turn <- cloned_ellmer$last_turn()
  
  assistant_reply
  metadata <- extract_metadata(api_obj, list(
    model = cloned_ellmer$get_model(),
    tokens = last_turn@tokens,
    cost = last_turn@cost,
    duration = last_turn@duration
  ))
  
  add_message(.llm     = .llm,
              .role    = "assistant", 
              .content = assistant_reply, 
              .json    = FALSE,
              .meta    = metadata)
}


#' Alias for the Ellmer Provider Function
#'
#' The `chat_ellmer()` function acts as a provider interface for interacting with
#' ellmer chat objects through `tidyllm`'s  verb interface
#'
#' This function allows you to use any ellmer chat object (e.g., from
#' `ellmer::chat_anthropic()`, `ellmer::chat_openai()`, etc.) as a stateless
#' backend for tidyllm. The ellmer object is cloned for each interaction to
#' maintain tidyllm's stateless approach. 
#'
#' @param .ellmer_chat An ellmer chat object to use as the backend.
#' @param ... Additional parameters to pass through (for consistency with other providers).
#' @param .called_from An internal argument specifying which action (e.g., 
#'   `chat`) the function is invoked from.
#'
#' @return A provider function that can be used with `tidyllm::chat()`.
#'
#' @export
ellmer <- create_provider_function(
  .name = "ellmer",
  chat = chat_ellmer
)



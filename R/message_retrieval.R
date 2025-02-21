#' Retrieve Assistant Reply as Text
#'
#' Extracts the plain text content of the assistant's reply from an `LLMMessage` object.
#' Use [get_reply_data()] for structured replies in JSON format.
#'
#' This function is the core utility for retrieving assistant replies by index. 
#' For convenience, [last_reply()] is provided as a wrapper to retrieve the 
#' latest assistant reply.
#'
#' @param .llm An `LLMMessage` object containing the message history.
#' @param .index A positive integer indicating the index of the assistant reply to retrieve.
#'   Defaults to `NULL`, which retrieves the last reply.
#' @return Returns a character string containing the assistant's reply, or `NA_character_` if no reply exists.
#' @seealso [get_reply_data()], [last_reply()]
#' @export
#' @rdname get_reply
get_reply <- function(.llm, .index = NULL) {
  # Validate that .llm is an LLMMessage object and index is within bounds if provided
  validate_inputs(c(
    "Input .llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
    "Index must be a positive integer within bounds" = is.null(.index) || 
      (is.numeric(.index) && .index > 0 && .index <= length(Filter(function(x) x$role == "assistant", .llm@message_history)))
  ))
  
  # Retrieve assistant replies
  assistant_replies <-  filter_roles(.llm@message_history,"assistant")
  
  # Check if any assistant replies are available
  if (length(assistant_replies) == 0) {
    warning("No assistant replies available in the message history.")
    return(NA_character_)
  }
  
  # Select reply by index or use the last reply if .index is NULL
  selected_reply <- if (is.null(.index)) assistant_replies[[length(assistant_replies)]] else assistant_replies[[.index]]
  
  selected_reply$content
}

#' @export
#' @rdname get_reply
last_reply <- function(.llm) {
  get_reply(.llm = .llm, .index = NULL)
}


#' Retrieve Assistant Reply as Structured Data
#'
#' Parses the assistant's reply as JSON and returns the corresponding structured data.
#' If the reply is not marked as JSON, attempts to extract and parse JSON content from the text.
#'
#' For convenience, [last_reply_data()] is provided as a wrapper to retrieve the 
#' latest assistant reply's data.
#'
#' @param .llm An `LLMMessage` object containing the message history.
#' @param .index A positive integer indicating the index of the assistant reply to retrieve.
#'   Defaults to `NULL`, which retrieves the last reply.
#' @return Returns the parsed data from the assistant's reply, or `NULL` if parsing fails.
#' @seealso [get_reply()], [last_reply_data()]
#' @export
#' @rdname get_reply_data
get_reply_data <- function(.llm, .index = NULL) {
  # Validate inputs for .llm and .index
  validate_inputs(c(
    "Input .llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage)
  ))
  
  # Retrieve assistant replies with JSON flag
  assistant_replies <- filter_roles(.llm@message_history, "assistant")
  
  # Check if any assistant replies are available
  if (length(assistant_replies) == 0) {
    warning("No assistant replies available in the message history.")
    return(NULL)
  }
  
  # Validate and select reply based on index
  if (is.null(.index)) {
    selected_reply <- assistant_replies[[length(assistant_replies)]]
  } else {
    validate_inputs(c(
      "Index must be a positive integer within bounds" = is.numeric(.index) && .index > 0 && .index <= length(assistant_replies)
    ))
    selected_reply <- assistant_replies[[.index]]
  }
  
  # Regular expression to detect and extract JSON content across multiple lines
  json_extract_pattern <- "\\{[\\s\\S]*\\}|\\[[\\s\\S]*\\]"
  
  # Check if the selected reply is marked as JSON or contains JSON-like structure
  if (selected_reply$json) {
    json_content <- selected_reply$content
  } else {
    warning("The reply is not explicitly marked as JSON. Trying to extract JSON.")
    json_content <- stringr::str_extract(selected_reply$content, json_extract_pattern)
    if (is.na(json_content)) {
      warning("The selected reply does not appear to contain valid JSON. Returning NULL.")
      return(NULL)
    }
  }
  
  # Attempt to parse extracted JSON content
  tryCatch(
    jsonlite::fromJSON(json_content),
    error = function(e) {
      warning("Failed to parse JSON content. Returning NULL.")
      NULL
    }
  )
}

#' @export
#' @rdname get_reply_data
last_reply_data <- function(.llm) {
  get_reply_data(.llm = .llm, .index = NULL)
}



#' Retrieve Metadata from Assistant Replies
#'
#' Retrieves metadata from assistant replies within an `LLMMessage` object. 
#' It returns the metadata as a tibble.
#'
#' Metadata columns may include:
#' - `model`: The model used for generating the reply.
#' - `timestamp`: The time when the reply was generated.
#' - `prompt_tokens`: The number of tokens in the input prompt.
#' - `completion_tokens`: The number of tokens in the assistant's reply.
#' - `total_tokens`: The total number of tokens (prompt + completion).
#' - `api_specific`: A list column with API-specific metadata.
#' 
#' For convenience, [last_metadata()] is provided to retrieve the metadata for the last message.
#'
#' @param .llm An `LLMMessage` object containing the message history.
#' @param .index A positive integer specifying which assistant reply's metadata to extract.
#'   If `NULL` (default), metadata for all replies is returned.
#' @return A tibble containing metadata for the specified assistant reply or all replies.
#' @seealso [last_metadata()]
#' @export
#' @rdname get_metadata
get_metadata <- function(.llm, .index = NULL) {
  # Validate input
  validate_inputs(c(
    "Input .llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
    "Index must be a positive integer within bounds" = is.null(.index) || 
      (is.numeric(.index) && .index > 0 && .index <= length(filter_roles(.llm@message_history, "assistant")))
  ))
  
  # Extract assistant replies
  assistant_replies <- filter_roles(.llm@message_history, "assistant")
  
  # Check if any assistant replies are available
  if (length(assistant_replies) == 0) {
    warning("No assistant replies available in the message history.")
    return(tibble::tibble())
  }
  
  # Select the specified reply or all replies
  selected_replies <- if (is.null(.index)) assistant_replies else list(assistant_replies[[.index]])
  
  # Extract metadata
  purrr::map_dfr(selected_replies, function(reply) {
    meta <- reply$meta
    
    tibble::tibble(
      model = if (!is.null(meta$model)) meta$model else NA_character_,
      timestamp = if (!is.null(meta$timestamp)) meta$timestamp else NA_character_,
      prompt_tokens = if (!is.null(meta$prompt_tokens)) meta$prompt_tokens else NA_integer_,
      completion_tokens = if (!is.null(meta$completion_tokens)) meta$completion_tokens else NA_integer_,
      total_tokens = if (!is.null(meta$total_tokens)) meta$total_tokens else NA_integer_,
      api_specific = list(meta$specific_metadata)
    )
  })
}

#' @export
#' @rdname get_metadata
last_metadata <- function(.llm) {
  get_metadata(.llm = .llm)
}


#' Retrieve Log Probabilities from Assistant Replies
#'
#' Extracts token log probabilities from assistant replies within an `LLMMessage` object. 
#' Each row represents a token with its log probability and top alternative tokens.
#'
#' An empty tibble is output if no logprobs were requested. Currently only works with `openai_chat()`
#'
#' Columns include:
#' - `reply_index`: The index of the assistant reply in the message history.
#' - `token`: The generated token.
#' - `logprob`: The log probability of the generated token.
#' - `bytes`: The byte-level encoding of the token.
#' - `top_logprobs`: A list column containing the top alternative tokens with their log probabilities.
#'
#' @param .llm An `LLMMessage` object containing the message history.
#' @param .index A positive integer specifying which assistant reply's log probabilities to extract.
#'   If `NULL` (default), log probabilities for all replies are returned.
#' @return A tibble containing log probabilities for the specified assistant reply or all replies.
#' @seealso [get_metadata()]
#' @export
#' @rdname get_logprobs
get_logprobs <- function(.llm, .index = NULL) {
  # Validate input
  validate_inputs(c(
    "Input .llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
    "Index must be a positive integer within bounds" = is.null(.index) || 
      (is.numeric(.index) && .index > 0 && .index <= length(filter_roles(.llm@message_history, "assistant")))
  ))
  
  # Extract assistant replies
  assistant_replies <- filter_roles(.llm@message_history, "assistant")
  
  # Check if any assistant replies are available
  if (length(assistant_replies) == 0) {
    warning("No assistant replies available in the message history.")
    return(tibble::tibble())
  }
  
  # Select the specified reply or all replies
  selected_indices <- if (is.null(.index)) seq_along(assistant_replies) else .index
  selected_replies <- assistant_replies[selected_indices]
  
  # Extract logprobs
  purrr::imap_dfr(selected_replies, function(reply, idx) {
    if (is.null(reply$logprobs)) {
      return(tibble::tibble(
        reply_index = idx,
        token = character(),
        logprob = numeric(),
        bytes = list(),
        top_logprobs = list()
      ))
    }
    
    # Extract each token's logprobs
    purrr::map_dfr(reply$logprobs, function(token_data) {
      tibble::tibble(
        reply_index = idx,  # Include the reply index
        token = token_data$token,
        logprob = token_data$logprob,
        bytes = list(token_data$bytes),
        top_logprobs = list(token_data$top_logprobs)
      )
    })
  })
}




#' Retrieve a User Message by Index
#'
#' Extracts the content of a user's message from an `LLMMessage` object at a specific index.
#' 
#' For convenience, [last_user_message()] is provided as a wrapper to retrieve the
#' latest user message without specifying an index.
#'
#' @param .llm An `LLMMessage` object.
#' @param .index A positive integer indicating which user message to retrieve. Defaults to `NULL`, which retrieves the last message.
#' @return Returns the content of the user's message at the specified index. If no messages are found, returns `NA_character_`.
#' @seealso [last_user_message()]
#' @export
#' @rdname get_user_message
get_user_message <- function(.llm, .index = NULL) {
  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage)
  ) |> validate_inputs()
  
  # Filter to get all user messages
  user_messages <- filter_roles(.llm@message_history,"user")
  
  # Return NULL if there are no user messages
  if (length(user_messages) == 0) {
    warning("No user messages available in the message history.")
    return(NA_character_)
  }
  
  # If .index is NULL, get the last user message
  if (is.null(.index)) {
    selected_message <- user_messages[[length(user_messages)]]
  } else {
    # Validate .index
    c(
      "Index must be a positive integer" = is.numeric(.index) && .index > 0,
      "Index is out of bounds" = .index <= length(user_messages)
    ) |> validate_inputs()
    selected_message <- user_messages[[.index]]
  }
  
  return(selected_message$content)
}

#' @export
#' @rdname get_user_message
last_user_message <- function(.llm) {
  get_user_message(.llm = .llm, .index = NULL)
}
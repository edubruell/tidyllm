#' Get Assistant Reply as Text
#'
#' Retrieves the assistant's reply as plain text from an `LLMMessage` object at a specified index.
#'
#' @param .llm An `LLMMessage` object containing the message history.
#' @param .index A positive integer for the assistant reply index to retrieve, defaulting to the last reply.
#' @return Plain text content of the assistant's reply, or `NA_character_` if no reply is available.
#' @export
get_reply <- function(.llm, .index = NULL) {
  # Validate that .llm is an LLMMessage object and index is within bounds if provided
  validate_inputs(c(
    "Input .llm must be an LLMMessage object" = inherits(.llm, "LLMMessage"),
    "Index must be a positive integer within bounds" = is.null(.index) || 
      (is.numeric(.index) && .index > 0 && .index <= length(Filter(function(x) x$role == "assistant", .llm$message_history)))
  ))
  
  # Retrieve assistant replies
  assistant_replies <-  filter_roles(.llm$message_history,"assistant")
  
  # Check if any assistant replies are available
  if (length(assistant_replies) == 0) {
    warning("No assistant replies available in the message history.")
    return(NA_character_)
  }
  
  # Select reply by index or use the last reply if .index is NULL
  selected_reply <- if (is.null(.index)) assistant_replies[[length(assistant_replies)]] else assistant_replies[[.index]]
  
  selected_reply$content
}


#' Get the Last Assistant Reply as Text
#'
#' A wrapper around `get_reply()` to retrieve the most recent assistant text reply.
#'
#' @param .llm A `LLMMessage` object.
#'
#' @return Returns the content of the assistant's reply at the specified index, based on the following conditions:
#' @export
last_reply <- function(.llm) {
  get_reply(.llm = .llm, .index = NULL)
}



#' Get Data from an Assistant Reply by parsing structured JSON responses
#'
#' Retrieves and parses the assistant's reply as JSON from an `LLMMessage` object at a specified index.
#' If the reply is not marked as JSON, attempts to extract JSON content from text.
#'
#' @param .llm An `LLMMessage` object containing the message history.
#' @param .index A positive integer for the assistant reply index to retrieve, defaulting to the last reply.
#' @return Parsed data content of the assistant's reply, or `NULL` if parsing fails.
#' @export
get_reply_data <- function(.llm, .index = NULL) {
  # Validate inputs for .llm and .index
  validate_inputs(c(
    "Input .llm must be an LLMMessage object" = inherits(.llm, "LLMMessage")
  ))
  
  # Retrieve assistant replies with JSON flag
  assistant_replies <- filter_roles(.llm$message_history, "assistant")
  
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


#' Get the Last Assistant Reply as Text
#'
#' A wrapper around `get_reply_data()` to retrieve structured data from the most recent assistant reply.
#'
#' @param .llm A `LLMMessage` object.
#'
#' @return Returns the content of the assistant's reply at the specified index, based on the following conditions:
#' @export
last_reply_data <- function(.llm) {
  get_reply_data(.llm = .llm, .index = NULL)
}


#' Retrieve a User Message by Index
#'
#' Extracts the content of a user's message from an `LLMMessage` object at a specific index.
#'
#' @param .llm A `LLMMessage` object.
#' @param .index A positive integer indicating which user message to retrieve. Defaults to `NULL`, which retrieves the last message.
#'
#' @return Returns the content of the user's message at the specified index. If no messages are found, returns `NULL`.
#' @export
get_user_message <- function(.llm, .index = NULL) {
  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = inherits(.llm, "LLMMessage")
  ) |> validate_inputs()
  
  # Filter to get all user messages
  user_messages <- filter_roles(.llm$message_history,"user")
  
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

#' Retrieve the Last User Message
#'
#' A wrapper around `get_user_message()` to retrieve the most recent user message.
#'
#' @param .llm A `LLMMessage` object.
#' @return The content of the last user message.
#' @export
last_user_message <- function(.llm) {
  get_user_message(.llm = .llm, .index = NULL)
}

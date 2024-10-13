#' Retrieve an Assistant Reply by Index
#'
#' Extracts the content of an assistant's reply from an `LLMMessage` object at a specific index.
#' This function can handle replies that are expected to be in JSON format by attempting to parse them.
#' If parsing fails or if the user opts for raw text, the function will gracefully return the original content.
#'
#' @param .llm A `LLMMessage` object containing the history of messages exchanged with the assistant.
#'             This parameter must be a valid `LLMMessage` object; otherwise, the function will throw an error.
#' @param .index A positive integer indicating which assistant reply to retrieve. Defaults to `NULL`, which retrieves the last reply.
#' @param .raw A logical value indicating whether to return the raw text even if the message is marked as JSON.
#'             Defaults to `FALSE`, meaning the function will attempt to parse the JSON.
#'
#' @return Returns the content of the assistant's reply at the specified index, based on the following conditions:
#'   - If there are no assistant replies, `NULL` is returned.
#'   - If the reply is marked as JSON and parsing is successful, a list containing:
#'     - `parsed_content`: The parsed JSON content.
#'     - `raw_response`: The original raw content.
#'     - `json`: A flag indicating successful JSON parsing (`TRUE`).
#'   - If JSON parsing fails, a list containing:
#'     - `parsed_content`: `NULL`.
#'     - `raw_response`: The original raw content.
#'     - `json`: `FALSE`.
#'   - If `.raw` is `TRUE` or the message is not marked as JSON, returns the raw text content directly.
#'
#' @export
get_reply <- function(.llm, .index = NULL, .raw = FALSE) {
  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = inherits(.llm, "LLMMessage"),
    "Input .raw must be logical if provided" = is.logical(.raw)
  ) |> validate_inputs()
  
  # Filter to get all assistant messages
  assistant_replies <- Filter(function(x) x$role == "assistant", .llm$message_history)
  
  # Return NULL if there are no assistant replies
  if (length(assistant_replies) == 0) {
    return(NULL)
  }
  
  # If .index is NULL, get the last assistant reply
  if (is.null(.index)) {
    selected_reply <- assistant_replies[[length(assistant_replies)]]
  } else {
    # Validate .index
    c(
      "Index must be a positive integer" = is.numeric(.index) && .index > 0,
      "Index is out of bounds" = .index <= length(assistant_replies)
    ) |> validate_inputs()
    selected_reply <- assistant_replies[[.index]]
  }
  
  content <- selected_reply$content
  is_json_response <- selected_reply$json
  
  # If raw content is requested or the message is not JSON, return the raw content
  if (.raw || !is_json_response) {
    return(content)
  }
  
  # Attempt to parse the JSON content
  parsed_content <- tryCatch(
    jsonlite::fromJSON(content),
    error = function(e) {
      warning("Failed to parse JSON content. Returning raw text.")
      NULL
    }
  )
  
  # Return structured response list
  response_list <- list(
    raw_response = content,
    parsed_content = parsed_content,
    is_parsed = !is.null(parsed_content)
  )
  
  return(response_list)
}

#' Retrieve the Last Assistant Reply
#'
#' A wrapper around `get_reply()` to retrieve the most recent assistant reply.
#'
#' @param .llm A `LLMMessage` object.
#' @param .raw A logical value indicating whether to return the raw text even if the message is marked as JSON.
#'             Defaults to `FALSE`, meaning the function will attempt to parse the JSON.
#'
#' @return Returns the content of the assistant's reply at the specified index, based on the following conditions:
#'   - If there are no assistant replies, `NULL` is returned.
#'   - If the reply is marked as JSON and parsing is successful, a list containing:
#'     - `parsed_content`: The parsed JSON content.
#'     - `raw_response`: The original raw content.
#'     - `json`: A flag indicating successful JSON parsing (`TRUE`).
#'   - If JSON parsing fails, a list containing:
#'     - `parsed_content`: `NULL`.
#'     - `raw_response`: The original raw content.
#'     - `json`: `FALSE`.
#'   - If `.raw` is `TRUE` or the message is not marked as JSON, returns the raw text content directly.
#'
#' @export
last_reply <- function(.llm, .raw = FALSE) {
  get_reply(.llm = .llm, .index = NULL, .raw = .raw)
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
  user_messages <- Filter(function(x) x$role == "user", .llm$message_history)
  
  # Return NULL if there are no user messages
  if (length(user_messages) == 0) {
    return(NULL)
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

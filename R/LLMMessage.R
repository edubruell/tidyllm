
#' Large Language Model Message Class
#'
#' @description
#' `LLMMessage` is an S7 class for managing a conversation history intended for use with large language models (LLMs). Please use 
#' `llm_message()`to create or modify `LLMMessage` objects. 
#'
#' @param message_history A list containing messages. Each message is a named list with keys like `role`, `content`, `media`, etc.
#' @param system_prompt A character string representing the default system prompt used for the conversation.
#' @details
#' The `LLMMessage` class includes the following features:
#' - Stores message history in a structured format.
#' - Supports attaching media and metadata to messages.
#' - Provides generics like `add_message()`, `has_image()`, and `remove_message()` for interaction.
#' - Enables API-specific formatting through the `to_api_format()` generic.
#' - `message_history`: A list containing messages. Each message is a named list with keys like `role`, `content`, `media`, etc.
#' - `system_prompt`: A character string representing the default system prompt used for the conversation.
#' @export
LLMMessage <- new_class("LLMMessage", 
                        properties = list(
                          message_history = class_list,
                          system_prompt = class_character
                        )
)

#' An API provider
#'
#' At the moment this is just a stub but is needed for methods dispatch
#' @noRd
api_provider <- new_class("api_provider")

#LLMMessage generics
add_message <- new_generic("add_message", ".llm")
has_image   <- new_generic("has_image", ".llm")
remove_message <- new_generic("remove_message", ".llm")
to_api_format <- new_generic("to_api_format", c(".llm", ".api"))
print.LLMMessage     <- new_external_generic("base", "print", "x")
as_tibble.LLMMessage <- new_external_generic("tibble", "as_tibble", "x")

#' Add a New Message to LLMMessage
#'
#' Adds a new message to the `message_history` property of an `LLMMessage` object.
#' Supports roles such as "user", "assistant", and "system" along with optional
#' media, metadata, and JSON flags.
#'
#' @noRd
method(add_message, LLMMessage) <- function(.llm,
                                            .role,
                                            .content,
                                            .media = NULL,
                                            .files = NULL,
                                            .json = FALSE,
                                            .meta = NULL,
                                            .logprobs = NULL) {
  if (!is.null(.files)) {
    files_list <- if (S7_inherits(.files, tidyllm_file)) list(.files) else .files
    bad <- !all(vapply(files_list, function(f) S7_inherits(f, tidyllm_file), logical(1)))
    if (bad) stop(".files must be a tidyllm_file object or a list of tidyllm_file objects.")
    .files <- files_list
  }

  message_details <- list(role = .role, content = .content, json = .json)
  if (!is.null(.media)) {
    message_details$media <- .media
  }
  if (!is.null(.files)) {
    message_details$files <- .files
  }
  if (!is.null(.meta)) {
    message_details$meta <- .meta
  }
  if (!is.null(.logprobs)) {
    message_details$logprobs <- .logprobs
  }
  .llm@message_history <- c(.llm@message_history, list(message_details))
  .llm
}


#' Check if Any Messages Contain Images
#'
#' Determines whether any messages in the `message_history` property of an `LLMMessage` object
#' include attached images.
#'
#' @noRd
method(has_image, LLMMessage) <- function(.llm) {
  .llm@message_history |>
    purrr::map_lgl(function(m) {
      length(extract_media(m$media, "image")) > 0
    }) |>
    any()
}

#' Prints the current message history in a structured format.
#'
#' By default, this function respects the `tidyllm_print_metadata` option. 
#' If the option is set to `TRUE`, metadata for each message (if available) will be printed.
#' 
#' Users can override the default behavior by explicitly passing a value to the `.meta` parameter.
#'
#' @param .meta Logical; if `TRUE`, prints metadata for each message. 
#' Defaults to the value of `getOption("tidyllm_print_metadata", FALSE)`.
#' @noRd
method(print.LLMMessage,LLMMessage) <- function(x,...,.meta = getOption("tidyllm_print_metadata", FALSE)) {
  cat("Message History:\n")
  
  purrr::walk(seq_along(x@message_history), function(i) {
    message <- x@message_history[[i]]
    
    # Print role and content
    content_lines <- unlist(strsplit(message$content, "\n"))
    wrapped_lines <- purrr::map(content_lines, function(line) {
      stringr::str_wrap(line, width = 60)
    })
    wrapped_content <- paste(unlist(wrapped_lines), collapse = "\n")
    cat(sprintf("%s:\n%s\n", message$role, wrapped_content))
    
    # Print media details if available
    if (!is.null(message$media) && length(message$media) > 0) {
      media_labels <- vapply(message$media, function(m) {
        if (S7_inherits(m, tidyllm_image)) return(paste0("[image: ", m@imagename, "]"))
        if (S7_inherits(m, tidyllm_audio)) return(paste0("[audio: ", m@audioname, "]"))
        if (S7_inherits(m, tidyllm_video)) return(paste0("[video: ", m@videoname, "]"))
        if (S7_inherits(m, tidyllm_pdf))   return(paste0("[pdf: ", m@pdfname, "]"))
        if (is.list(m) && !is.null(m$filename)) return(paste0("[", tolower(m$type %||% "media"), ": ", m$filename, "]"))
        "[media]"
      }, character(1))
      cat(" ->", paste(media_labels, collapse = " "), "\n")
    }

    # Print remote file references if available
    if (!is.null(message$files) && length(message$files) > 0) {
      file_labels <- vapply(message$files, function(f) {
        sprintf("[file: %s (%s, %s)]", f@filename, f@provider, f@id)
      }, character(1))
      cat(" ->", paste(file_labels, collapse = " "), "\n")
    }
    
    # Print metadata if .meta is TRUE and metadata is available
    if (.meta && !is.null(message$meta)) {
      cat("\n  Metadata:\n")
      meta_fields <- names(message$meta)
      lapply(meta_fields[meta_fields!="specific_metadata"], function(key) {
        meta_value <- message$meta[[key]]
        # Ensure proper formatting of timestamp
        if (inherits(meta_value, "POSIXct")) {
          meta_value <- format(meta_value, "%Y-%m-%d %H:%M:%S")
        } else if (key == "timestamp" && is.numeric(meta_value)) {
          meta_value <- format(as.POSIXct(meta_value, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d %H:%M:%S")
        }
        
        cat(sprintf("    %s: %s\n", key, ifelse(is.null(meta_value), "NA", meta_value)))
      })
    }
    
    # Print message separator
    cat("--------------------------------------------------------------\n")
  })
  invisible(x)
}

method(remove_message, LLMMessage) <- function(.llm, .index) {
  # Validate index
  c(
    "Index must be a positive integer" = is_integer_valued(.index) && .index > 0,
    "Index is out of bounds" = .index <= length(.llm@message_history)) |> 
    validate_inputs()
  
  # Remove the message
  .llm@message_history <- .llm@message_history[-.index]
  .llm
}


#' Convert a LLMMessage Object to a Tibble
#'
#' Converts the `message_history` of an `LLMMessage` object into a tibble.
#'
#' @param x A `LLMMessage` object.
#' @param ... Additional arguments (ignored).
#' @param .rows Number of rows (ignored).
#' @param .name_repair Name repair strategy (ignored).
#' @param rownames Row names (ignored).
#' 
#' @return A tibble with two columns:
#'   - `role`: The role of the message sender (e.g., "user", "assistant", "system").
#'   - `content`: The message content.
#'   
#' @noRd 
method(as_tibble.LLMMessage, LLMMessage) <- function(x, ..., .rows = NULL, .name_repair = "check_unique", rownames = NULL) {
  purrr::map_dfr(x@message_history, ~{
    tibble::tibble(
      role = .x$role,
      content = .x$content
    )
  })
}

#' Pre-format Message for API format methods
#'
#' A helper function to process and format a message object for use with  `to_api_format` methods.
#'
#' @param message A list containing:
#'   - `content`: The base textual content of the message.
#'   - `media`: A list of media objects, where each object has `type` (e.g., "text", "image"), `filename` (for images), and `content` (the base64-encoded content or text content).
#'
#' @return A list formatted for APIs format. If `message$media` includes an image, the returned list contains:
#'   - `content`: The combined text content.
#'   - `image`: A sublist with:
#'     - `type`: Always `"base64"`.
#'     - `media_type`: The MIME type of the image (e.g., "image/png").
#'     - `data`: The base64-encoded image content.
#' If no image is present, the list only contains `content`.
#'
#' @keywords internal
#' @noRd
format_message <- function(message) {
  base_content <- message$content
  media_list   <- message$media
  text_media   <- extract_media(media_list, "text")
  image_media  <- extract_media(media_list, "image")

  combined_text <- paste(c(base_content, unlist(text_media)), collapse = " ")

  images <- lapply(image_media, function(img_item) {
    if (S7_inherits(img_item, tidyllm_image)) {
      mime <- guess_mime_type(img_item@imagename)
      # imagebase64 is stored as "data:{mime};base64,{data}" — strip the header
      b64 <- sub("^data:[^;]+;base64,", "", img_item@imagebase64)
      list(type = "base64", media_type = mime, data = b64)
    } else {
      # Legacy plain-list format: list(type="Image", content=base64, filename=...)
      list(type = "base64", media_type = guess_mime_type(img_item$filename), data = img_item$content)
    }
  })

  list(content = combined_text, images = images)
}

  



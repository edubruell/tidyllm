#' Is an input an integer valued numeric (needed to validate inputs)
#'
#' This internal function is a small helper used to validate inputs in functions
#'
#' @param .x An input that is checked to see whether it is an interger valued numeric
#' @return A logical
#' @noRd
is_integer_valued <- function(.x) {
  # Check if the input is numeric
  if (is.numeric(.x)) {
    # Further check if the numeric input is effectively an integer
    if (.x == as.integer(.x)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

#' Validate Input Conditions for R Functions
#'
#' This internal function validates specified conditions for function inputs and stops the function execution if any condition is not met. It uses a named vector of predicates where each name is the error message associated with the predicate condition.
#'
#' @param .predicates A named vector where each element is a logical condition and the name of each element is the corresponding error message to be displayed if the condition is FALSE.
#' @return None; the function will stop execution and throw an error if a validation fails.
#' @examples
#' validate_inputs(c(
#'   "Input must be numeric" = is.numeric(5),
#'   "Input must be integer" = 5 == as.integer(5)
#' ))
#' @noRd
validate_inputs <- function(.predicates) {
  # Use lapply to iterate over predicates and stop on the first failure
  results <- lapply(names(.predicates), function(error_msg) {
    if (!.predicates[[error_msg]]) {
      stop(error_msg)
    }
  })
}

#' Small helper function to recursively find whether a list contains a name
#'
#' @param .x A list to search within.
#' @param .name A character string of the name to search for.
#' @noRd
r_has_name <- function(.x, .name) {
  if (is.list(.x)) {
    return(.name %in% names(.x) || any(sapply(.x, r_has_name, .name = .name)))
  }
  FALSE
}

#' Helper to determine if some object is an ellmer type
#'
#' @param .x An input
#' @noRd
is_ellmer_type <- function(.x) {
  if (!requireNamespace("ellmer", quietly = TRUE)) return(FALSE)
  inherits(.x, c("ellmer::TypeBasic", "ellmer::TypeEnum", 
                "ellmer::TypeObject", "ellmer::TypeArray"))
}

# Helper function to filter mmessages by roles
#'
#' @param .message_hisotry A message history in the format used within LLMMessage
#' @param .roles A vector of roles (default: `c("user", "assistant")`)
#' @return A filtered message_history
#' @noRd
filter_roles = function(.message_history,
                        .roles = c("user", "assistant")) {
  Filter(function(x) "role" %in% names(x) && x$role %in% .roles, .message_history)
}

#' @title Guess MIME Type from File Extension
#' @description Internal utility function to guess the MIME type of a file based on its extension.
#' This function provides a mapping of common file extensions to their respective MIME types.
#' 
#' @param file_path A string representing the path to the file.
#' 
#' @return A character vector of possible MIME types, or "application/octet-stream" if the extension is unknown.
#' 
#' @importFrom rlang %||%
#' @noRd
guess_mime_type <- function(file_path) {
  # Extract the file extension
  ext <- tools::file_ext(file_path)
  
  # Define a mapping of extensions to MIME types
  mime_types <- list(
    pdf = "application/pdf",
    txt = "text/plain",
    html = "text/html",
    css = "text/css",
    md = "text/md",
    csv = "text/csv",
    xml = "text/xml",
    rtf = "text/rtf",
    js =  "text/javascript",
    py =  "text/x-python",
    png = "image/png",
    jpeg = "image/jpeg",
    jpg = "image/jpeg",
    webp = "image/webp",
    heic = "image/heic",
    heif = "image/heif",
    mp4 = "video/mp4",
    mpeg = "video/mpeg",
    mov = "video/mov",
    avi = "video/avi",
    flv = "video/x-flv",
    mpg = "video/mpg",
    webm = "video/webm",
    wmv = "video/wmv",
    `3gpp` = "video/3gpp",
    wav = "audio/wav",
    mp3 = "audio/mp3",
    aiff = "audio/aiff",
    aac = "audio/aac",
    ogg = "audio/ogg",
    flac = "audio/flac"
  )
  
  # Return the corresponding MIME type(s) or a default value
  mime_types[[tolower(ext)]] %||% "application/octet-stream"
}

#' Parse Embedding Input
#'
#' This function processes an .input for embedding functions, extracting and combining message content
#' and any associated text media. If the input is a character vector, it is returned as is.
#' 
#' @param .input The input object, either a character vector or a list containing message history.
#' @return A character vector containing the combined message texts.
#' @noRd
parse_embedding_input <- function(.input) {
  if (!is.character(.input)) {
   history <- Filter(function(x) {
      if ("role" %in% names(x)) {
        return(x$role %in% c("user", "assistant"))
      } else {
        return(FALSE)
      }
    }, .input@message_history)
    
    # Extract messages and combine content and text media
    message_texts <- purrr::map_chr(history, function(m) {
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
  } else {
    message_texts <- .input
  }
  
  return(message_texts)
}
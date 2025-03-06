#' Parse Embedding Input
#'
#' This function processes an .input for embedding functions, extracting and combining message content
#' and any associated text media. If the input is a character vector, it is returned as is. If the input mixes 
#' text and image objects it is returned as a multimodal embedding object.
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
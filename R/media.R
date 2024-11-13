
#' Extract  Media Content
#'
#' This internal function extracts and formats media content, specifically for Images or text formats. For PDF, Input files and RConsole types,
#' it formats these items into tagged strings suitable for inclusion in message content.
#'
#' @param .media_list A list of media items, each containing 'type', 'filename', and 'content' keys.
#' @param .type What kind of media should be extracted (text or image)
#' @return A list of media items with formatted strings based on the media type.
#' @importFrom glue glue
#' @noRd
extract_media <- function(.media_list,.type){
  
  if(.type=="text"){
    out <-  Filter(function(x){
      if ("type" %in% names(x)) {
        return(x$type %in% c("PDF","TextFile","RConsole"))
      } else {
        return(FALSE)
      }},.media_list) |>
      lapply(function(x){
        if(x$type=="PDF"){tagged <- glue::glue("<pdf filename=\"{x$filename}>{x$content}</pdf>\n")}
        if(x$type=="RConsole"){tagged <- glue::glue("<ROutput>{x$content}</ROutput>\n")}
        if(x$type=="TextFile"){tagged <- glue::glue("<file filename=\"{x$filename}>{x$content}</file>\n")}
        tagged
      })
  }
  
  if(.type=="image"){
    out <- Filter(function(x){
      if ("type" %in% names(x)) {
        return(x$type %in% c("Image"))
      } else {
        return(FALSE)
      }},.media_list) 
    
  }
  
  return(out)
}

#' Extract Response Metadata
#'
#' This helper function extracts and formats metadata from LLM API responses.
#' It standardizes the model used, timestamp of the response, and token usage statistics 
#' (prompt, completion, and total tokens) across various LLM API providers.
#'
#' @param .response A list containing the response object from an LLM API call.
#'                  The structure of this list may vary depending on the provider 
#' @return A named list containing the standardized metadata: model, timestamp, prompt tokens, 
#'         completion tokens, and total tokens.
#' @importFrom lubridate as_datetime
#' @importFrom rlang %||%
#' @noRd
extract_response_metadata <- function(.response) {
  timestamp <- .response$created_at %||% .response$created %||% NA
  timestamp <- if (is.numeric(timestamp)) {
    lubridate::as_datetime(timestamp)
  } else if (is.character(timestamp)) {
    lubridate::as_datetime(timestamp, format = "%Y-%m-%dT%H:%M:%OSZ")
  } else {
    NA
  }
  
  prompt_tokens <- .response$usage$prompt_tokens %||% 
    .response$usage$input_tokens %||% 
    .response$usageMetadata$promptTokenCount %||% NA
  
  completion_tokens <- .response$usage$completion_tokens %||% 
    .response$usage$output_tokens %||% 
    .response$usageMetadata$candidatesTokenCount %||% NA
  
  total_tokens <- .response$usage$total_tokens %||% 
    .response$usageMetadata$totalTokenCount %||% 
    if (!is.na(prompt_tokens) && !is.na(completion_tokens)) {
      prompt_tokens + completion_tokens
    } else {
      NA
    }
  
  list(
    model = .response$model %||% .response$modelVersion %||%  NA,
    timestamp = timestamp,
    prompt_tokens = prompt_tokens,
    completion_tokens = completion_tokens,
    total_tokens = total_tokens
  )
}

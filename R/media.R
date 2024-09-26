
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

#Future functions to extract media from responses or pre-process other media will go here. 
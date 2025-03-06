tidyllm_image <- S7::new_class(
  name = "tidyllm_image",
  properties = list(
    imagepath   = class_character,  # Where the image file is located
    imagename   = class_character,  # The basename of the file. Used for input labels in Embedding  tibbles
    imagebase64 = class_character   # Base64-encoded image data
  )
)


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

#' Create an  Image Object
#'
#' This function reads an image file from disk, encodes it in base64,
#' and returns a `tidyllm_image` object that can be used in multimodal 
#' embedding requests.
#'
#' @param .path The path to the image file on disk.
#'
#' @return An `tidyllm_image`, containing:
#'   - `imagepath`: The original file path
#'   - `imagename`: The basename of the image
#'   - `imagebase64`: a "data:image/...;base64,..." string
#' @export
img <- function(.path) {
  if (!file.exists(.path)) {
    stop("File does not exist: ", .path)
  }
  
  # Read file contents as raw, then base64-encode
  raw_file <- readBin(.path, what = "raw", n = file.size(.path))
  b64      <- base64enc::base64encode(raw_file)
  
  # Optionally guess MIME type from extension (e.g., "png" or "jpeg")
  # For simplicity, assume PNG here
  mime_type <- if (grepl("\\.jpe?g$", .path, ignore.case = TRUE)) {
    "image/jpeg"
  } else if (grepl("\\.png$", .path, ignore.case = TRUE)) {
    "image/png"
  } else {
    # fallback
    "application/octet-stream"
  }
  
  # Prepend the mime type
  b64_with_header <- paste0("data:", mime_type, ";base64,", b64)
  
  tidyllm_image(
    imagepath   = .path,
    imagename   = basename(.path),
    imagebase64 = b64_with_header
  )
}


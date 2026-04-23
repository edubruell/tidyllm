tidyllm_image <- S7::new_class(
  name = "tidyllm_image",
  properties = list(
    imagepath   = class_character,
    imagename   = class_character,
    imagebase64 = class_character
  )
)

tidyllm_audio <- S7::new_class(
  name = "tidyllm_audio",
  properties = list(
    audiopath = class_character,
    audioname = class_character,
    audiomime = class_character
  )
)

tidyllm_video <- S7::new_class(
  name = "tidyllm_video",
  properties = list(
    videopath = class_character,
    videoname = class_character,
    videomime = class_character
  )
)

tidyllm_pdf <- S7::new_class(
  name = "tidyllm_pdf",
  properties = list(
    pdfpath      = class_character,
    pdfname      = class_character,
    pdftext      = class_character,
    pages        = class_any,
    text_extract = class_logical
  )
)

tidyllm_file <- S7::new_class(
  name = "tidyllm_file",
  properties = list(
    id        = class_character,
    provider  = class_character,
    mime_type = class_character,
    filename  = class_character,
    uri       = class_character
  )
)

print.tidyllm_image <- S7::new_external_generic("base", "print", "x")
method(print.tidyllm_image, tidyllm_image) <- function(x, ...) {
  size_kb <- round(file.size(x@imagepath) / 1024, 1)
  cat(sprintf("<tidyllm_image> %s  (%.1f KB)\n", x@imagename, size_kb))
  invisible(x)
}

print.tidyllm_audio <- S7::new_external_generic("base", "print", "x")
method(print.tidyllm_audio, tidyllm_audio) <- function(x, ...) {
  size_kb <- round(file.size(x@audiopath) / 1024, 1)
  cat(sprintf("<tidyllm_audio> %s  [%s]  (%.1f KB)\n",
              x@audioname, x@audiomime, size_kb))
  invisible(x)
}

print.tidyllm_video <- S7::new_external_generic("base", "print", "x")
method(print.tidyllm_video, tidyllm_video) <- function(x, ...) {
  size_mb <- round(file.size(x@videopath) / 1024^2, 1)
  cat(sprintf("<tidyllm_video> %s  [%s]  (%.1f MB)\n",
              x@videoname, x@videomime, size_mb))
  invisible(x)
}

print.tidyllm_pdf <- S7::new_external_generic("base", "print", "x")
method(print.tidyllm_pdf, tidyllm_pdf) <- function(x, ...) {
  size_kb <- round(file.size(x@pdfpath) / 1024, 1)
  pages_str <- if (is.null(x@pages)) "all pages" else paste0("pages ", paste(range(x@pages), collapse = "-"))
  mode_str  <- if (x@text_extract) "text" else "binary"
  cat(sprintf("<tidyllm_pdf> %s  %s  [%s]  (%.1f KB)\n",
              x@pdfname, pages_str, mode_str, size_kb))
  invisible(x)
}

print.tidyllm_file <- S7::new_external_generic("base", "print", "x")
method(print.tidyllm_file, tidyllm_file) <- function(x, ...) {
  cat(sprintf("<tidyllm_file [%s] id=%s mime=%s file=%s>\n",
              x@provider, x@id, x@mime_type, x@filename))
  invisible(x)
}


#' Extract  Media Content
#'
#' This internal function extracts and formats media content, specifically for Images or text formats. For PDF, Input files and RConsole types,
#' it formats these items into tagged strings suitable for inclusion in message content.
#'
#' @param .media_list A list of media items, each either a tidyllm S7 object or a legacy named list with 'type', 'filename', and 'content' keys.
#' @param .type What kind of media should be extracted (text or image)
#' @return A list of media items with formatted strings based on the media type.
#' @importFrom glue glue
#' @noRd
extract_media <- function(.media_list, .type) {
  if (is.null(.media_list) || length(.media_list) == 0) return(list())

  if (.type == "text") {
    out <- Filter(Negate(is.null), lapply(.media_list, function(x) {
      if (S7_inherits(x, tidyllm_pdf)) {
        tagged <- glue::glue("<pdf filename=\"{x@pdfname}\">{x@pdftext}</pdf>\n")
        return(tagged)
      }
      if (is.list(x) && !is.null(x$type)) {
        if (x$type == "PDF") {
          return(glue::glue("<pdf filename=\"{x$filename}\">{x$content}</pdf>\n"))
        }
        if (x$type == "RConsole") {
          return(glue::glue("<ROutput>{x$content}</ROutput>\n"))
        }
        if (x$type == "TextFile") {
          return(glue::glue("<file filename=\"{x$filename}\">{x$content}</file>\n"))
        }
      }
      NULL
    }))
  } else if (.type == "image") {
    out <- Filter(Negate(is.null), lapply(.media_list, function(x) {
      if (S7_inherits(x, tidyllm_image)) return(x)
      if (is.list(x) && !is.null(x$type) && x$type == "Image") return(x)
      NULL
    }))
  } else if (.type == "audio") {
    out <- Filter(Negate(is.null), lapply(.media_list, function(x) {
      if (S7_inherits(x, tidyllm_audio)) return(x)
      NULL
    }))
  } else if (.type == "video") {
    out <- Filter(Negate(is.null), lapply(.media_list, function(x) {
      if (S7_inherits(x, tidyllm_video)) return(x)
      NULL
    }))
  } else {
    out <- list()
  }

  return(out)
}

#' Create an Image Object
#'
#' This function reads an image file from disk, encodes it in base64,
#' and returns a `tidyllm_image` object that can be used in multimodal
#' embedding requests or attached to messages via `.media`.
#'
#' @param .path The path to the image file on disk.
#'
#' @return A `tidyllm_image` object.
#' @export
img <- function(.path) {
  if (!file.exists(.path)) {
    stop("File does not exist: ", .path)
  }

  raw_file <- readBin(.path, what = "raw", n = file.size(.path))
  b64      <- base64enc::base64encode(raw_file)

  mime_type <- if (grepl("\\.jpe?g$", .path, ignore.case = TRUE)) {
    "image/jpeg"
  } else if (grepl("\\.png$", .path, ignore.case = TRUE)) {
    "image/png"
  } else {
    "application/octet-stream"
  }

  b64_with_header <- paste0("data:", mime_type, ";base64,", b64)

  tidyllm_image(
    imagepath   = .path,
    imagename   = basename(.path),
    imagebase64 = b64_with_header
  )
}

#' Create an Audio Object
#'
#' Stores a reference to a local audio file for use in multimodal messages.
#' The file is not encoded until the message is formatted for a provider.
#'
#' @param .path The path to the audio file on disk.
#' @return A `tidyllm_audio` object.
#' @export
audio_file <- function(.path) {
  if (!file.exists(.path)) {
    stop("File does not exist: ", .path)
  }
  tidyllm_audio(
    audiopath = .path,
    audioname = basename(.path),
    audiomime = guess_mime_type(.path)
  )
}

#' Create a Video Object
#'
#' Stores a reference to a local video file for use in multimodal messages.
#' The file is not encoded until the message is formatted for a provider.
#'
#' @param .path The path to the video file on disk.
#' @return A `tidyllm_video` object.
#' @export
video_file <- function(.path) {
  if (!file.exists(.path)) {
    stop("File does not exist: ", .path)
  }
  tidyllm_video(
    videopath = .path,
    videoname = basename(.path),
    videomime = guess_mime_type(.path)
  )
}

#' Create a PDF Object
#'
#' Stores a reference to a local PDF file along with a pdftools text extraction.
#' Binary encoding is deferred to format time; providers that support binary PDF
#' encode on demand, others use the stored text fallback.
#'
#' @param .path The path to the PDF file on disk.
#' @param pages An integer vector of page numbers to include (e.g. `1:3`). NULL uses all pages.
#' @param .text_extract Logical; if TRUE always use pdftools text extraction regardless of provider
#'   (equivalent to the old `.pdf` parameter behaviour). Default FALSE uses binary-first.
#' @param engine Optional engine name (e.g. `"mistral-ocr"`); a warning is issued if the provider
#'   does not support binary PDF and the engine choice cannot be honoured.
#' @return A `tidyllm_pdf` object.
#' @export
pdf_file <- function(.path, pages = NULL, .text_extract = FALSE, engine = NULL) {
  if (!file.exists(.path)) {
    stop("File does not exist: ", .path)
  }
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("The 'pdftools' package is required for pdf_file(). Please install it.")
  }

  if (!is.null(pages)) {
    all_text <- pdftools::pdf_text(.path)
    valid_pages <- pages[pages >= 1 & pages <= length(all_text)]
    pdftext <- paste(all_text[valid_pages], collapse = "\n")
  } else {
    pdftext <- paste(pdftools::pdf_text(.path), collapse = "\n")
  }

  tidyllm_pdf(
    pdfpath      = .path,
    pdfname      = basename(.path),
    pdftext      = pdftext,
    pages        = pages,
    text_extract = .text_extract
  )
}

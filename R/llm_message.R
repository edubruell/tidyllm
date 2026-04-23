#' Create or Update Large Language Model Message Object
#'
#' This function creates a new `LLMMessage` object or updates an existing one.
#' It supports adding text prompts and various media types, such as images, PDFs, text files, or plots.
#'
#' @param .llm An existing LLMMessage object or an initial text prompt.
#' @param .prompt Text prompt to add to the message history.
#' @param .role The role of the message sender, typically "user" or "assistant".
#' @param .system_prompt Default system prompt if a new LLMMessage needs to be created.
#' @param .media An inline media object or a list of them. Accepted types: `img()`, `audio_file()`, `video_file()`, `pdf_file()`.
#' @param .files A `tidyllm_file` object or list of them returned by `upload_file()`. These are remote file references stored on a provider's server.
#' @param .imagefile Path to an image file to be attached (optional). Deprecated; use `.media = img(path)` instead.
#' @param .pdf Path to a PDF file to be attached (optional). Deprecated; use `.media = pdf_file(path)` instead.
#' @param .textfile Path to a text file to be read and attached (optional).
#' @param .capture_plot Boolean to indicate whether a plot should be captured and attached as an image (optional).
#' @param .f An R function or an object coercible to a function via `rlang::as_function`, whose output should be captured and attached (optional).
#' @return Returns an updated or new LLMMessage object.
#' @importFrom jsonlite toJSON
#' @importFrom base64enc base64encode
#' @family Message Creation Utilities
#' @seealso [df_llm_message()]
#' @export
llm_message <- function(.llm = NULL,
                        .prompt = NULL,
                        .role = "user",
                        .system_prompt = "You are a helpful assistant",
                        .media = NULL,
                        .files = NULL,
                        .imagefile = NULL,
                        .pdf = NULL,
                        .textfile = NULL,
                        .capture_plot = FALSE,
                        .f = NULL) {

  media_list <- list()
  files_list <- NULL

  validate_inputs(c(
    ".llm must be NULL, a character vector, or an LLMMessage object" = is.null(.llm) || is.character(.llm) || S7_inherits(.llm, LLMMessage),
    ".prompt must be a non-empty string if provided" = is.null(.prompt) || (is.character(.prompt) && length(.prompt) == 1 && nchar(.prompt) > 0),
    ".role must be a non-empty string" = is.character(.role) && length(.role) == 1 && nchar(.role) > 0,
    ".system_prompt must be a string" = is.character(.system_prompt),
    ".imagefile must be NULL or a valid file path" = is.null(.imagefile) || (is.character(.imagefile) && length(.imagefile) == 1 && file.exists(.imagefile)),
    ".pdf must be NULL, a file path, or a list with filename, start_page, and end_page" = is.null(.pdf) ||
      (is.character(.pdf) && length(.pdf) == 1 && file.exists(.pdf)) ||
      (is.list(.pdf) && all(c("filename", "start_page", "end_page") %in% names(.pdf)) &&
         is.character(.pdf$filename) && length(.pdf$filename) == 1 && file.exists(.pdf$filename) &&
         is.numeric(.pdf$start_page) && is.numeric(.pdf$end_page) && .pdf$start_page >= 1 && .pdf$end_page >= .pdf$start_page),
    ".textfile must be NULL or a valid file path" = is.null(.textfile) || (is.character(.textfile) && length(.textfile) == 1 && file.exists(.textfile)),
    ".capture_plot must be logical" = is.logical(.capture_plot) && length(.capture_plot) == 1,
    ".f must be NULL or coercible to a function via rlang::as_function" = is.null(.f) || (tryCatch({rlang::as_function(.f); TRUE}, error = function(e) FALSE))
  ))

  pre_existing_object <- S7_inherits(.llm, LLMMessage)

  # Handle .media — accept a single S7 media object or a list
  if (!is.null(.media)) {
    media_items <- if (is.list(.media) && !S7_inherits(.media, tidyllm_image) &&
                       !S7_inherits(.media, tidyllm_audio) && !S7_inherits(.media, tidyllm_video) &&
                       !S7_inherits(.media, tidyllm_pdf)) {
      .media
    } else {
      list(.media)
    }
    media_list <- c(media_list, media_items)
  }

  # Handle .files — accept a tidyllm_file or list of tidyllm_file
  if (!is.null(.files)) {
    files_list <- if (S7_inherits(.files, tidyllm_file)) list(.files) else .files
  }

  # Handle .imagefile (deprecated)
  if (!is.null(.imagefile) || .capture_plot) {
    if (.capture_plot) {
      plot_file <- tempfile(fileext = ".png")
      dev.copy(png, filename = plot_file)
      dev.off()
      .imagefile <- plot_file
    }
    lifecycle::deprecate_warn("0.5.0", "llm_message(.imagefile=)", "llm_message(.media=)",
      details = "Replace with .media = img(path).")
    media_list <- c(media_list, list(img(.imagefile)))
  }

  # Handle .pdf (deprecated)
  if (!is.null(.pdf)) {
    lifecycle::deprecate_warn("0.5.0", "llm_message(.pdf=)", "llm_message(.media=)",
      details = "Replace with .media = pdf_file(path, .text_extract = TRUE) to preserve identical behaviour.")
    if (!requireNamespace("pdftools", quietly = TRUE)) {
      stop("The 'pdftools' package is required to read PDF files. Please install it.")
    }
    if (is.character(.pdf) && length(.pdf) == 1) {
      media_list <- c(media_list, list(pdf_file(.pdf, .text_extract = TRUE)))
    } else if (is.list(.pdf)) {
      pdf_path   <- .pdf$filename
      start_page <- .pdf$start_page
      end_page   <- .pdf$end_page
      pages      <- seq(start_page, end_page)
      media_list <- c(media_list, list(pdf_file(pdf_path, pages = pages, .text_extract = TRUE)))
    } else {
      stop(".pdf must be either a file path or a list with filename, start_page, and end_page.")
    }
  }

  # Handle .textfile
  if (!is.null(.textfile)) {
    text_content <- readLines(.textfile) |>
      stringr::str_c(collapse = "\n")
    media_list <- c(media_list, list(list(type = "TextFile", content = text_content, filename = basename(.textfile))))
  }

  # Handle .f
  if (!is.null(.f)) {
    output <- utils::capture.output(rlang::as_function(.f)(), file = NULL) |>
      stringr::str_c(collapse = "\n")
    media_list <- c(media_list, list(list(type = "RConsole", content = output, filename = "RConsole.txt")))
  }

  media_arg <- if (length(media_list) > 0) media_list else NULL

  if (is.character(.llm)) {
    initial_prompt <- .llm
    .llm <- LLMMessage(system_prompt = .system_prompt,
                       message_history = list(list(role = "system", content = .system_prompt)))
    if (is.null(.prompt)) {
      return(add_message(.llm, .role, initial_prompt, .media = media_arg, .files = files_list))
    }
  }

  if (is.null(.llm)) {
    .llm <- LLMMessage(system_prompt = .system_prompt,
                       message_history = list(list(role = "system", content = .system_prompt)))
  }

  if (!is.null(.prompt)) {
    return(add_message(.llm, .role, .prompt, .media = media_arg, .files = files_list))
  }
}

#' Convert a Data Frame to an LLMMessage Object
#'
#' This function converts a data frame into an `LLMMessage` object representing a conversation history.
#' The data frame must have specific columns (`role` and `content`), with each row representing a message.
#'
#' @param .df A data frame with at least two rows and columns `role` and `content`.
#'   The `role` column should contain "user", "assistant", or "system". The `content` column should contain the corresponding message text.
#' @return An `LLMMessage` object representing the structured conversation.
#' @family Message Creation Utilities
#' @seealso [llm_message()]
#' @export
df_llm_message <- function(.df){
  #Validate the inputs
  c(
    "Input .df must be a dataframe" = is.data.frame(.df),
    "Input .df must contain the columns role and content" = (sum(c("role","content") %in% names(.df))==2),
    "Input .df column content must be character" = is.character(.df$content),
    "Input .df column role  must be character" = is.character(.df$role),
    "Input .df must have at least two rows" = nrow(.df)>=2,
    "Only user, system and assistant are allowed as values of .df column role" =all(.df$role %in% c("user","assistant","system"))
  ) |>
    validate_inputs()
  
  # Splitting the dataframe into a list of single-row dataframes
  single_rows <- split(.df, seq(nrow(.df)))
  #Get the system prompt and the first message into a message history
  if(single_rows[[1]]$role=="system"){
    initial_message <- llm_message(.llm = single_rows[[2]]$content,.system_prompt = single_rows[[1]]$content)
  } else {
    initial_message <- llm_message(.llm = single_rows[[1]]$content)
  }
  
  #We are done if it is just a two-row data.frame
  if(nrow(.df)<=2){final_message <- initial_message}
  
  #For more than 3 rows we iteratively apply the messages to a message history
  if(nrow(.df)>2){
    last_output <- initial_message
    for (i in 3:nrow(.df)) {
      current_row <- single_rows[[i]]
      last_output <- add_message(llm = last_output,
                                 role = current_row$role,
                                 content=current_row$content)
    }
    final_message <- last_output
  }
  return(final_message)
}






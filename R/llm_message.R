#' Create or Update Large Language Model Message Object
#'
#' This function creates a new `LLMMessage` object or updates an existing one.
#' It supports adding text prompts and various media types, such as images, PDFs, text files, or plots.
#'
#' @param .llm An existing LLMMessage object or an initial text prompt.
#' @param .prompt Text prompt to add to the message history.
#' @param .role The role of the message sender, typically "user" or "assistant".
#' @param .system_prompt Default system prompt if a new LLMMessage needs to be created.
#' @param .imagefile Path to an image file to be attached (optional).
#' @param .pdf Path to a PDF file to be attached (optional). Can be a character vector of length one (file path), or a list with `filename`, `start_page`, and `end_page`.
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
                        .imagefile = NULL, 
                        .pdf = NULL, 
                        .textfile = NULL, 
                        .capture_plot = FALSE,
                        .f = NULL) {
  
  # Handle media attached to messages
  media_list = list()
  
  # Validate inputs using the existing validate_inputs function
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
  
  # Early check whether an llm object existed before
  pre_existing_object <- S7_inherits(.llm, LLMMessage)
  
  # Handle images or captured plots
  if (!is.null(.imagefile) || .capture_plot) {
    if (.capture_plot) {
      # Capturing plot logic to temporary file
      plot_file <- tempfile(fileext = ".png")
      dev.copy(png, filename = plot_file)
      dev.off()
      .imagefile <- plot_file
    }
    # Validate and encode the image file
    valid_types <- c("jpeg", "jpg", "png")
    file_type <- tools::file_ext(.imagefile)
    if (!(file_type %in% valid_types)) {
      stop("Unsupported file type. Only JPEG and PNG are allowed.")
    }
    image_base64 <- base64enc::base64encode(.imagefile)
    media_list <- c(media_list, list(list(type = "Image", content = image_base64, filename = basename(.imagefile))))
  }
  
  # Handle PDFs
  if (!is.null(.pdf)) {
    if (!requireNamespace("pdftools", quietly = TRUE)) {
      stop("The 'pdftools' package is required to read PDF files. Please install it.")
    }
    if (is.character(.pdf) && length(.pdf) == 1) {
      # .pdf is a path to a PDF file
      pdf_file <- .pdf
      start_page <- 1
      pdf_info <- pdftools::pdf_info(pdf_file)
      total_pages <- pdf_info$pages
      end_page <- total_pages
    } else if (is.list(.pdf)) {
      # .pdf is a list with filename, start_page, end_page
      pdf_file <- .pdf$filename
      start_page <- .pdf$start_page
      end_page <- .pdf$end_page
      if (start_page < 1) {
        warning("start_page is less than 1. Setting start_page to 1.")
        start_page <- 1
      }
      pdf_info <- pdftools::pdf_info(pdf_file)
      total_pages <- pdf_info$pages
      if (end_page > total_pages) {
        warning("end_page is greater than total pages. Setting end_page to total pages.")
        end_page <- total_pages
      }
    } else {
      stop(".pdf must be either a file path (character vector of length 1) or a list with filename, start_page, and end_page.")
    }
    # Read the specified pages
    pdf_text <- pdftools::pdf_text(pdf_file)[start_page:end_page] |> 
      stringr::str_c(collapse = "\n")
    media_list <- c(media_list, list(list(type = "PDF", content = pdf_text, filename = basename(pdf_file))))
  }
  
  # Handle text files
  if (!is.null(.textfile)) {
    text_content <- readLines(.textfile) |> 
      stringr::str_c(collapse = "\n")
    media_list <- c(media_list, list(list(type = "TextFile", content = text_content, filename = basename(.textfile))))
  }
  
  # Handle function text outputs
  if (!is.null(.f)) {
    output <- utils::capture.output(rlang::as_function(.f)(), file = NULL) |> 
      stringr::str_c(collapse = "\n")
    media_list <- c(media_list, list(list(type = "RConsole", content = output, filename = "RConsole.txt")))
  }
  
  # If a character vector is supplied instead of an llm, use it as an initial prompt
  if (is.character(.llm)) {
    initial_prompt <- .llm
    .llm <- LLMMessage(system_prompt = .system_prompt,
                       message_history = list(
                         list(role="system", content = .system_prompt)
                       )
    )
    if (is.null(.prompt)) {
      llm <- add_message(.llm,.role, initial_prompt, media_list)
      return(llm)
    }
  }
  
  # Set up a new llm if it is null with the system prompt
  if (is.null(.llm)) {
    .llm <- LLMMessage(system_prompt = .system_prompt,
                       message_history = list(
                         list(role="system", content = .system_prompt)
                       )
    )
  }
  
  # Explicit prompts have precedence over ones supplied via .llm
  if (!is.null(.prompt) && !pre_existing_object) {
    llm <- add_message(.llm,.role, .prompt, media_list)
    return(llm)
  }
  
  if (!is.null(.prompt) && pre_existing_object) {
    llm <- add_message(.llm,.role, .prompt, media_list)
    return(llm)
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






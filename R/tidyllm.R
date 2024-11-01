

#' Large Language Model Message Class
#'
#' This class manages a history of messages and media interactions intended for use with large language  models.
#' It allows for adding messages, converting messages for API usage, and printing the history in a structured format.
#'
LLMMessage <- R6::R6Class(
  "LLMMessage",
  public = list(
    
    #' @field message_history List to store all message interactions.
    message_history = list(),
    
    #' @field system_prompt The system prompt used for a conversation
    system_prompt = "You are a helpful assistant",
    
    #' @description
    #' Initializes the LLMMessage object with an optional system prompt.
    #' 
    #' @param system_prompt A string that sets the initial system prompt.
    #' 
    #' @return A new LLMMessage object.
    initialize = function(system_prompt = "You are a helpful assistant") {
      self$system_prompt <- system_prompt
      self$add_message("system", self$system_prompt)
    },
    
    
    #' Deep Clone of LLMMessage Object
    #'
    #' This method creates a deep copy of the `LLMMessage` object. It ensures that 
    #' all internal states, including message histories and settings, are copied 
    #' so that the original object remains unchanged when mutations are applied 
    #' to the copy. This is particularly useful for maintaining immutability in 
    #' a tidyverse-like functional programming context where functions should 
    #' not have side effects on their inputs.
    #'
    #' @return A new `LLMMessage` object that is a deep copy of the original.
    clone_deep = function() {
      new_copy <- LLMMessage$new(self$system_prompt)
      new_copy$message_history <- rlang::duplicate(self$message_history, shallow = FALSE)
      return(new_copy)
    },
    
    
    #' Add a message
    #' 
    #' Adds a message to the history. Optionally includes media.
    #' @param role The role of the message sender (e.g., "user", "assistant").
    #' @param content The textual content of the message.
    #' @param media Optional; media content to attach to the message.
    #' @param json Is the message a raw string that contains a json response?
    add_message = function(role, content, media = NULL,json=FALSE) {
      message_details <- list(role = role, content = content,json=json)
      if (!is.null(media)) {
        message_details$media <- media
      }
      self$message_history <- c(self$message_history, list(message_details))
      invisible(self)
    },
    
    #' Convert to API format
    #' 
    #' Converts the message history to a format suitable for various API calls.
    #' @param api_type The type of API (e.g., "claude","groq","openai").
    #' @param cgpt_image_detail Specific option for ChatGPT API (imagedetail - set to auto)
    #' @param no_system Without system prompt (default: FALSE)
    #' 
    #' @return A message history in the target API format
    to_api_format = function(api_type, cgpt_image_detail = "auto", no_system = FALSE) {
        
        # Helper function within to_api_format
        format_message <- function(message) {
          base_content <- message$content
          media_list <- message$media
          text_media <- extract_media(media_list, "text")
          image_media <- extract_media(media_list, "image")
          
          combined_text <- paste(base_content, text_media, sep = " ")
          
          if (length(image_media) > 0) {
            image_file_type <- paste("image", tools::file_ext(image_media[[1]]$filename), sep = "/")
            base64_image <- image_media[[1]]$content
            
            list(
              content = combined_text,
              image = list(type = "base64", media_type = image_file_type, data = base64_image)
            )
          } else {
            list(content = combined_text)
          }
        }
        
        # Define specific message histories per API requirements
        openai_history <- if (no_system) filter_roles(self$message_history, c("user", "assistant")) else self$message_history
        claude_history <- filter_roles(self$message_history, c("user", "assistant"))
        
        # Switch logic for API types
        switch(api_type,
               "claude" = {
                 lapply(claude_history, function(m) {
                   formatted_message <- format_message(m)
                   if (!is.null(formatted_message$image)) {
                     list(role = m$role, content = list(
                       list(type = "image", source = formatted_message$image),
                       list(type = "text", text = formatted_message$content)
                     ))
                   } else {
                     list(role = m$role, content = list(
                       list(type = "text", text = formatted_message$content)
                     ))
                   }
                 })
               },
               "openai" = {
                 lapply(openai_history, function(m) {
                   formatted_message <- format_message(m)
                   if (!is.null(formatted_message$image)) {
                     list(
                       role = m$role,
                       content = list(
                         list(type = "text", text = formatted_message$content),
                         list(type = "image_url", image_url = list(
                           url = glue::glue("data:{formatted_message$image$media_type};base64,{formatted_message$image$data}")
                         ))
                       )
                     )
                   } else {
                     list(role = m$role, content = formatted_message$content)
                   }
                 })
               },
               "ollama" = {
                 ollama_history <- filter_roles(self$message_history, c("user", "assistant"))
                 lapply(ollama_history, function(m) {
                   formatted_message <- format_message(m)
                   if (!is.null(formatted_message$image)) {
                     list(
                       role = m$role,
                       content = formatted_message$content,
                       images = list(glue::glue("{formatted_message$image$data}"))
                     )
                   } else {
                     list(role = m$role, content = formatted_message$content)
                   }
                 })
               },
               stop("Unknown API-Format specified")
        )
    },
    
    #' Simple helper function to determine whether the message history contains 
    #' an image
    #' We check this function whenever we call models that do not support images
    #' so we can post a warning to the user that images were found but not sent
    #' to the model
    #' @return Returns TRUE if the message hisotry contains images
    has_image= function() {
    
      sapply(self$message_history, function(m){
         #Get the relevant media for the current message
         image_list <- extract_media(m$media,"image")
         
         
         if(length(image_list)>0){
           return(TRUE)
         } else {
           return(FALSE)
         } 
        }) |> max() |> as.logical()
    },
    
    #' Remove a Message by Index
    #'
    #' Removes a message from the message history at the specified index.
    #' @param index A positive integer indicating the position of the message to remove.
    #' @return The `LLMMessage` object, invisibly.
    remove_message = function(index) {
      # Validate index
      c(
        "Index must be a positive integer" = is_integer_valued(index) && index > 0,
        "Index is out of bounds" = index <= length(self$message_history)
      ) |> validate_inputs()
      
      # Remove the message
      self$message_history <- self$message_history[-index]
      invisible(self)
    },
    #' @description
    #' Prints the current message history in a structured format.
    print = function() {
      cat("Message History:\n")
      lapply(self$message_history, function(message) {
        #Print role and content
        cat(sprintf("%s: %s\n", message$role, message$content))
        #Print attachments
        if (!is.null(message$media)) {
          media_files <- sapply(message$media, function(media) media$filename)
          if(length(media_files)!=0){
            cat(" -> Attached Media Files: ", paste(media_files, collapse=", "), "\n")
          }
        }
        #Print message separator
        cat("--------------------------------------------------------------\n") 
      })
    }
  )
)

#' Create or Update Large Language Model Message Object
#'
#' This function allows the creation of a new LLMMessage object or the updating of an existing one.
#' It can handle the addition of text prompts and various media types such as images, PDFs, text files, or plots.
#' The function includes input validation to ensure that all provided parameters are in the correct format.
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
#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#' @importFrom base64enc base64encode
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
    ".llm must be NULL, a character vector, or an LLMMessage object" = is.null(.llm) || is.character(.llm) || inherits(.llm, "LLMMessage"),
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
  pre_existing_object <- inherits(.llm, "LLMMessage")
  
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
    .llm <- LLMMessage$new(.system_prompt)
    if (is.null(.prompt)) {
      .llm$add_message(.role, initial_prompt, media_list)
      return(.llm)
    }
  }
  
  # Set up a new llm if it is null with the system prompt
  if (is.null(.llm)) {
    .llm <- LLMMessage$new(.system_prompt)
  }
  
  # Explicit prompts have precedence over ones supplied via .llm
  if (!is.null(.prompt) && !pre_existing_object) {
    .llm$add_message(.role, .prompt, media_list)
    return(.llm)
  }
  
  # Deep copy output so original object is never changed
  if (!is.null(.prompt) && pre_existing_object) {
    llm_copy <- .llm$clone_deep()
    llm_copy$add_message(.role, .prompt, media_list)
    return(llm_copy)
  }
}





#' Convert a Data Frame to an LLMMessage Object
#'
#' This function takes a data frame and converts it into an LLMMessage object
#' representing a conversation history. The data frame should contain specific
#' columns (`role` and `content`) with each row representing a message in the
#' conversation.
#'
#' @param .df A data frame with at least two rows and columns `role` and `content`.
#'            The column `role` should contain values from "user", "assistant", or "system",
#'            and `content` should be of type character.
#'
#' @return An LLMMessage object containing the structured messages as per the input data frame.
#'
#'
#' @export
df_llm_message <- function(.df){
  #Validate the inputs
  c(
    "Input .df must be a dataframe" = is.data.frame(.df),
    "Input .df must contain the columns role and content" = (sum(c("role","content") %in% names(.df))==2),
    "Input .df column content must be character" = is.character(.df$content),
    "Input .df column role  must be character" = is.character(.df$role),
    "Input .df must have at least two rows" = nrow(.df)>2,
    "Only user, system and assistant are allowed as values of .df column role" =all(.df$role %in% c("user","assistant","system"))
  ) |>
    validate_inputs()
  
  # Splitting the dataframe into a list of single-row dataframes
  single_rows <- split(.df, seq(nrow(.df)))
  #Get the system prompt and the first message into a message history
  initial_message <- llm_message(.llm = single_rows[[2]]$content,.system_prompt = single_rows[[1]]$content)
  
  #We are done if it is just a two-row data.frame
  if(nrow(.df)<=2){final_message <- initial_message}
  
  #For more than 3 rows we iteratively apply the messages to a message history
  if(nrow(.df)>2){
    last_output <- initial_message
    for (i in 3:nrow(.df)) {
      current_row <- single_rows[[i]]
      last_output$add_message(role = current_row$role,content=current_row$content)
    }
    final_message <- last_output
  }
  return(final_message)
}



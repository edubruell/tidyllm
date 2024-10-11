

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
    add_message = function(role, content, media = NULL) {
      message_details <- list(role = role, content = content)
      if (!is.null(media)) {
        message_details$media <- media
      }
      self$message_history <- c(self$message_history, list(message_details))
      invisible(self)
    },
    
    #' Convert to API format
    #' 
    #' Converts the message history to a format suitable for various API calls.
    #' @param api_type The type of API (e.g., "claude","groq","chatgpt").
    #' @param cgpt_image_detail Specific option for ChatGPT API (imagedetail - set to auto)
    to_api_format = function(api_type,cgpt_image_detail="auto") {
      switch(api_type,
             "claude" = {
               claude_history <- Filter(function(x){
                 if ("role" %in% names(x)) {
                   return(x$role %in% c("user","assistant"))
                 } else {
                   return(FALSE)
                 }},self$message_history)
               
               lapply(claude_history, function(m){
                 #The basic text content supplied with the prompt
                 base_content <- m$content
                 
                 #Get the relevant media for the current message
                 media_list <- m$media
                 
                 #Extract the text content in put it into tags that are put before 
                 #the main text of the prompt
                 text_media  <- extract_media(media_list,"text")
                 image_media <- extract_media(media_list,"image")
                 
                 
                 if(length(image_media)>0){
                   
                   image_file_type <- paste("image",
                                            tools::file_ext(image_media[[1]]$filename), 
                                            sep = "/")
                   
                   # Add image content to the user content
                   output <- list(role = m$role,
                                  content = list(
                                    list(type = "image",
                                         source = list(
                                           type = "base64",
                                           media_type = image_file_type,
                                           data = image_media[[1]]$content
                                         )),
                                    list(type = "text", text = paste(base_content,text_media)))
                   )
                 } else {
                   # Text only content
                   output <- list(role = m$role, 
                                  content = list(
                                    list(type = "text", 
                                         text = paste(base_content,text_media))
                                  )
                   )
                 }
                 
                 output
               }) 
             },
             "groq" = {
               lapply(self$message_history, function(m){
                 #The basic text content supplied with the prompt
                 base_content <- m$content
                 
                 #Get the relevant media for the current message
                 media_list <- m$media
                 
                 #Extract the text content in put it into tags that are put before 
                 #the main text of the prompt
                 text_media <- extract_media(media_list,"text")
                 
                 # Text only content
                 list(role    = m$role, 
                      content = paste(base_content,text_media))
               }) 
             },
             "chatgpt" = {
               lapply(self$message_history, function(m) {
                 # The basic text content supplied with the prompt
                 base_content <- m$content
                 
                 # Get the relevant media for the current message
                 media_list <- m$media
                 
                 # Extract the text content and put it into tags that are put before 
                 # the main text of the prompt
                 text_media <- extract_media(media_list, "text")
                 image_media <- extract_media(media_list, "image")
                 
                 # Combine text content
                 combined_text <- paste(base_content, text_media, sep=" ")
                 
                 if (length(image_media) > 0) {
                   # Determine the MIME type based on file extension
                   image_file_type <- paste("image", tools::file_ext(image_media[[1]]$filename), sep="/")
                   
                   # Use the pre-encoded base64 image content
                   base64_image <- image_media[[1]]$content
                   
                   # Add image content to the user content
                   output <- list(
                     role = m$role,
                     content = list(
                       list(type = "text", text = combined_text),
                       list(type = "image_url", image_url = list(
                         url = glue::glue("data:{image_file_type};base64,{base64_image}")
                       ))
                     )
                   )
                 } else {
                   # Text-only content
                   output <- list(
                     role = m$role,
                     content = combined_text
                   )
                 }
                 
                 output
               })
            
              },
             "ollama"={
               ollama_history <- Filter(function(x){
                 if ("role" %in% names(x)) {
                   return(x$role %in% c("user","assistant"))
                 } else {
                   return(FALSE)
                 }},self$message_history)
               
               lapply(ollama_history, function(m) {
                 # The basic text content supplied with the prompt
                 base_content <- m$content
                 
                 # Get the relevant media for the current message
                 media_list <- m$media
                 
                 # Extract the text content and put it into tags that are put before 
                 # the main text of the prompt
                 text_media <- extract_media(media_list, "text")
                 image_media <- extract_media(media_list, "image")
                 
                 # Combine text content
                 combined_text <- paste(base_content, text_media, sep=" ")
                 
                 if (length(image_media) > 0) {
                   # Determine the MIME type based on file extension
                   image_file_type <- paste("image", tools::file_ext(image_media[[1]]$filename), sep="/")
                   
                   # Use the pre-encoded base64 image content
                   base64_image <- image_media[[1]]$content
                   
                   # Add image content to the user content
                   output <- list(
                     role = m$role,
                     content = combined_text,
                     images  = list(glue::glue("{base64_image}"))
                   )
                 } else {
                   # Text-only content
                   output <- list(
                     role = m$role,
                     content = combined_text
                   )
                 }
                 
                 output
               })
             }
             # Additional cases as needed
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
#'
#' @param .llm An existing LLMMessage object or an initial text prompt.
#' @param .prompt Text prompt to add to the message history.
#' @param .role The role of the message sender, typically "user" or "assistant".
#' @param .system_prompt Default system prompt if a new LLMMessage needs to be created.
#' @param .imagefile Path to an image file to be attached (optional).
#' @param .pdf Path to a PDF file to be attached (optional).
#' @param .textfile Path to a text file to be read and attached (optional).
#' @param .capture_plot Boolean to indicate whether a plot should be captured and attached as an image (optional).
#' @param .f An R function whose output should be captured and attached (optional).
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
  
  # If .llm is provided, check if it is an LLMMessage object
  if (!is.character(.llm) & !inherits(.llm, "LLMMessage")) {
    stop("Input .llm must be an LLMMessage object or an initial text prompt.")
  }
  
  # Early check whether an llm object existed before
  pre_existing_object <- TRUE
  if (inherits(.llm, "LLMMessage")) {
    pre_existing_object <- TRUE
  }
  
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
    pdf_text <- pdftools::pdf_text(.pdf) |> 
      stringr::str_c(collapse = "\n")
    
    media_list <- c(media_list, list(list(type = "PDF", content = pdf_text, filename = basename(.pdf))))
  }
  
  # Handle text files
  if (!is.null(.textfile)) {
    if (!file.exists(.textfile)) {
      stop("The specified text file does not exist.")
    }
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
  
  # If a character vector is supplied instead of an llm, we use it as an initial prompt
  if (is.character(.llm)) {
    initial_prompt <- .llm
    # Validate input prompt in case a prompt is used instead of a llm message object
    if (length(.llm) == 0) {
      stop("Prompt must be a non-empty string.")
    }
    
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
  
  # Explicit prompts have precedent over ones supplied via .llm
  if (!is.null(.prompt) & !pre_existing_object) {
    if ((length(.prompt) == 0) | !is.character(.prompt)) {
      stop("Prompt must be a non-empty string.")
    }
    .llm$add_message(.role, .prompt, media_list)
    return(.llm)
  }
  
  # Deep copy output so original object is never changed
  if (!is.null(.prompt) & pre_existing_object) {
    if ((length(.prompt) == 0) | !is.character(.prompt)) {
      stop("Prompt must be a non-empty string.")
    }
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
#' @examples
#' # Example data frame with role and content
#'df_example <- data.frame(
#'  role = c("system", "user", "assistant","user"),
#'  content = c("You allways only answer with two words", 
#'               "Why is the sky blue?", 
#'               "Rayleigh scattering",
#'               "Why is the sun yellow?"),
#'  stringsAsFactors = FALSE
#')
#'df_llm_message(df_example)
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

#' Retrieve Last Reply from an Assistant
#'
#' This function extracts the last reply made by the assistant from a given LLMMessage object.
#' It is particularly useful for fetching the most recent response from the assistant to display
#' or log it separately.
#'
#' @param .llm An LLMMessage object containing the history of messages exchanged.
#'             This must be a valid LLMMessage object; otherwise, the function will stop with an error.
#' @param .json Should structured json data from the last reply be returned as R list (default: FALSE)
#' @return Returns the content of the last reply made by the assistant. If the assistant
#'         has not replied yet, or if there are no assistant messages in the history, `NULL` is returned.
#' @export
#'
#' @seealso \code{\link{LLMMessage}} for details on the message object structure.
#'
#' @note This function only returns the content of the last assistant message and does not include
#'       media or other types of content that might be attached to the message.
#'
#'
#' @rdname last_reply
last_reply <- function(.llm  = NULL,
                       .json = FALSE) {
  # Check if .llm is provided and is a valid LLMMessage object
  c(
    "Input .llm must be an LLMMessage object" = inherits(.llm, "LLMMessage"),
    "Input .json must be logical if provided" = is.logical(.json)
  ) |>
    validate_inputs()
  
  # Filter out all messages from the assistant
  assistant_replies <- Filter(function(x) x$role == "assistant", .llm$message_history)
  
  # Extract the last assistant reply, if available
  last_reply <- assistant_replies[length(assistant_replies)]
  if (length(last_reply) > 0) {
    if(.json==FALSE){ return(last_reply[[1]]$content)}
    if(.json==TRUE){  return(jsonlite::fromJSON(last_reply[[1]]$content))}
    
  } else {
    return(NULL)
  }
}





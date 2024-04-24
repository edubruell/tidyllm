#' Extract  Media Content
#'
#' This internal function extracts and formats media content, specifically for Images or text formats. For PDF and RConsole types,
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
        return(x$type %in% c("PDF","RConsole"))
      } else {
        return(FALSE)
      }},.media_list) |>
      lapply(function(x){
        if(x$type=="PDF"){tagged <- glue::glue("<pdf filename=\"{x$filename}>{x$content}</pdf>\n")}
        if(x$type=="RConsole"){tagged <- glue::glue("<ROutput>{x$content}</ROutput>\n")}
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

#' Is an input an integer valued numeric (needed to validate inputs)
#'
#' This internal function is a small helper used to validate inputs in functions
#'
#' @param .x
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


#' Large Language Model Message Class
#'
#' This class manages a history of messages and media interactions intended for use with large language  models.
#' It allows for adding messages, converting messages for API usage, and printing the history in a structured format.
#'
#' @field message_history List to store all message interactions.
#' @field system_prompt The default prompt used by the system.
#' @import R6
LLMMessage <- R6::R6Class(
  "LLMMessage",
  public = list(
    message_history = list(),
    system_prompt = "You are a helpful assistant",
    
    #' Constructor
    #' 
    #' Initializes the message history with a system prompt.
    #' @param system_prompt Setting the default systen prompt for the LLM
    initialize = function(system_prompt = "You are a helpful assistant") {
      self$system_prompt <- system_prompt
      self$add_message("system", self$system_prompt)
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
               lapply(self$message_history, function(m){
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
                                    list(type = "text", text = paste(base_content,text_media)),
                                    list(type = "image_url",
                                         image_url = list(
                                           url = glue::glue("data:image/{image_file_type};base64,{image_media[[1]]$content}")
                                         ))
                                  ))
                   
                 } else {
                   # Text only content
                   output <- list(role = m$role,
                                  content = list(
                                    type = "text", text = paste(base_content,text_media))
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
    #' Print message history
    #' 
    #' Prints the current message history in a readable format.
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
#' It can handle the addition of text prompts and various media types such as images, PDFs, or plots.
#'
#' @param .llm An existing LLMMessage object or an initial text prompt.
#' @param .prompt Text prompt to add to the message history.
#' @param .role The role of the message sender, typically "user" or "assistant".
#' @param .system_prompt Default system prompt if a new LLMMessage needs to be created.
#' @param .imagefile Path to an image file to be attached (optional).
#' @param .pdf Path to a PDF file to be attached (optional).
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
                        .capture_plot = FALSE,
                        .f = NULL) {

  # Handle media attached to messages
  media_list = list()

  #  # If .llm is provided, check if it is an LLMMessage object
  if (!is.character(.llm) & !inherits(.llm, "LLMMessage")) {
    stop("Input .llm must be an LLMMessage object or an initial text prompt.")
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
    valid_types <- c("jpeg", "png")
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
    
    media_list <- c(media_list, list(list(type = "PDF",content=pdf_text, filename = basename(.pdf))))
  }
  
  #Handle function text outputs
  if (!is.null(.f)) {
    output <- capture.output(rlang::as_function(.f)(),file=NULL) |>
      stringr::str_c(collapse="\n")
    
    media_list <- c(media_list, list(list(type = "RConsole",content=output, filename = "RConsole.txt")))
  }
  
  #If a character vector is supplied instead of an llm we use it as an inital prompt
  if (is.character(.llm)) {
    initial_prompt <- .llm
    # Validate input prompt incase a prompt is used instead of a llm message object
    if (length(.llm) == 0) {
      stop("Prompt must be a non-empty string.")
    }
    
    .llm <- LLMMessage$new(.system_prompt)
    
    if (is.null(.prompt)) {
      .llm$add_message(.role, initial_prompt, media_list)
    }
  }
  
  #set up a new llm if it is null with the system prompt
  if (is.null(.llm)) {
    .llm <- LLMMessage$new(.system_prompt)
  }
  

  #explicit prompts have precedent over ones supplied via .llm
  if (!is.null(.prompt)) {
    if ((length(.prompt) == 0) | !is.character(.prompt)){
      stop("Prompt must be a non-empty string.")
    }
    .llm$add_message(.role, .prompt, media_list)
  }
  
  return(.llm)
}


#' Retrieve Last Reply from an Assistant
#'
#' This function extracts the last reply made by the assistant from a given LLMMessage object.
#' It is particularly useful for fetching the most recent response from the assistant to display
#' or log it separately.
#'
#' @param .llm An LLMMessage object containing the history of messages exchanged.
#'             This must be a valid LLMMessage object; otherwise, the function will stop with an error.
#'
#' @return Returns the content of the last reply made by the assistant. If the assistant
#'         has not replied yet, or if there are no assistant messages in the history, `NULL` is returned.
#'
#' @examples
#' # Assuming `llm_msg` is an existing LLMMessage object that has been interacted with
#' last_reply(llm_msg)
#'
#' @export
#'
#' @seealso \code{\link{LLMMessage}} for details on the message object structure.
#'
#' @note This function only returns the content of the last assistant message and does not include
#'       media or other types of content that might be attached to the message.
#'
#'
#' @rdname last_reply
last_reply <- function(.llm = NULL) {
  # Check if .llm is provided and is a valid LLMMessage object
  if (!inherits(.llm, "LLMMessage")) {
    stop("Input .llm must be an LLMMessage object")
  }
  
  # Filter out all messages from the assistant
  assistant_replies <- Filter(function(x) x$role == "assistant", .llm$message_history)
  
  # Extract the last assistant reply, if available
  last_reply <- assistant_replies[length(assistant_replies)]
  if (length(last_reply) > 0) {
    return(last_reply[[1]]$content)
  } else {
    return(NULL)
  }
}


#' Call the Anthropic API to interact with Claude models
#'
#' @param .llm An existing LLMMessage object or an initial text prompt.
#' @param .model The model identifier (default: "claude-3-sonnet-20240229").
#' @param .max_tokens The maximum number of tokens to generate (default: 1024).
#' @param .temperature Control for randomness in response generation (optional).
#' @param .top_k Top k sampling parameter (optional).
#' @param .top_p Nucleus sampling parameter (optional).
#' @param .system Additional system parameters (optional).
#' @param .metadata Additional metadata for the request (optional).
#' @param .stop_sequences Sequences that stop generation (optional).
#' @param .tools Additional tools used by the model (optional).
#' @param .api_url Base URL for the API (default: "https://api.anthropic.com/v1/messages").
#' @param .timeout Request timeout in seconds (default: 60).
#'
#' @return Returns an updated LLMMessage object.
#' @export
#' @examples
claude <- function(.llm,
                  .model = "claude-3-sonnet-20240229",
                  .max_tokens = 1024,
                  .temperature = NULL,
                  .top_k = NULL,
                  .top_p = NULL,
                  .metadata = NULL,
                  .stop_sequences = NULL,
                  .tools = NULL,
                  .api_url = "https://api.anthropic.com/v1/messages",
                  .timeout = 60) {  # Default timeout set to 60 seconds
  
  #Validate inputes to the Claude function
  c(
    "Input .llm must be an LLMMessage object"    = inherits(a, "LLMMessage"),
    "Input .max_tokens must be an integer"       = is_integer_valued(.max_tokens),
    ".timeout must be an integer-valued numeric (seconds till timeour)" = is_integer_valued(.timeout),
    ".temperature must be numeric if provided"   = is.null(.temp)  | is.numeric(.temp),
    ".top_k must be numeric if provided"         = is.null(.top_k) | is.numeric(.top_k),
    ".top_p must be numeric if provided"         = is.null(.top_p) | is.numeric(.top_p),
    ".stop_sequences must be a character vector" = is.null(.stop_sequences) | is.character(.stop_sequences)
  ) |>
    validate_inputs()
  

  #Get the formated message list so we can send it to a claude model
  messages <- .llm$to_api_format("claude")
  
  # Retrieve API key from environment variables
  api_key <- base::Sys.getenv("ANTHROPIC_API_KEY")
  if (api_key == "") stop("API key is not set. Please set it with : Sys.setenv(ANTHROPIC_API_KEY = \"YOUR-KEY-GOES-HERE\").")
  
  # Setup headers using httr package
  headers <- httr::add_headers(
    `x-api-key` = api_key,
    `anthropic-version` = "2023-06-01",
    `content-type` = "application/json; charset=utf-8"
  )
  
  # Data payload
  body <- list(
    model = .model,
    max_tokens = .max_tokens,
    messages = messages,
    system = .llm$system_prompt,
    temperature = .temperature,
    top_k = .top_k,
    top_p = .top_p,
    metadata = .metadata,
    stop_sequences = .stop_sequences,
    stream = FALSE,
    tools = .tools
  )
  
  # Filter out NULL values from the body
  body <- base::Filter(base::Negate(base::is.null), body)
  
  # Encode the body as JSON
  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)
  
  # Make the POST request using httr package
  response <- httr::POST(.api_url, headers, body = body_json, encode = "json", httr::timeout(.timeout))
  
  # Check for errors
  if (httr::http_status(response)$category != "Success") {
    stop("API request failed: ", httr::http_status(response)$reason)
  }
  
  # Get the content of the response
  response <-  httr::content(response, "text", encoding = "UTF-8")
  
  # Decode the response from JSON
  response_decoded <- jsonlite::fromJSON(response)
  
  #Add claudes message to the history of the llm message object
  .llm$add_message("assistant",response_decoded$content$text)
  
  return(.llm)
}

#' Call the OpenAI API to interact with ChatGPT models
#'
#' @param .llm An existing LLMMessage object or an initial text prompt.
#' @param .model The model identifier (default: "gpt-4").
#' @param .max_tokens The maximum number of tokens to generate (default: 1024).
#' @param .temperature Control for randomness in response generation (optional).
#' @param .top_k Top k sampling parameter (optional).
#' @param .top_p Nucleus sampling parameter (optional).
#' @param .system Additional system parameters (optional).
#' @param .metadata Additional metadata for the request (optional).
#' @param .stop_sequences Sequences that stop generation (optional).
#' @param .tools Additional tools used by the model (optional).
#' @param .api_url Base URL for the API (default: "https://api.anthropic.com/v1/messages").
#' @param .timeout Request timeout in seconds (default: 60).
#'
#' @return Returns an updated LLMMessage object.
#' @export
chatgpt <- function(.llm,
                    .model = "gpt-4",
                    .max_tokens = 1024,
                    .temperature = NULL,
                    .top_p = NULL,
                    .frequency_penalty = NULL,
                    .presence_penalty = NULL,
                    .api_url = "https://api.openai.com/v1/completions",
                    .timeout = 60) {
  
  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = inherits(.llm, "LLMMessage"),
    "Input .max_tokens must be an integer" = is_integer_valued(.max_tokens),
    ".timeout must be an integer-valued numeric" = is_integer_valued(.timeout),
    ".temperature must be numeric if provided" = is.null(.temperature) | is.numeric(.temperature),
    ".top_p must be numeric if provided" = is.null(.top_p) | is.numeric(.top_p),
    ".frequency_penalty must be numeric if provided" = is.null(.frequency_penalty) | is.numeric(.frequency_penalty),
    ".presence_penalty must be numeric if provided" = is.null(.presence_penalty) | is.numeric(.presence_penalty)
  ) |>
    validate_inputs()
  
  # Get formatted message list for ChatGPT
  messages <- .llm$to_api_format("chatgpt")
  
  # Retrieve API key from environment variables
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") stop("API key is not set. Please set it with: Sys.setenv(OPENAI_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  
  # Setup headers using httr package
  headers <- httr::add_headers(
    `Authorization` = sprintf("Bearer %s", api_key),
    `Content-Type` = "application/json; charset=utf-8"
  )
  
  # Data payload
  body <- list(
    model = .model,
    max_tokens = .max_tokens,
    messages = messages,
    temperature = .temperature,
    top_p = .top_p,
    frequency_penalty = .frequency_penalty,
    presence_penalty = .presence_penalty
  )
  
  # Filter out NULL values from the body
  body <- Filter(Negate(is.null), body)
  
  # Encode the body as JSON
  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)
  
  # Make the POST request using httr package
  response <- httr::POST(.api_url, headers, body = body_json, encode = "json", httr::timeout(.timeout))
  
  # Check for errors
  if (httr::http_status(response)$category != "Success") {
    stop("API request failed: ", httr::http_status(response)$reason)
  }
  
  # Get the content of the response
  response_content <- httr::content(response, "text", encoding = "UTF-8")
  
  # Decode the response from JSON
  response_decoded <- jsonlite::fromJSON(response_content)
  
  # Add ChatGPT's message to the history of the LLMMessage object
  .llm$add_message("assistant", response_decoded$choices[[1]]$text)
  
  return(.llm)
}


#' Call the Groq API to interact with fast opensource models on Groq
#'
#' @param .llm An existing LLMMessage object or an initial text prompt.
#' @param .model The model identifier (default: "gpt-4").
#' @param .max_tokens The maximum number of tokens to generate (default: 1024).
#' @param .temperature Control for randomness in response generation (optional).
#' @param .top_k Top k sampling parameter (optional).
#' @param .top_p Nucleus sampling parameter (optional).
#' @param .system Additional system parameters (optional).
#' @param .metadata Additional metadata for the request (optional).
#' @param .stop_sequences Sequences that stop generation (optional).
#' @param .tools Additional tools used by the model (optional).
#' @param .api_url Base URL for the API (default: "https://api.anthropic.com/v1/messages").
#' @param .timeout Request timeout in seconds (default: 60).
#'
#' @return Returns an updated LLMMessage object.
#' @export
groq <- function(.llm,
                    .model = "mixtral-8x7b-32768",
                    .max_tokens = 1024,
                    .temperature = NULL,
                    .top_p = NULL,
                    .frequency_penalty = NULL,
                    .presence_penalty = NULL,
                    .api_url = "https://api.groq.com/openai/v1/chat/completions",
                    .timeout = 60) {

  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = inherits(.llm, "LLMMessage"),
    "Input .max_tokens must be an integer" = is_integer_valued(.max_tokens),
    ".timeout must be an integer-valued numeric" = is_integer_valued(.timeout),
    ".temperature must be numeric if provided" = is.null(.temperature) | is.numeric(.temperature),
    ".top_p must be numeric if provided" = is.null(.top_p) | is.numeric(.top_p),
    ".frequency_penalty must be numeric if provided" = is.null(.frequency_penalty) | is.numeric(.frequency_penalty),
    ".presence_penalty must be numeric if provided" = is.null(.presence_penalty) | is.numeric(.presence_penalty)
  ) |>
    validate_inputs()
  
  # Get formatted message list for Groq models
  messages <- .llm$to_api_format("groq")
  
  # Retrieve API key from environment variables
  api_key <- Sys.getenv("GROQ_API_KEY")
  if (api_key == "") stop("API key is not set. Please set it with: Sys.setenv(GROQ_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  
  # Setup headers using httr package
  headers <- httr::add_headers(
    `Authorization` = sprintf("Bearer %s", api_key),
    `Content-Type` = "application/json; charset=utf-8"
  )
  
  # Data payload
  body <- list(
    model = .model,
    max_tokens = .max_tokens,
    messages = messages,
    temperature = .temperature,
    top_p = .top_p,
    frequency_penalty = .frequency_penalty,
    presence_penalty = .presence_penalty
  )
  
  # Filter out NULL values from the body
  body <- Filter(Negate(is.null), body)
  
  # Encode the body as JSON
  body_json <- jsonlite::toJSON(body, auto_unbox = TRUE)
  
  # Make the POST request using httr package
  response <- httr::POST(.api_url, headers, body = body_json, encode = "json", httr::timeout(.timeout))
  
  # Check for errors
  if (httr::http_status(response)$category != "Success") {
    stop("API request failed: ", httr::http_status(response))
  }
  
  # Get the content of the response
  response_content <- httr::content(response, "text", encoding = "UTF-8")
  
  # Decode the response from JSON
  response_decoded <- jsonlite::fromJSON(response_content)

  # Add model's message to the history of the LLMMessage object
  .llm$add_message("assistant", response_decoded$choices$message$content)
  
  return(.llm)
}



#What works till now. using and mixing models! Yes:  
#llm_message("Describe this image",
#                 .imagefile = "/Users/ebr/data/test.png") |>
#  claude() |>
#  llm_message("Based on your previous description and knowing that WfH refers to the Working from Home potential 
#              of occupations (classified based on a German survey on job tasks), what could the reserach here be about ") |>
#  groq()

#Todo: Make chatpgt() work 
#Todo: Add a count_tokens() function

#a <- llm_message("Here is a first R script for a package I am writing to communicate with varying llms
#(claude, chatgpt, groq open-models on dedicated hardware and future ones i will add). The idea is to provide a tidy interface to
#language model APIs that feels natural to an R user. My project should be named tidyllm. Can you write an engaging README.MD for my project? Use native pipes in the intro and show how do things like 
#           llm_message(\"Describe this image\",
#                 .imagefile = \"/Users/ebr/data/test.png\") |>
#                  claude() |>
#            llm_message(\"Based on your previous description and knowing that WfH refers to the Working from Home potential 
#                          of occupations (classified based on a German survey on job tasks), what could the reserach here be about \") |>
#              groq()
#              
#            Add llm_message() groq() chatgpt() claude()  and last_reply() in the section on functions. 
#           ",.f=~{here("R","oop.R") |> readr::read_file()}) |>
#  groq()
#llm_message("What does this package do? Suggest some more functions that would be useful for a project like the Rpackage I show you the source from.",.f=~{here("R","oop.R") |> readr::read_file()}) |>
#    groq()
#
#llm_message("Explain how i can use devtools to set up a new empty R package. Btw. I work with RStudio. I want to d") |>
#  groq()

#' The base class for API-provider specific classes
#'
#' @noRd
APIProvider <- new_class("APIProvider",properties = list(
  short_name = class_character,
  long_name  = class_character,
  api_key_env_var = class_character
))

generate_callback_function <- new_generic("generate_callback_function","api")
ratelimit_from_header      <- new_generic("ratelimit_from_header",c("api", "headers"))
parse_chat_function        <- new_generic("parse_chat_response","api")
get_api_key                <- new_generic("get_api_key","api")
prepare_llms_for_batch     <- new_generic("prepare_llms_for_batch","api")
extract_metadata           <- new_generic("extract_metadata",c("api", "response"))


#Default method for the streaming callback function
#'
#' @noRd
method(generate_callback_function,APIProvider) <- function(api) {
  # Default testing callback implementation
  function(.data) {
    if (is.null(.tidyllm_stream_env$testing_chunck_list)) {
      .tidyllm_stream_env$testing_chunck_list <- list()
    }
    
    # Append new data to the buffer
    new_data <- rawToChar(.data, multiple = FALSE)
    .tidyllm_stream_env$testing_chunck_list <- append(.tidyllm_stream_env$testing_chunck_list, new_data)
    length(.tidyllm_stream_env$testing_chunck_list) |> cat("\n")
    utils::flush.console()
    TRUE
  }
}


#Default method for metadata extraction
#' A function to get metadata from Claude responses
#'
#' @noRd
method(extract_metadata, list(APIProvider,class_list))<- function(api,response) {
  list(
    model             = NA_character_,
    timestamp         = lubridate::as_datetime(lubridate::now()),
    prompt_tokens     = NA_integer_,
    completion_tokens = NA_integer_,
    total_tokens      = NA_integer_,
    specific_metadata = list() 
  )
}  



#Default method for the API key checks
#'
#' @noRd
method(get_api_key, APIProvider) <- function(api,dry_run=FALSE) {
  api_key <- Sys.getenv(api@api_key_env_var)
  if (api_key == "" & dry_run==FALSE) {
    paste0("API key is not set. Please set it with: Sys.setenv(",api@api_key_env_var," = \"YOUR-KEY-GOES-HERE\")." ) |>
      rlang::abort()
  }
  return(api_key)
}


#Prepare a list of LLMs for batch requests 
#'
#' @noRd
method(prepare_llms_for_batch, APIProvider) <- function(api, .llms, .id_prefix, .overwrite = FALSE) {
  # Check for unique non-missing names
  non_missing_names <- names(.llms)[!(is.na(names(.llms)) | names(.llms) == "")]
  if (anyDuplicated(non_missing_names)) {
    rlang::abort("Each specified name in .llms must be unique. Please ensure that all non-missing names are unique.")
  }
  
  # Check for existing batch_id
  if (!is.null(attr(.llms, "batch_id"))) {
    if (.overwrite) {
      rlang::warn("Batch ID is already set in the provided list. Overwriting with a new batch ID.")
    } else {
      rlang::abort("Batch ID is already set in the provided list. Set .overwrite = TRUE to overwrite.")
    }
  }
  
  # Generate custom IDs for missing names
  names(.llms) <- lapply(seq_along(.llms), function(i) {
    current_name <- names(.llms)[i]
    if (is.null(current_name) || current_name == "" || is.na(current_name)) {
      paste0(.id_prefix, i)
    } else {
      current_name
    }
  })
  
  return(.llms)
}


#api_fart <- APIProvider(short_name = "Fart",long_name = "fart.ai",api_key_env_var = "FOPENAI_API_KEY" )
#get_api_key(api_fart,TRUE)

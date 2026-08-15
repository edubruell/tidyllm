#' The base class for API-provider specific classes
#'
#' @noRd
APIProvider <- new_class("APIProvider",properties = list(
  short_name = class_character,
  long_name  = class_character,
  api_key_env_var = class_character,
  # Which reader the shared stream pump uses: "sse" for text/event-stream,
  # "lines" for newline-delimited JSON. Set it at the construction site, never
  # as a subclass default override: S7 0.2.2 records a subclass override in
  # @properties but the constructor still returns the parent's value, silently,
  # and a wrong transport points the SSE reader at an ndjson stream.
  stream_transport = new_property(class_character, default = "sse")
))

parse_chat_response        <- new_generic("parse_chat_response",c(".api",".content"))
handle_stream              <- new_generic("handle_stream",c(".api",".stream_response"))
parse_stream_event         <- new_generic("parse_stream_event",".api")
ratelimit_from_header      <- new_generic("ratelimit_from_header",c(".api", ".headers"))
get_api_key                <- new_generic("get_api_key",".api")
prepare_llms_for_batch     <- new_generic("prepare_llms_for_batch",".api")
extract_metadata           <- new_generic("extract_metadata",c(".api", ".response"))
extract_metadata_stream    <- new_generic("extract_metadata_stream",c(".api", ".stream_raw_data"))
parse_logprobs             <- new_generic("parse_logprobs", c(".api", ".input"))
assemble_stream_response   <- new_generic("assemble_stream_response", c(".api", ".events"))

#' Defaults: a provider reports no rate limits and no logprobs
#'
#' Both generics used to be partial, defined only for the providers that had
#' something to return, so every caller had to know in advance whether calling
#' them was safe. That knowledge lived as two booleans on each chat request.
#' With a default method the question is answered by the provider class, which is
#' where the rest of this package's capability knowledge lives.
#'
#' Providers that inherit a method they should not use override it back to NULL
#' next to their own class definition, the same way they override
#' `extract_metadata()`.
#'
#' @noRd
method(ratelimit_from_header, list(APIProvider, class_any)) <- function(.api, .headers) NULL

#' @noRd
method(parse_logprobs, list(APIProvider, class_any)) <- function(.api, .input) NULL

#' Default: a provider's stream does not reassemble into a response body
#'
#' `assemble_stream_response()` folds the events a stream produced back into the
#' body shape a blocking request would have returned, so that everything
#' downstream of the transport, above all the tool loop, reads one shape and
#' does not care how the response arrived.
#'
#' Providers that implement it can stream and call tools in the same request.
#' Providers that do not keep the client-side guard against `.stream` with
#' `.tools`, and returning NULL here leaves `has_tool_calls()` reading an absent
#' body, which is FALSE for every provider.
#'
#' @noRd
method(assemble_stream_response, list(APIProvider, class_any)) <- function(.api, .events) NULL

#Default method for metadata extraction
#'
#' @noRd
method(extract_metadata, list(APIProvider,class_list))<- function(.api,.response) {
  list(
    model             = NA_character_,
    timestamp         = lubridate::as_datetime(lubridate::now()),
    prompt_tokens     = NA_integer_,
    completion_tokens = NA_integer_,
    total_tokens      = NA_integer_,
    cached_tokens         = NA_integer_,
    cache_creation_tokens = NA_integer_,
    stream            = FALSE,
    specific_metadata = list() 
  )
}  

#Default method for metadata extraction
#'
#' @noRd
method(extract_metadata_stream, list(APIProvider,class_list))<- function(.api,.stream_raw_data) {
  list(
    model             = NA_character_,
    timestamp         = lubridate::as_datetime(lubridate::now()),
    prompt_tokens     = NA_integer_,
    completion_tokens = NA_integer_,
    total_tokens      = NA_integer_,
    cached_tokens         = NA_integer_,
    cache_creation_tokens = NA_integer_,
    stream            = TRUE,
    specific_metadata = list() 
  )
}  


#Default method for the API key checks
#'
#' @noRd
method(get_api_key, APIProvider) <- function(.api,.dry_run=FALSE) {
  api_key <- Sys.getenv(.api@api_key_env_var)
  if (api_key == "" & .dry_run==FALSE) {
    paste0("API key is not set. Please set it with: Sys.setenv(",.api@api_key_env_var," = \"YOUR-KEY-GOES-HERE\")." ) |>
      rlang::abort()
  }
  return(api_key)
}


#Prepare a list of LLMs for batch requests 
#'
#' @noRd
method(prepare_llms_for_batch, APIProvider) <- function(.api, .llms, .id_prefix, .overwrite = FALSE) {
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


# Default error stubs for file verbs — overridden per provider that supports the Files API

upload_file_default <- function(.path, .called_from, ...) {
  api_obj <- rlang::env_get(parent.frame(), "api_obj", default = NULL)
  name <- if (!is.null(api_obj)) api_obj@short_name else "this"
  stop(glue::glue(
    "The '{name}' provider does not support file upload.\n",
    "Only claude(), gemini(), and openai() have a Files API."
  ))
}

list_files_default <- function(.called_from, ...) {
  api_obj <- rlang::env_get(parent.frame(), "api_obj", default = NULL)
  name <- if (!is.null(api_obj)) api_obj@short_name else "this"
  stop(glue::glue("The '{name}' provider does not support list_files()."))
}

file_info_default <- function(.file_id, .called_from, ...) {
  api_obj <- rlang::env_get(parent.frame(), "api_obj", default = NULL)
  name <- if (!is.null(api_obj)) api_obj@short_name else "this"
  stop(glue::glue("The '{name}' provider does not support file_info()."))
}

delete_file_default <- function(.file_id, .called_from, ...) {
  api_obj <- rlang::env_get(parent.frame(), "api_obj", default = NULL)
  name <- if (!is.null(api_obj)) api_obj@short_name else "this"
  stop(glue::glue("The '{name}' provider does not support delete_file()."))
}

#api_fart <- APIProvider(short_name = "Fart",long_name = "fart.ai",api_key_env_var = "FOPENAI_API_KEY" )
#get_api_key(api_fart,TRUE)

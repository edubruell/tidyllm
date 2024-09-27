#' Generate API-Specific Callback Function for Streaming Responses
#'
#' This function generates a callback function that processes streaming responses
#' from different language model APIs. The callback function is specific to the
#' API provided (`claude`, `ollama`, or `chatgpt`) and processes incoming data streams,
#' printing the content to the console and updating a global environment for further use.
#'
#' @param .api A character string indicating the API type. Supported values are
#'   `"claude"`, `"ollama"`, and `"chatgpt"`.
#' @return A function that serves as a callback to handle streaming responses
#'   from the specified API. The callback function processes the raw data, updates
#'   the `.tidyllm_stream_env$stream` object, and prints the streamed content to the console.
#'   The function returns `TRUE` if streaming should continue, and `FALSE` when
#'   streaming is finished.
#' @details
#' - **For Claude API**: The function processes event and data lines, and handles the `message_start`
#'   and `message_stop` events to control streaming flow.
#' - **For Ollama API**: The function directly parses the stream content as JSON and extracts the
#'   `message$content` field.
#' - **For ChatGPT API**: The function handles JSON data streams and processes content deltas.
#'   It stops processing when the `[DONE]` message is encountered.
#' @examples
#' # Create a callback function for Claude API
#' callback_fn <- generate_callback_function("claude")
#' 
#' # Use the callback function to process a stream (example)
#' # callback_fn(stream_data)
generate_callback_function <- function(.api) {
  if (.api == "claude") {
    callback_fn <- function(.data) {
      
      # Read the stream content and split into lines
      lines <- .data |>
        rawToChar(multiple = FALSE) |>
        stringr::str_split("\n") |>
        unlist()
      
      # Initialize a flag to control early exit
      continue_processing <- TRUE
      
      # Separate event and data lines
      event_lines <- lines |>
        purrr::keep(~ grepl("^event:", .x) && .x != "")
      data_lines <- lines |>
        purrr::keep(~ grepl("^data:", .x) && .x != "")
      
      # Process event lines
      purrr::walk(event_lines, ~ {
        if (grepl("message_start", .x)) {
          .tidyllm_stream_env$stream <- ""
        } else if (grepl("message_stop", .x)) {
          cat("\n---------\nStream finished\n---------\n")
          continue_processing <<- FALSE
        }
      })
      
      # Process data lines
      purrr::walk(data_lines, ~ {
        json_part <- sub("^data: ", "", .x)
        # Try to parse the JSON content
        parsed_event <- tryCatch(
          jsonlite::fromJSON(json_part),
          error = function(e) {
            message("Failed to parse JSON: ", e$message)
            return(NULL)
          }
        )
        if (!is.null(parsed_event) && parsed_event$type == "content_block_delta") {
          delta_content <- parsed_event$delta$text
          .tidyllm_stream_env$stream <- paste0(.tidyllm_stream_env$stream, delta_content)
          cat(delta_content)
          flush.console()
        }
      })
      
      return(continue_processing)
    }
  } else if (.api == "ollama") {
    #This obviously needs some refactoring, but it works
    callback_fn <- function(.data) {
      stream_content <- rawToChar(.data, multiple = FALSE) |> 
        jsonlite::fromJSON()
      
      stream_response <- stream_content$message$content 
      .tidyllm_stream_env$stream <- glue::glue("{.tidyllm_stream_env$stream}{stream_response}") |> as.character()
      cat(stream_response)
      flush.console()
      TRUE
      }
  } else if (.api == "chatgpt") {
      callback_fn <- function(.data) {
        # Read the stream content and split into lines
        lines <- .data |>
          rawToChar(multiple = FALSE) |>
          stringr::str_split("\n") |>
          unlist()
        
        # Initialize a flag to control early exit
        continue_processing <- TRUE
        
        # Process lines that start with "data: "
        data_lines <- lines |>
          purrr::keep(~ grepl("^data: ", .x) && .x != "")
        
        # Process data lines
        purrr::walk(data_lines, ~ {
          
          json_part <- sub("^data: ", "", .x)
          
          if (json_part != "[DONE]") {
            # Try to parse the JSON content
            parsed_event <- tryCatch(
              jsonlite::fromJSON(json_part, simplifyVector = FALSE, simplifyDataFrame = FALSE),
              error = function(e) {
                message("Failed to parse JSON: ", e$message)
                return(NULL)
              }
            )
            
            if (!is.null(parsed_event)) {
              delta_content <- parsed_event$choices[[1]]$delta$content
              if (!is.null(delta_content)) {
                .tidyllm_stream_env$stream <- paste0(.tidyllm_stream_env$stream, delta_content)
                cat(delta_content)
                flush.console()
              }
            }
          } else {
            cat("\n---------\nStream finished\n---------\n")
            continue_processing <<- FALSE
          }
        })
        
        return(continue_processing)
      }
    } else {
    stop("Unknown API for callback function.")
  }
  
  return(callback_fn)
}


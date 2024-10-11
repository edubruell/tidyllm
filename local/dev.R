# Helper function to perform API calls with optional streaming
call_llm_api <- function(.llm,
                         api_name,
                         api_url,
                         request_path,
                         model,
                         messages,
                         request_body_fields,
                         headers,
                         api_key_env_var = NULL,
                         timeout = 60,
                         wait = TRUE,
                         min_tokens_reset = 0L,
                         verbose = FALSE,
                         parse_response_function,
                         prepare_rate_limit_info = NULL,
                         stream = FALSE,
                         stream_callback = NULL,
                         buffer_kb = 0.05,
                         round = "line") {
  

  # Validate .llm
  if (!inherits(.llm, "LLMMessage")) stop("Input .llm must be an LLMMessage object")
  
  # Get API key if needed
  if (!is.null(api_key_env_var)) {
    api_key <- Sys.getenv(api_key_env_var)
    if (api_key == "") {
      stop(sprintf("API key is not set. Please set it with: Sys.setenv(%s = \"YOUR-KEY-GOES-HERE\")", api_key_env_var))
    }
    # Add API key to headers if not already included
    if (!any(names(headers) == "Authorization") && !any(names(headers) == "x-api-key")) {
      # Determine the header key based on the API service
      if (api_name == "chatgpt" || api_name == "groq") {
        headers <- c(headers, `Authorization` = sprintf("Bearer %s", api_key))
      } else if (api_name == "claude") {
        headers <- c(headers, `x-api-key` = api_key)
      }
    }
  }
  
  # Wait for rate limit if necessary
  if (wait == TRUE && !is.null(.tidyllm_rate_limit_env[[api_name]])) {
    wait_rate_limit(api_name, min_tokens_reset)
  }
  
  # Prepare request body
  request_body <- c(
    list(
      model = model,
      messages = messages
    ),
    request_body_fields
  )
  request_body <- base::Filter(Negate(is.null), request_body)
  
  # Add streaming parameter to request body if applicable
  if (stream) {
    # The streaming parameter may vary between APIs
    if (api_name == "chatgpt") {
      request_body$stream <- TRUE
    } else if (api_name == "claude") {
      request_body$stream <- TRUE
    }
    # Add other services' streaming parameters as needed
  }
  
  # Build the request
  req <- httr2::request(api_url) |>
    httr2::req_url_path_append(request_path) |>
    httr2::req_headers(.headers = headers) |>
    httr2::req_body_json(data = request_body) |>
    httr2::req_timeout(timeout)
  
  # Initialize assistant_reply
  assistant_reply <- NULL
  
  if (stream && !is.null(stream_callback)) {
    # Initialize the streaming environment variable
    .tidyllm_stream_env$stream <- ""
    cat("\n---------\nStart streaming: \n---------\n")
    
    # Perform streaming request
    httr2::req_perform_stream(req, callback = stream_callback, buffer_kb = buffer_kb, round = round)
    
    # Assign the collected stream to assistant_reply
    assistant_reply <- .tidyllm_stream_env$stream
    
    cat("\n---------\nStream finished\n---------\n")
  } else {
    # Non-streaming request
    response <- httr2::req_perform(req)
    
    # Check for errors
    if (httr2::resp_status(response) != 200) {
      stop("API request failed! Here is the raw response from the server: ", httr2::resp_body_string(response))
    }
    
    # Parse response
    body_json <- httr2::resp_body_json(response)
    
    # Update rate limit info if applicable
    if (!is.null(prepare_rate_limit_info)) {
      response_headers <- httr2::resp_headers(response)
      rl <- prepare_rate_limit_info(response_headers)
      initialize_api_env(api_name)
      update_rate_limit(api_name, rl)
      
      # Verbose output
      if (verbose) {
        rate_limit_message <- glue::glue(
          "{api_name} API response received at {rl$this_request_time}.\n",
          "Remaining requests: {rl$ratelimit_requests_remaining}/{rl$ratelimit_requests}.\n",
          "Requests reset at: {rl$ratelimit_requests_reset_time}.\n",
          "Remaining tokens: {rl$ratelimit_tokens_remaining}/{rl$ratelimit_tokens}.\n",
          "Tokens reset at: {rl$ratelimit_tokens_reset_time}.\n"
        )
        cat(rate_limit_message)
      }
    }
    
    # Extract assistant's reply
    assistant_reply <- parse_response_function(body_json)
  }
  
  # Create a deep copy of the LLMMessage object and add assistant's reply
  llm_copy <- .llm$clone_deep()
  llm_copy$add_message("assistant", assistant_reply)
  
  return(llm_copy)
}


chatgpt_new <- function(.llm,
                    .model = "gpt-4o",
                    .max_tokens = 1024,
                    .temperature = NULL,
                    .top_p = NULL,
                    .frequency_penalty = NULL,
                    .presence_penalty = NULL,
                    .api_url = "https://api.openai.com/",
                    .timeout = 60,
                    .verbose = FALSE,
                    .wait = TRUE,
                    .min_tokens_reset = 0L,
                    .stream = FALSE) {

  # Prepare messages
  messages <- .llm$to_api_format("chatgpt")
  
  # Request body fields
  request_body_fields <- list(
    max_tokens = .max_tokens,
    temperature = .temperature,
    top_p = .top_p,
    frequency_penalty = .frequency_penalty,
    presence_penalty = .presence_penalty
  )
  
  # Headers
  headers <- list(`Content-Type` = "application/json")
  
  # Function to parse response
  parse_response_function <- function(body_json) {
    body_json$choices[[1]]$message$content
  }
  
  # Function to prepare rate limit info
  prepare_rate_limit_info <- function(response_headers) {
    request_time <- strptime(response_headers$date, format="%a, %d %b %Y %H:%M:%S", tz="GMT")
    ratelimit_requests_reset_dt <- as.numeric(response_headers$`x-ratelimit-reset-requests`)
    ratelimit_requests_reset_time <- request_time + ratelimit_requests_reset_dt
    ratelimit_tokens_reset_dt <- as.numeric(response_headers$`x-ratelimit-reset-tokens`)
    ratelimit_tokens_reset_time <- request_time + ratelimit_tokens_reset_dt
    
    list(
      this_request_time = request_time,
      ratelimit_requests = as.integer(response_headers$`x-ratelimit-limit-requests`),
      ratelimit_requests_remaining = as.integer(response_headers$`x-ratelimit-remaining-requests`),
      ratelimit_requests_reset_time = ratelimit_requests_reset_time,
      ratelimit_tokens = as.integer(response_headers$`x-ratelimit-limit-tokens`),
      ratelimit_tokens_remaining = as.integer(response_headers$`x-ratelimit-remaining-tokens`),
      ratelimit_tokens_reset_time = ratelimit_tokens_reset_time
    )
  }
  
  # Streaming callback function
  stream_callback <- NULL
  if (.stream) {
    .tidyllm_stream_env$stream <- ""
    cat("\n---------\nStart OpenAI streaming: \n---------\n")
    
    stream_callback <- function(.data, .req) {
      # Split the data into lines
      lines <- strsplit(rawToChar(.data), "\n")[[1]]
      for (line in lines) {
        if (nzchar(line)) {
          if (grepl("^data:", line)) {
            json_line <- sub("^data: ", "", line)
            if (json_line == "[DONE]") {
              return(FALSE)  # Stop streaming
            }
            content <- jsonlite::fromJSON(json_line)
            if (!is.null(content$choices[[1]]$delta$content)) {
              delta <- content$choices[[1]]$delta$content
              .tidyllm_stream_env$stream <- paste0(.tidyllm_stream_env$stream, delta)
              cat(delta)
              flush.console()
            }
          }
        }
      }
      TRUE  # Continue streaming
    }
  }
  
  # Call the generic API function
  llm_copy <- call_llm_api(
    .llm = .llm,
    api_name = "chatgpt",
    api_url = .api_url,
    request_path = "/v1/chat/completions",
    model = .model,
    messages = messages,
    request_body_fields = request_body_fields,
    headers = headers,
    api_key_env_var = "OPENAI_API_KEY",
    timeout = .timeout,
    wait = .wait,
    min_tokens_reset = .min_tokens_reset,
    verbose = .verbose,
    parse_response_function = parse_response_function,
    prepare_rate_limit_info = prepare_rate_limit_info,
    stream = .stream,
    stream_callback = stream_callback
  )
  
  if (.stream) {
    cat("\n---------\nStream finished\n---------\n")
  }
  
  return(llm_copy)
}

.tidyllm_rate_limit_env

test_llm |>
  chatgpt()

llm_message("Write a haiku about robots.txt")  |>
  claude(.stream=FALSE)







test_llm <- llm_message("Write a haiku about robots.txt") 

request_body <- list(
  model = "gpt-4o",
  max_tokens = NULL,
  messages = test_llm$to_api_format("chatgpt"),
  temperature = NULL,
  top_p = NULL,
  frequency_penalty = NULL,
  presence_penalty = NULL
)

request_body <- base::Filter(Negate(is.null), request_body)



b <- httr2::request("https://api.openai.com/") |>
  httr2::req_url_path("/v1/chat/completions") |>
  httr2::req_headers(
    `Authorization` = sprintf("Bearer %s", Sys.getenv("OPENAI_API_KEY")),
    `Content-Type` = "application/json"
  ) |>
  httr2::req_body_json(data = request_body) |> 
  httr2::req_timeout(30) |>
  httr2::req_perform()

body_json <- b |> httr2::resp_body_json()




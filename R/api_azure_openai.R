#' The Azure OpenAI API provider class
#'
#'Inherit most of the functionality from vanilla OpenAI API
#'
#' @noRd
api_azure_openai <- new_class("Azure OpenAI", api_openai)


#' Extract rate limit info from  Azure Openai API-Headers
#'
#' @noRd
method(ratelimit_from_header, list(api_azure_openai,new_S3_class("httr2_headers"))) <- function(api,headers){
  request_time <- strptime(headers["date"]$date, 
                           format="%a, %d %b %Y %H:%M:%S", tz="GMT")
  
  # Extract remaining requests and tokens
  ratelimit_requests_remaining <- as.integer(
    headers["x-ratelimit-remaining-requests"]$`x-ratelimit-remaining-requests`)
  ratelimit_tokens_remaining <- as.integer(
    headers["x-ratelimit-remaining-tokens"]$`x-ratelimit-remaining-tokens`)
  
  # Assuming reset occurs every 60 seconds (at least I got minutes in my azure console)
  reset_interval <- 60         
  
  ratelimit_requests_reset_time <- request_time + reset_interval
  ratelimit_tokens_reset_time <- request_time + reset_interval
  
  list(
    this_request_time = request_time,
    ratelimit_requests = NA,
    ratelimit_requests_remaining = ratelimit_requests_remaining,
    ratelimit_requests_reset_time = ratelimit_requests_reset_time,
    ratelimit_tokens = NA,
    ratelimit_tokens_remaining = ratelimit_tokens_remaining,
    ratelimit_tokens_reset_time = ratelimit_tokens_reset_time
  )
}


#' A chat parsing method for Azure Openai to extract the assitant response
#'
#' @noRd
method(parse_chat_function, api_azure_openai) <- function(api) {
  api_label <- api@long_name 
  function(body_json){
    if("error" %in% names(body_json)){
      sprintf("%s returned an Error:\nType: %s\nMessage: %s",
              api_label,
              body_json$error$code,
              body_json$error$message) |>
        stop()
    }
    
    if (length(body_json$choices) == 0) {
      paste0("Received empty response from ",api_label) |>
        stop()
    }
    body_json$choices[[1]]$message$content  
  }
}  


#' Send LLM Messages to an OpenAI Chat Completions endpoint on Azure 
#'
#' @description
#' This function sends a message history to the Azure OpenAI Chat Completions API and returns the assistant's reply. 
#' This function is work in progress and not fully tested
#'
#' @param .llm An `LLMMessage` object containing the conversation history.
#' @param .endpoint_url Base URL for the API (default:  Sys.getenv("AZURE_ENDPOINT_URL")).
#' @param .deployment The identifier of the model that is deployed (default: "gpt-4o-mini").
#' @param .api_version Which version of the API is deployed (default: "2024-08-01-preview")
#' @param .max_completion_tokens An upper bound for the number of tokens that can be generated for a completion, including visible output tokens and reasoning tokens.
#' @param .frequency_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far.
#' @param .logit_bias A named list modifying the likelihood of specified tokens appearing in the completion.
#' @param .logprobs Whether to return log probabilities of the output tokens (default: FALSE).
#' @param .top_logprobs An integer between 0 and 20 specifying the number of most likely tokens to return at each token position.
#' @param .presence_penalty Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far.
#' @param .seed If specified, the system will make a best effort to sample deterministically.
#' @param .stop Up to 4 sequences where the API will stop generating further tokens.
#' @param .stream If set to TRUE, the answer will be streamed to console as it comes (default: FALSE).
#' @param .temperature What sampling temperature to use, between 0 and 2. Higher values make the output more random.
#' @param .top_p An alternative to sampling with temperature, called nucleus sampling.
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .verbose Should additional information be shown after the API call (default: FALSE).
#' @param .max_tries Maximum retries to perform request
#' @param .json_schema A JSON schema object as R list to enforce the output structure (If defined has precedence over JSON mode).
#' @param .dry_run If TRUE, perform a dry run and return the request object (default: FALSE).
#'
#'
#' @return A new `LLMMessage` object containing the original messages plus the assistant's response.
#' @examples
#' \dontrun{
#' # Basic usage
#' msg <- llm_message("What is R programming?")
#' result <- azure_openai_chat(msg)
#' 
#' # With custom parameters
#' result2 <- azure_openai_chat(msg, 
#'                  .deployment = "gpt-4o-mini",
#'                  .temperature = 0.7, 
#'                  .max_tokens = 1000)
#' }
#'
#' @export
azure_openai_chat <- function(
    .llm,
    .endpoint_url = Sys.getenv("AZURE_ENDPOINT_URL"),
    .deployment = "gpt-4o-mini",
    .api_version = "2024-08-01-preview",
    .max_completion_tokens = NULL,
    .frequency_penalty = NULL,
    .logit_bias = NULL,
    .logprobs = FALSE,
    .top_logprobs = NULL,
    .presence_penalty = NULL,
    .seed = NULL,
    .stop = NULL,
    .stream = FALSE,
    .temperature = NULL,
    .top_p = NULL,
    .timeout = 60,
    .verbose = FALSE,
    .json_schema = NULL,
    .dry_run = FALSE,
    .max_tries = 3
) {
    #Check enpoint
  if (.endpoint_url == ""& .dry_run==FALSE){
   stop("No valid Azure endpoint defined. Please set it either as input to this function or with: Sys.setenv(AZURE_ENDPOINT_URL = \"https://endpoint.openai.azure.com/\")")
  }
  
  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
    "Input .deployment must be a string" = is.character(.deployment),
    "Input .max_completion_tokens must be NULL or a positive integer" = is.null(.max_completion_tokens) | (is_integer_valued(.max_completion_tokens) & .max_completion_tokens > 0),    ".frequency_penalty must be numeric or NULL" = is.null(.frequency_penalty) | is.numeric(.frequency_penalty),
    "Input .logit_bias must be a list or NULL" = is.null(.logit_bias) | is.list(.logit_bias),
    "Input .logprobs must be logical or NULL" = is.null(.logprobs) | is.logical(.logprobs),
    "Input .top_logprobs must be NULL or an integer between 0 and 20" = is.null(.top_logprobs) | (is_integer_valued(.top_logprobs) & .top_logprobs >= 0 & .top_logprobs <= 20),
    "Input .presence_penalty must be numeric or NULL" = is.null(.presence_penalty) | is.numeric(.presence_penalty),
    "Input .seed must be NULL or an integer" = is.null(.seed) | is_integer_valued(.seed),
    "Input .stop must be NULL or a character vector or string" = is.null(.stop) | is.character(.stop),
    "Input .stream must be logical or NULL" = is.null(.stream) | is.logical(.stream),
    "Input .temperature must be numeric or NULL" = is.null(.temperature) | is.numeric(.temperature),
    "Input .top_p must be numeric or NULL" = is.null(.top_p) | is.numeric(.top_p),
    "Input .timeout must be integer-valued numeric" = is_integer_valued(.timeout),
    "Input .verbose must be logical" = is.logical(.verbose),
    "Input .json_schema must be NULL or a list" = is.null(.json_schema) | is.list(.json_schema),
    "Input .dry_run must be logical" = is.logical(.dry_run)
  ) |> validate_inputs()
  
  
  api_obj <- api_azure_openai(short_name = "azure_openai",
                              long_name  = "Azure OpenAI",
                              api_key_env_var = "AZURE_OPENAI_API_KEY")
  
  api_key <- get_api_key(api_obj,.dry_run)
  
  #This filters out the system prompt for reasoning models.
  no_system_prompt <- FALSE
  if(.deployment %in% c("o1-preview","o1-mini")){
    message("Note: Reasoning models do not support system prompts")
    no_system_prompt <- TRUE
  }
  
  messages <- to_api_format(llm=.llm,
                            api=api_obj,
                            no_system=no_system_prompt)
  
  
  # Handle JSON schema
  json <- FALSE
  response_format <- NULL
  if (!is.null(.json_schema)) {
    json=TRUE
    response_format <- list(
      type = "json_schema",
      json_schema = .json_schema
    )
  } 

  # Build the request body
  request_body <- list(
    messages = messages,
    frequency_penalty = .frequency_penalty,
    logit_bias = .logit_bias,
    logprobs = .logprobs,
    top_logprobs = .top_logprobs,
    max_completion_tokens = .max_completion_tokens,
    presence_penalty = .presence_penalty,
    response_format = response_format,
    seed = .seed,
    stop = .stop,
    stream = .stream,
    temperature = .temperature,
    top_p = .top_p
  ) |> purrr::compact()

  # Build the request
  request <- httr2::request(.endpoint_url) |>
    httr2::req_url_path(paste0("openai/deployments/", .deployment,"/chat/completions")) |>
    httr2::req_url_query(`api-version` = .api_version) |>
    httr2::req_headers(
      `Content-Type` = "application/json",
      `api-key` = api_key,
    )  |>
    httr2::req_body_json(data = request_body)
  
  # Return only the request object in a dry run.
  if (.dry_run) {
    return(request)
  }
  
  response <- perform_chat_request(request,api_obj,.stream,.timeout,.max_tries)
  
  
  # Extract assistant reply and rate limiting info from response headers
  assistant_reply <- response$assistant_reply
  track_rate_limit(api_obj,response$headers,.verbose)
  
    add_message(llm = .llm,
                role = "assistant", 
                content = assistant_reply , 
                json    = json,
                meta    = response$meta)
}


#' Generate Embeddings Using OpenAI API on Azure
#'
#' @param .input A character vector of texts to embed or an `LLMMesssage`object
#' @param .deployment The embedding model identifier (default: "text-embedding-3-small").
#' @param .endpoint_url Base URL for the API (default:  Sys.getenv("AZURE_ENDPOINT_URL")).
#' @param .truncate Whether to truncate inputs to fit the model's context length (default: TRUE).
#' @param .timeout Timeout for the API request in seconds (default: 120).
#' @param .dry_run If TRUE, perform a dry run and return the request object.
#' @param .max_tries Maximum retry attempts for requests (default: 3).
#' @param .api_version What API-Version othe Azure OpenAI API should be used (default: "2023-05-15")
#' @return A tibble with two columns: `input` and `embeddings`. 
#' The `input` column contains the texts sent to embed, and the `embeddings` column 
#' is a list column where each row contains an embedding vector of the sent input.
#' @export
azure_openai_embedding <- function(.input,
                                   .deployment = "text-embedding-3-small",
                                   .endpoint_url = Sys.getenv("AZURE_ENDPOINT_URL"),
                                   .api_version = "2023-05-15",
                                   .truncate = TRUE,
                                   .timeout = 120,
                                   .dry_run = FALSE,
                                   .max_tries = 3) {

  # Validate the inputs
  c(
    "Input .input must be an LLMMessage object or a character vector" = S7_inherits(.input, LLMMessage) | is.character(.input),
    "Input .deployment must be a string" = is.character(.deployment),
    "Input .truncate must be logical" = is.logical(.truncate),
    "Input .timeout must be an integer-valued numeric (seconds till timeout)" = is.numeric(.timeout) && .timeout > 0,
    ".dry_run must be logical" = is.logical(.dry_run)
  ) |> validate_inputs()
  
  # Get the Azure OpenAI API key
  api_key <- Sys.getenv("AZURE_OPENAI_API_KEY")
  if ((api_key == "")& .dry_run==FALSE){
    stop("API key is not set. Please set it with: Sys.setenv(AZURE_OPENAI_API_KEY = \"YOUR-KEY-GOES-HERE\")")
  }
  
  input_texts <- parse_embedding_input(.input)
  # Prepare the request body
  request_body <- list(
    input = input_texts
  )
  
  # Build the request
  request <- httr2::request(.endpoint_url) |>
    httr2::req_url_path(paste0("openai/deployments/", .deployment,"/embeddings")) |>
    httr2::req_url_query(`api-version` = .api_version) |>
    httr2::req_headers(
      `Content-Type` = "application/json",
      `api-key` = api_key,
    )  |>
    httr2::req_body_json(data = request_body)
  
  # Return the request object if it's a dry run
  if (.dry_run) {
    return(request)
  }
  
  extract_embeddings_fn <- function(response_content,error,response_headers){
    if(error){
      paste0("API error response - ", response_content$error$message) |>
        stop()
    }

    response_content$data |> 
      purrr::map("embedding") |>
      purrr::map(unlist)
  }
  
  # Perform a standard embedding API request
  perform_embedding_request(.request = request,
                            .timeout = .timeout,
                            .max_tries = 3,
                            .input_texts = input_texts, 
                            .fn_extract_embeddings = extract_embeddings_fn)
}


#' Azure-OpenAI Endpoint Provider Function
#'
#' The `azure_openai()` function acts as an interface for interacting with the Azure OpenAI API 
#' through main `tidyllm` verbs.
#' 
#' `azure_openai()` currently routes messages only to `azure_openai_chat()` when used with `chat()`.
#'
#' @param ... Parameters to be passed to the Azure OpenAI API specific function, 
#'   such as model configuration, input text, or API-specific options.
#' @param .called_from An internal argument that specifies which action (e.g., 
#'   `chat`) the function is being invoked from. 
#'   This argument is automatically managed and should not be modified by the user.
#'
#' @return The result of the requested action, depending on the specific function invoked 
#'   (currently, only an updated `LLMMessage` object for `azure_openai_chat()`).
#'
#' @export
azure_openai <- create_provider_function(
  .name = "azure_openai",
  chat = azure_openai_chat,
  embed = azure_openai_embedding
)


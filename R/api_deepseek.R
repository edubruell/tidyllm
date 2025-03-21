
#' The Deepseek API provider class (inherits from OpenAI)
#'
#' @noRd
api_deepseek <- new_class("Deepseek", api_openai)


#' A function to get metadata from Perplexity responses
#'
#' @noRd
method(extract_metadata, list(api_deepseek,class_list))<- function(.api,.response) {
  list(
    model             = .response$model,
    timestamp         = lubridate::as_datetime(.response$created),
    prompt_tokens     = .response$usage$prompt_tokens,
    completion_tokens = .response$usage$completion_tokens,
    total_tokens      = .response$usage$total_tokens,
    stream            = FALSE,
    specific_metadata = list(
      id        = .response$id    
    ) 
  )
}  

#' Send LLM Messages to the DeepSeek Chat API
#'
#' @description
#' This function sends a message history to the DeepSeek Chat API and returns the assistant's reply.
#' Currently tool calls cause problems on the DeepSeek API
#'
#' @param .llm An `LLMMessage` object containing the conversation history.
#' @param .model The identifier of the model to use (default: "deepseek-chat").
#' @param .max_tokens The maximum number of tokens that can be generated in the response (default: 2048).
#' @param .temperature Controls the randomness in the model's response. Values between 0 and 2 are allowed (optional).
#' @param .top_p Nucleus sampling parameter that controls the proportion of probability mass considered (optional).
#' @param .frequency_penalty Number between -2.0 and 2.0. Penalizes repeated tokens to reduce repetition (optional).
#' @param .presence_penalty Number between -2.0 and 2.0. Encourages new topics by penalizing tokens that have appeared so far (optional).
#' @param .stop One or more sequences where the API will stop generating further tokens (optional).
#' @param .stream Logical; if TRUE, streams the response piece by piece (default: FALSE).
#' @param .logprobs If TRUE, returns log probabilities of each output token (default: FALSE).
#' @param .top_logprobs Number between 0 and 5 specifying the number of top log probabilities to return (optional).
#' @param .tools Either a single TOOL object or a list of TOOL objects representing the available functions for tool calls.
#' @param .tool_choice A character string specifying the tool-calling behavior; valid values are `"none"`, `"auto"`, or `"required"` (optional).
#' @param .api_url Base URL for the DeepSeek API (default: `"https://api.deepseek.com/"`).
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .verbose If TRUE, displays additional information after the API call (default: FALSE).
#' @param .dry_run If TRUE, returns the constructed request object without executing it (default: FALSE).
#' @param .max_tries Maximum retries to perform the request (default: 3).
#'
#' @return A new `LLMMessage` object containing the original messages plus the assistant's response.
#'
#' @export
deepseek_chat <- function(.llm,
                          .model = "deepseek-chat",
                          .max_tokens = 2048,
                          .temperature = NULL,
                          .top_p = NULL,
                          .frequency_penalty = NULL,
                          .presence_penalty = NULL,
                          .stop = NULL,
                          .stream = FALSE,
                          .logprobs = NULL,
                          .top_logprobs = NULL,
                          .tools = NULL,
                          .tool_choice = NULL,
                          .api_url = "https://api.deepseek.com/",
                          .timeout = 60,
                          .verbose = FALSE,
                          .dry_run = FALSE,
                          .max_tries = 3) {

  # Validate inputs
  c(
    "Input .llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
    "Input .model must be a non-empty string" = is.character(.model) & nzchar(.model),
    "Input .max_tokens must be a positive integer" = is_integer_valued(.max_tokens) & .max_tokens > 0,
    "Input .temperature must be between 0 and 2 if provided" = is.null(.temperature) | (.temperature >= 0 & .temperature <= 2),
    "Input .top_p must be between 0 and 1 if provided" = is.null(.top_p) | (.top_p >= 0 & .top_p <= 1),
    "Input .frequency_penalty must be between -2 and 2 if provided" = is.null(.frequency_penalty) | (.frequency_penalty >= -2 & .frequency_penalty <= 2),
    "Input .presence_penalty must be between -2 and 2 if provided" = is.null(.presence_penalty) | (.presence_penalty >= -2 & .presence_penalty <= 2),
    "Input .stop must be NULL, a string, or a list of strings" = is.null(.stop) | is.character(.stop) | is.list(.stop),
    "Input .stream must be logical" = is.logical(.stream),
    "Input .logprobs must be logical or NULL" = is.null(.logprobs) | is.logical(.logprobs),
    "Input .top_logprobs must be NULL or an integer between 0 and 5" = is.null(.top_logprobs) | (is_integer_valued(.top_logprobs) & .top_logprobs >= 0 & .top_logprobs <= 5),
    "Input .tools must be NULL, a TOOL object, or a list of TOOL objects" = is.null(.tools) || S7_inherits(.tools, TOOL) || (is.list(.tools) && all(purrr::map_lgl(.tools, ~ S7_inherits(.x, TOOL)))),
    "Input .tool_choice must be NULL or a character (one of 'none', 'auto', 'required')" = is.null(.tool_choice) || (is.character(.tool_choice) && .tool_choice %in% c("none", "auto", "required")),
    "Streaming is not supported for requests with tool calls" = is.null(.tools) || !isTRUE(.stream),
    "Streaming is not supported for requests with logprobs" = is.null(.logprobs) || !isTRUE(.stream),
    "Input .tool_choice must be NULL or one of 'none', 'auto', 'required'" = is.null(.tool_choice) | (.tool_choice %in% c("none", "auto", "required")),
    "Input .api_url must be a valid URL" = is.character(.api_url) & nzchar(.api_url),
    "Input .timeout must be a positive integer" = is_integer_valued(.timeout) & .timeout > 0,
    "Input .verbose must be logical" = is.logical(.verbose),
    "Input .dry_run must be logical" = is.logical(.dry_run),
    "Input .max_tries must be integer-valued numeric" = is_integer_valued(.max_tries)
  ) |> validate_inputs()
  
  api_obj <- api_deepseek(short_name = "deepseek",
                          long_name  = "DeepSeek",
                          api_key_env_var = "DEEPSEEK_API_KEY")
  
  messages <- to_api_format(.llm,api_obj,FALSE)
  
  api_key <- get_api_key(api_obj, .dry_run)
  
  #Put a single tool into a list if only one is provided. 
  tools_def <- if (!is.null(.tools)) {
    if (S7_inherits(.tools, TOOL))  list(.tools) else .tools
  } else {
    NULL
  }
  
  # Construct request body
  request_body <- list(
    model = .model,
    messages = messages,
    max_tokens = .max_tokens,
    temperature = .temperature,
    top_p = .top_p,
    frequency_penalty = .frequency_penalty,
    presence_penalty = .presence_penalty,
    stop = .stop,
    stream = .stream,
    logprobs = .logprobs,
    top_logprobs = .top_logprobs,
    tools = if(!is.null(tools_def)) tools_to_api(api_obj,tools_def) else NULL,
    tool_choice = .tool_choice
  ) |> purrr::compact()
  
  request <- httr2::request(.api_url) |>
    httr2::req_url_path("/chat/completions") |>
    httr2::req_headers(
      `Authorization` = sprintf("Bearer %s", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(data = request_body)
  
  if (.dry_run) {
    return(request)
  }
  
  response <- perform_chat_request(request, api_obj, .stream, .timeout, .max_tries)
  if(r_has_name(response$raw,"tool_calls")){
    #Tool call logic can go here!
    tool_messages <- run_tool_calls(api_obj,
                                    response$raw$content$choices[[1]]$message$tool_calls,
                                    tools_def)
    ##Append the tool call to API
    request_body$messages <- request_body$messages |> 
      append(tool_messages)
    
    request <- request |>
      httr2::req_body_json(data = request_body)
    
    response <- perform_chat_request(request,api_obj,.stream,.timeout,.max_tries)
  }
  
  assistant_reply <- response$assistant_reply

  logprobs  <- parse_logprobs(api_obj, response$raw)
  
  
  add_message(.llm     = .llm,
              .role    = "assistant", 
              .content = assistant_reply , 
              .json    = FALSE,
              .meta    = response$meta,
              .logprobs = logprobs)
}

#' Deepseek Provider Function
#'
#' The `deepseek()` function acts as a provider interface for interacting with the Deepseek API 
#' through `tidyllm`'s `chat()` verb.
#' It dynamically routes requests to deepseek-specific function. At the moment this is only
#' `deepseek_chat()`
#'
#' @param ... Parameters to be passed to the appropriate Deepseek-specific function, 
#'   such as model configuration, input text, or API-specific options.
#' @param .called_from An internal argument specifying which action (e.g., 
#'   `chat`, `embed`) the function is invoked from. 
#'   This argument is automatically managed by the `tidyllm` verbs and should not be modified by the user.
#'
#' @return The result of the requested action, depending on the specific function invoked 
#'   (e.g., an updated `LLMMessage` object for `chat()`).
#'
#' @export
deepseek <- create_provider_function(
  .name = "deepseek",
  chat = deepseek_chat
)

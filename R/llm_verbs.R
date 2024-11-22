#' Create a Provider Function for Routing LLM Actions
#'
#' This function generates a provider-specific function that dynamically routes 
#' different LLM-related actions (e.g., chat, generate embeddings, batch operations) 
#' based on the `.called_from` argument or the presence of an `LLMMessage` object.
#'
#' @param .name A string representing the name of the provider (e.g., "openai").
#' @param ... Named functions corresponding to the various actions the provider supports 
#'   (e.g., `chat`, `embed`, `send_batch`).
#'
#' @return A function that dynamically routes to the appropriate action based on its 
#'   inputs.
#' @noRd 
create_provider_function <- function(.name, ...) {
  function_map <- list(...)
  
  if ("metadata" %in% names(function_map)) {
    stop(glue::glue("'metadata' is a reserved keyword and cannot be used as a function name in provider '{.name}'."))
  }
  
  # Capture the arguments used in the provider function
  supported_args <- purrr::map(function_map, formals) |> 
    purrr::map(names)
  
  # Create the provider function
  provider_function <- function(..., .called_from = NULL) {
    args <- list(...)
    
    if (!is.null(.called_from)) {
      if (.called_from == "metadata") {
        # Return provider metadata
        return(list(
          provider_name = .name,
          supported_args = supported_args
        ))
      } else if (.called_from %in% names(function_map)) {
        # Call the appropriate function
        return(do.call(function_map[[.called_from]], args))
      } else {
        stop(glue::glue("'{.called_from}' is not available for API-provider '{.name}'."), call. = FALSE)
      }
    }
    
    # Check if any argument is an LLMMessage
    if (any(purrr::map_lgl(args, ~ S7_inherits(.x, LLMMessage)))) {
      lifecycle::deprecate_warn(
        when = "0.2.3",
        what = glue::glue("{.name}(.llm)"),
        details = glue::glue(
          "Passing an `LLMMessage` directly to `{.name}()` is deprecated.\n",
          "Please use either `chat({.name}(...))` for a verb-based approach, or `{.name}_chat()`."
        ))
      return(do.call(function_map[["chat"]], args))
    }
    
    # Default: Construct and return an unevaluated call to the provider function
    return(rlang::call2(.name, !!!args))
  }
  
  # Return the provider function with metadata accessible via "metadata" call
  provider_function
}



#' Chat with a Language Model
#'
#' The `chat()` function sends a message to a language model via a specified provider and returns the response. 
#' It routes the provided `LLMMessage` object to the appropriate provider-specific chat function, 
#' while allowing for the specification of common arguments applicable across different providers.
#'
#' @param .llm An `LLMMessage` object containing the message or conversation history to send to the language model.
#' @param .provider A function or function call specifying the language model provider and any additional parameters.
#'   This should be a call to a provider function like `openai()`, `claude()`, etc. 
#'   You can also set a default provider function via the `tidyllm_chat_default` option.
#' @param .dry_run Logical; if `TRUE`, simulates the request without sending it to the provider. Useful for testing.
#' @param .stream Logical; if `TRUE`, streams the response from the provider in real-time.
#' @param .temperature Numeric; controls the randomness of the model's output (0 = deterministic).
#' @param .timeout Numeric; the maximum time (in seconds) to wait for a response.
#' @param .top_p Numeric; nucleus sampling parameter, which limits the sampling to the top cumulative probability `p`.
#' @param .max_tries Integer; the maximum number of retries for failed requests.
#' @param .model Character; the model identifier to use (e.g., `"gpt-4"`).
#' @param .verbose Logical; if `TRUE`, prints additional information about the request and response.
#' @param .json_schema List; A JSON schema object as R list to enforce the output structure 
#' @param .seed Integer; sets a random seed for reproducibility.
#' @param .stop Character vector; specifies sequences where the model should stop generating further tokens.
#' @param .frequency_penalty Numeric; adjusts the likelihood of repeating tokens (positive values decrease repetition).
#' @param .presence_penalty Numeric; adjusts the likelihood of introducing new tokens (positive values encourage novelty).
#'
#' @return An updated `LLMMessage` object containing the response from the language model.
#'
#' @examples
#' \dontrun{
#' # Basic usage with OpenAI provider
#' llm_message("Hello World") |>
#'    chat(ollama(.ollama_server = "https://my-ollama-server.de"),.model="mixtral")
#'    
# llm_message("Hello World") |>
#'    chat(mistral,.model="mixtral")
#'
#' # Use streaming with Claude provider
#' llm_message("Tell me a story") |>
#'    chat(claude(),.stream=TRUE)
#' }
#'
#' @details
#' The `chat()` function provides a unified interface for interacting with different language model providers.
#' Common arguments such as `.temperature`, `.model`, and `.stream` are supported by most providers and can be
#' passed directly to `chat()`. If a provider does not support a particular argument, an error will be raised.
#'
#' Advanced provider-specific configurations can be accessed via the provider functions. 
#'
#' @export
chat <- function(
    .llm,
    .provider = getOption("tidyllm_chat_default"),
    .dry_run = NULL,
    .stream = NULL,
    .temperature = NULL,
    .timeout = NULL,
    .top_p = NULL,
    .max_tries = NULL,
    .model = NULL,
    .verbose = NULL,
    .json_schema = NULL,
    .seed = NULL,
    .stop = NULL,
    .frequency_penalty = NULL,
    .presence_penalty = NULL) {
  
  if (!S7_inherits(.llm, LLMMessage)) {
    stop("Input .llm must be an LLMMessage object.")
  }
  
  if (is.null(.provider)) {
    stop("You need to specify a .provider function in chat().")
  }
  
  # Evaluate or capture the provider
  if (rlang::is_function(.provider)) {
    .provider <- .provider()
  }
  
  # Capture the provider call expression
  if(!rlang::is_call(.provider)){
    provider_expr <- rlang::quo_get_expr(rlang::enquo(.provider))
  } else {
    provider_expr <- .provider
  }
  
  provider_meta_data <- rlang::call_modify(provider_expr ,
                                           .called_from = "metadata") |>
    rlang::eval_tidy()
  
  supported_args <- provider_meta_data$supported_args$chat

  # Collect common arguments only if they are not NULL
  common_args <- list(
    .llm = .llm,
    .model = .model,
    .verbose = .verbose,
    .max_tries = .max_tries,
    .stream = .stream,
    .timeout = .timeout,
    .temperature = .temperature,
    .dry_run = .dry_run,
    .top_p = .top_p,
    .json_schema = .json_schema,
    .seed = .seed,
    .stop = .stop,
    .frequency_penalty = .frequency_penalty,
    .presence_penalty = .presence_penalty
  )
  common_args <- common_args[!sapply(common_args, is.null)]
  
  # Warn about unsupported arguments
  unsupported_args <- setdiff(names(common_args), supported_args)
  if (length(unsupported_args) > 0) {
    stop(glue::glue(
      "The following arguments are not supported by the provider's `chat()` function: {paste(unsupported_args, collapse = ', ')}."
    ))
  }
  
  # Inject valid arguments into the provider function call
  valid_args <- common_args[names(common_args) %in% supported_args]
  valid_args <- valid_args |> append(list(.called_from= "chat")) 
  modified_call <- rlang::call_modify(provider_expr, !!!valid_args)
  
  return(rlang::eval_tidy(modified_call))
}


#' Generate text embeddings
#'
#' The `embed()` function allows you to embed a text via a specified provider.
#' It routes the input to the appropriate provider-specific embedding function.
#'
#' @param .input  A character vector of texts to embed or an `LLMMessage` object
#' @param .provider A function or function call specifying the language model provider and any additional parameters.
#'   This should be a call to a provider function like `openai()`, `ollama()`, etc. 
#'   You can also set a default provider function via the `tidyllm_embed_default` option.
#' @param .model The embedding model to use
#' @param .truncate Whether to truncate inputs to fit the model's context length
#' @param .timeout Timeout for the API request in seconds
#' @param .dry_run If TRUE, perform a dry run and return the request object.
#' @param .max_tries Maximum retry attempts for requests
#' @return A tibble with two columns: `input` and `embeddings`. 
#' The `input` column contains the texts sent to embed, and the `embeddings` column 
#' is a list column where each row contains an embedding vector of the sent input.
#' @examples
#' \dontrun{
#'c("What is the meaning of life, the universe and everything?",
#'  "How much wood would a woodchuck chuck?",
#'  "How does the brain work?") |>
#'  embed(gemini)
#'  }
#' @export
embed <- function(.input, 
                  .provider = getOption("tidyllm_embed_default"),
                  .model=NULL,
                  .truncate=NULL,
                  .timeout=NULL,
                  .dry_run=NULL,
                  .max_tries=NULL) {

  # Validate the inputs
  c(
    "Input .input must be a character vector or an LLMMessage object" = S7_inherits(.input, LLMMessage) | is.character(.input),
    "You need to specify a .provider function in embed()" = !is.null(.provider)
  ) |> validate_inputs()
  
  #Accept an unevaluated function
  if(rlang::is_function(.provider)){
    .provider <- .provider()
  }
  
  # Capture the provider call expression
  if(!rlang::is_call(.provider)){
    provider_expr <- rlang::quo_get_expr(rlang::enquo(.provider))
  } else {
    provider_expr <- .provider
  }
  
  
  provider_meta_data <- rlang::call_modify(provider_expr ,
                                           .called_from = "metadata") |>
    rlang::eval_tidy()
  
  supported_args <- provider_meta_data$supported_args$embed
  
  # Collect common arguments only if they are not NULL
  common_args <- list(
    .input = .input,
    .model = .model,
    .truncate = .truncate,
    .max_tries = .max_tries,
    .timeout = .timeout,
    .dry_run = .dry_run
  )
  common_args <- common_args[!sapply(common_args, is.null)]
  
  # Throw an error for  unsupported arguments
  unsupported_args <- setdiff(names(common_args), supported_args)
  if (length(unsupported_args) > 0) {
    stop(glue::glue(
      "The following arguments are not supported by the provider's `embed()` function: {paste(unsupported_args, collapse = ', ')}."
    ))
  }
  
  # Inject valid arguments into the provider function call
  valid_args <- common_args[names(common_args) %in% supported_args]
  valid_args <- valid_args |> append(list(.called_from= "embed")) 
  modified_call <- rlang::call_modify(provider_expr, !!!valid_args)
  
  return(rlang::eval_tidy(modified_call))
}


#' Send a batch of messages to a batch API
#'
#' The `send_batch()` function allows you to send a list of `LLMMessage` objects
#' to an API.
#' It routes the input to the appropriate provider-specific batch API function.
#'
#' @param .llms A list of `LLMMessage` objects containing conversation histories.
#' @param .provider A function or function call specifying the language model provider and any additional parameters.
#'   This should be a call to a provider function like `openai()`, `claude()`, etc. 
#'   You can also set a default provider function via the `tidyllm_sbatch_default` option.
#' @param .model Character; the model identifier to use (e.g., `"gpt-4"`).
#' @param .temperature Numeric; controls the randomness of the model's output (0 = deterministic).
#' @param .timeout Numeric; the maximum time (in seconds) to wait for a response.
#' @param .top_p Numeric; nucleus sampling parameter, which limits the sampling to the top cumulative probability `p`.
#' @param .max_tries Integer; the maximum number of retries for failed requests.
#' @param .verbose Logical; if `TRUE`, prints additional information about the request and response.
#' @param .json_schema List; A JSON schema object as R list to enforce the output structure 
#' @param .seed Integer; sets a random seed for reproducibility.
#' @param .stop Character vector; specifies sequences where the model should stop generating further tokens.
#' @param .frequency_penalty Numeric; adjusts the likelihood of repeating tokens (positive values decrease repetition).
#' @param .presence_penalty Numeric; adjusts the likelihood of introducing new tokens (positive values encourage novelty).
#' @param .dry_run Logical; if `TRUE`, simulates the request without sending it to the provider. Useful for testing.
#' @param .id_prefix Character string to specify a prefix for generating custom IDs when names in `.llms` are missing 
#' @return An updated and named list of `.llms` with identifiers that align with batch responses, including a `batch_id` attribute.
#' @export
send_batch <- function(.llms, 
                       .provider = getOption("tidyllm_sbatch_default"),
                       .dry_run = NULL,
                       .temperature = NULL,
                       .timeout = NULL,
                       .top_p = NULL,
                       .max_tries = NULL,
                       .model = NULL,
                       .verbose = NULL,
                       .json_schema = NULL,
                       .seed = NULL,
                       .stop = NULL,
                       .frequency_penalty = NULL,
                       .presence_penalty = NULL,
                       .id_prefix = NULL){

  # Validate the inputs
  c(
    ".llms must be a list of LLMMessage objects" = is.list(.llms) && all(sapply(.llms, S7_inherits, LLMMessage)),
    "You need to specify a .provider function in send_batch()" = !is.null(.provider)
  ) |> validate_inputs()
  
  #Accept an unevaluated function
  if(rlang::is_function(.provider)){
    .provider <- .provider()
  }
  
  # Capture the provider call expression
  if(!rlang::is_call(.provider)){
    provider_expr <- rlang::quo_get_expr(rlang::enquo(.provider))
  } else {
    provider_expr <- .provider
  }
  
  provider_meta_data <- rlang::call_modify(provider_expr ,
                                           .called_from = "metadata") |>
    rlang::eval_tidy()
  
  supported_args <- provider_meta_data$supported_args$send_batch
  
  # Collect common arguments only if they are not NULL
  common_args <- list(
    .llms = .llms,
    .model = .model,
    .verbose = .verbose,
    .max_tries = .max_tries,
    .timeout = .timeout,
    .temperature = .temperature,
    .dry_run = .dry_run,
    .top_p = .top_p,
    .json_schema = .json_schema,
    .seed = .seed,
    .stop = .stop,
    .frequency_penalty = .frequency_penalty,
    .presence_penalty = .presence_penalty,
    .id_prefix = .id_prefix
  )
  common_args <- common_args[!sapply(common_args, is.null)]
  
  # Warn about unsupported arguments
  unsupported_args <- setdiff(names(common_args), supported_args)
  if (length(unsupported_args) > 0) {
    stop(glue::glue(
      "The following arguments are not supported by the provider's `send_batch()` function: {paste(unsupported_args, collapse = ', ')}."
    ))
  }
  
  # Inject valid arguments into the provider function call
  valid_args <- common_args[names(common_args) %in% supported_args]
  valid_args <- valid_args |> append(list(.called_from= "send_batch")) 
  modified_call <- rlang::call_modify(provider_expr, !!!valid_args)
  
  return(rlang::eval_tidy(modified_call))
}

#' Check Batch Processing Status 
#'
#' This function retrieves the processing status and other details of a specified 
#' batchid or a list of `LLMMessage` objects with batch attribute.
#' It routes the input to the appropriate provider-specific batch API function.
#'
#' @param .llms A list of `LLMMessage` objects or a character vector with a batch ID.
#' @param .provider A function or function call specifying the language model provider and any additional parameters.
#'   This should be a call to a provider function like `openai()`, `claude()`, etc. 
#'   You can also set a default provider function via the `tidyllm_cbatch_default` option.
#' @param .dry_run Logical; if TRUE, returns the prepared request object without executing it 
#' @param .max_tries Maximum retries to perform the request
#' @param .timeout Integer specifying the request timeout in seconds
#' @return A tibble with information about the status of batch processing.
#' @export
check_batch <- function(.llms,
                        .provider = getOption("tidyllm_cbatch_default"),
                        .dry_run  = NULL,
                        .max_tries = NULL,
                        .timeout = NULL) {

  # Validate the inputs
  c(
    ".llms must be a list of LLMMessage objects or a character vector with a Batch ID" = (is.list(.llms) && all(sapply(.llms, S7_inherits, LLMMessage))) | is.character(.llms),
    "You need to specify a .provider function in check_batch()" = !is.null(.provider)
  ) |> validate_inputs()
  
  
  # Extract batch_id
  if((is.list(.llms) && all(sapply(.llms, S7_inherits, LLMMessage)))){
    batch_id <- attr(.llms, "batch_id")
  }
  if(is.character(.llms)){
    batch_id <- .llms
  }
  
  #Accept an unevaluated function
  if(rlang::is_function(.provider)){
    .provider <- .provider()
  }
  
  # Capture the provider call expression
  if(!rlang::is_call(.provider)){
    provider_expr <- rlang::quo_get_expr(rlang::enquo(.provider))
  } else {
    provider_expr <- .provider
  }
  
  
  # Collect common arguments only if they are not NULL
  common_args <- list(
    .batch_id = batch_id,
    .max_tries = .max_tries,
    .timeout = .timeout,
    .dry_run = .dry_run
  )
  common_args <- common_args[!sapply(common_args, is.null)]
  
  valid_args <- common_args |> append(list(.called_from= "check_batch")) 
  modified_call <- rlang::call_modify(provider_expr, !!!valid_args)
  
  # Evaluate the modified provider call
  return(rlang::eval_tidy(modified_call))
}

#' List all Batch Requests on a Batch API
#'
#' @param .provider A function or function call specifying the language model provider and any additional parameters.
#'   This should be a call to a provider function like `openai()`, `claude()`, etc. 
#'   You can also set a default provider function via the `tidyllm_lbatch_default` option.
#' @return A tibble with information about the status of batch processing.
#' @export
list_batches <- function(.provider = getOption("tidyllm_lbatch_default")) {
  
  if (is.null(.provider)) {
    stop("You need to specify a .provider function in list_batches().")
  }
  
  #Accept an unevaluated function
  if(rlang::is_function(.provider)){
    .provider <- .provider()
  }
  
  # Capture the provider call expression
  if(!rlang::is_call(.provider)){
    provider_expr <- rlang::quo_get_expr(rlang::enquo(.provider))
  } else {
    provider_expr <- .provider
  }
  
  # Modify the provider call by injecting .llm and .called_from
  modified_call <- rlang::call_modify(
    provider_expr,
    .called_from = "list_batches" 
  )
  
  # Evaluate the modified provider call
  return(rlang::eval_tidy(modified_call))
}


#' Fetch Results from a Batch API
#'
#' This function retrieves the results of a completed  batch and updates
#' the provided list of `LLMMessage` objects with the responses. It aligns each
#' response with the original request using the `custom_id`s generated in `send_batch()`.
#'
#' The function routes the input to the appropriate provider-specific batch API function.
#'
#' @param .llms A list of `LLMMessage` objects containing conversation histories.
#' @param .provider A function or function call specifying the language model provider and any additional parameters.
#'   This should be a call to a provider function like `openai()`, `claude()`, etc. 
#'   You can also set a default provider function via the `tidyllm_fbatch_default` option.
#' @param .dry_run Logical; if `TRUE`, returns the constructed request without executing it
#' @param .max_tries Integer; maximum number of retries if the request fails
#' @param .timeout Integer; request timeout in seconds
#' @return A list of updated `LLMMessage` objects, each with the assistant's response added if successful.
#' @export
fetch_batch <- function(.llms, 
                        .provider = getOption("tidyllm_fbatch_default"),
                        .dry_run=NULL,
                        .max_tries=NULL,
                        .timeout=NULL) {

  # Validate the inputs
  c(
    ".llms must be a list of LLMMessage objects with names as custom IDs" = is.list(.llms) && all(sapply(.llms, S7_inherits, LLMMessage)),
    "You need to specify a .provider function in fetch_batch()" = !is.null(.provider)
  ) |> validate_inputs()
  
  #Accept an unevaluated function
  if(rlang::is_function(.provider)){
    .provider <- .provider()
  }
  
  # Capture the provider call expression
  if(!rlang::is_call(.provider)){
    provider_expr <- rlang::quo_get_expr(rlang::enquo(.provider))
  } else {
    provider_expr <- .provider
  }
  
  # Collect common arguments only if they are not NULL
  common_args <- list(
    .llms = .llms,
    .max_tries = .max_tries,
    .timeout = .timeout,
    .dry_run = .dry_run
  )
  common_args <- common_args[!sapply(common_args, is.null)]
  
  valid_args <- common_args |> append(list(.called_from= "fetch_batch")) 
  modified_call <- rlang::call_modify(provider_expr, !!!valid_args)
  
  # Evaluate the modified provider call
  return(rlang::eval_tidy(modified_call))
}















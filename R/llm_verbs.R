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
dispatch_to_provider <- function(provider_expr, verb_name, common_args,
                                  validate = TRUE, env = rlang::caller_env()) {
  if (validate) {
    meta      <- rlang::call_modify(provider_expr, .called_from = "metadata") |> rlang::eval_tidy()
    supported <- meta$supported_args[[verb_name]]
    unsupported <- setdiff(names(common_args), supported)
    if (length(unsupported) > 0) {
      stop(glue::glue(
        "The following arguments are not supported by the provider's `{verb_name}()` function: {paste(unsupported, collapse = ', ')}."
      ))
    }
    valid_args <- common_args[names(common_args) %in% supported]
  } else {
    valid_args <- common_args
  }
  valid_args$.called_from <- verb_name
  rlang::eval_tidy(rlang::call_modify(provider_expr, !!!valid_args), env = env)
}

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
#' @param .tools Either a single TOOL object or a list of TOOL objects representing the available functions for tool calls.
#' @param .max_tool_rounds Integer; the maximum number of tool use iterations for multi-turn tool calling (default varies by provider).
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
    .tools = NULL,
    .max_tool_rounds = NULL,
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
    .presence_penalty = .presence_penalty,
    .tools = .tools,
    .max_tool_rounds = .max_tool_rounds
  )
  common_args <- common_args[!sapply(common_args, is.null)]

  # Validate attachments in the last user message before dispatch
  provider_meta <- rlang::call_modify(provider_expr, .called_from = "metadata") |> rlang::eval_tidy()
  validate_message_attachments(.llm, provider_meta$provider_name)

  return(dispatch_to_provider(provider_expr, "chat", common_args))
}


#' Generate text embeddings
#'
#' The `embed()` function allows you to embed a text via a specified provider.
#' It routes the input to the appropriate provider-specific embedding function.
#'
#' @param .input  A character vector of texts v, a list of texts and image objects,  or an `LLMMessage` object
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
    "Input .input must be a character vector, a list or an LLMMessage object" = S7_inherits(.input, LLMMessage) | is.character(.input) | is.list(.input),
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
  
  
  common_args <- list(
    .input = .input,
    .model = .model,
    .truncate = .truncate,
    .max_tries = .max_tries,
    .timeout = .timeout,
    .dry_run = .dry_run
  )
  common_args <- common_args[!sapply(common_args, is.null)]

  return(dispatch_to_provider(provider_expr, "embed", common_args))
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

  return(dispatch_to_provider(provider_expr, "send_batch", common_args))
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
  
  
  common_args <- list(
    .batch_id = batch_id,
    .max_tries = .max_tries,
    .timeout = .timeout,
    .dry_run = .dry_run
  )
  common_args <- common_args[!sapply(common_args, is.null)]

  return(dispatch_to_provider(provider_expr, "check_batch", common_args, validate = FALSE))
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
  
  return(dispatch_to_provider(provider_expr, "list_batches", list(), validate = FALSE))
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
  
  common_args <- list(
    .llms = .llms,
    .max_tries = .max_tries,
    .timeout = .timeout,
    .dry_run = .dry_run
  )
  common_args <- common_args[!sapply(common_args, is.null)]

  return(dispatch_to_provider(provider_expr, "fetch_batch", common_args, validate = FALSE))
}

#' Run Deep Research via a Provider
#'
#' The `deep_research()` function sends a message to a provider's deep research endpoint.
#' Currently supported: Perplexity (`sonar-deep-research` via async API).
#'
#' @param .llm An `LLMMessage` object containing the research question.
#' @param .provider A function or function call specifying the provider (e.g., `perplexity()`).
#' @param .background Logical; if TRUE, returns a `tidyllm_research_job` immediately (default: FALSE).
#' @param ... Additional arguments passed to the provider's deep research function.
#'
#' @return If `.background = FALSE`, an `LLMMessage` with the research reply.
#'   If `.background = TRUE`, a `tidyllm_research_job` for use with `check_job()`/`fetch_job()`.
#' @export
deep_research <- function(.llm, .provider, .background = FALSE, ...) {
  if (!S7_inherits(.llm, LLMMessage)) {
    stop("Input .llm must be an LLMMessage object.")
  }
  if (is.null(.provider)) {
    stop("You need to specify a .provider function in deep_research().")
  }
  if (rlang::is_function(.provider)) {
    .provider <- .provider()
  }
  if (!rlang::is_call(.provider)) {
    provider_expr <- rlang::quo_get_expr(rlang::enquo(.provider))
  } else {
    provider_expr <- .provider
  }
  common_args <- list(.llm = .llm, .background = .background, ...)
  return(dispatch_to_provider(provider_expr, "deep_research", common_args))
}


#' Check the Status of a Batch or Research Job
#'
#' `check_job()` dispatches to `check_batch()` for batch objects or
#' `perplexity_check_research()` / `openai_check_research()` for `tidyllm_research_job` objects.
#'
#' @param .job An object with a `batch_id` attribute (from `send_batch()`) or
#'   a `tidyllm_research_job` (from `deep_research(.background = TRUE)`).
#' @param ... Additional arguments passed to the underlying function.
#' @return Status information; type depends on `.job` class.
#' @export
check_job <- function(.job, ...) {
  if (!is.null(attr(.job, "batch_id"))) {
    check_batch(.job, ...)
  } else if (inherits(.job, "tidyllm_research_job")) {
    provider <- .job$provider %||% "perplexity"
    if (provider == "openai") {
      openai_check_research(.job, ...)
    } else {
      perplexity_check_research(.job, ...)
    }
  } else {
    stop("check_job() expects an object with a 'batch_id' attribute or a tidyllm_research_job.")
  }
}


#' Fetch Results from a Batch or Research Job
#'
#' `fetch_job()` dispatches to `fetch_batch()` for batch objects or
#' `perplexity_fetch_research()` / `openai_fetch_research()` for `tidyllm_research_job` objects.
#'
#' @param .job An object with a `batch_id` attribute (from `send_batch()`) or
#'   a `tidyllm_research_job` (from `deep_research(.background = TRUE)`).
#' @param .provider A provider function (required for batch jobs, ignored for research jobs).
#' @param ... Additional arguments passed to the underlying function.
#' @return Fetched results; type depends on `.job` class.
#' @export
fetch_job <- function(.job, .provider = NULL, ...) {
  if (!is.null(attr(.job, "batch_id"))) {
    fetch_batch(.job, .provider, ...)
  } else if (inherits(.job, "tidyllm_research_job")) {
    provider <- .job$provider %||% "perplexity"
    if (provider == "openai") {
      openai_fetch_research(.job, ...)
    } else {
      perplexity_fetch_research(.job, ...)
    }
  } else {
    stop("fetch_job() expects an object with a 'batch_id' attribute or a tidyllm_research_job.")
  }
}


#' List Available Models for a Provider
#'
#' The `list_models()` function retrieves available models from the specified provider.
#'
#' @param .provider A function or function call specifying the provider and any additional parameters.
#'   You can also set a default provider via the `tidyllm_lmodels_default` option.
#' @param ... Additional arguments to be passed to the provider-specific list_models function.
#'
#' @return A tibble containing model information.
#' @export
list_models <- function(.provider = getOption("tidyllm_lmodels_default"), ...) {
  if (is.null(.provider)) {
    stop("You need to specify a .provider function in list_models().")
  }
  
  # Accept an unevaluated function
  if (rlang::is_function(.provider)) {
    .provider <- .provider()
  }
  
  # Capture the provider call expression
  if (!rlang::is_call(.provider)) {
    provider_expr <- rlang::quo_get_expr(rlang::enquo(.provider))
  } else {
    provider_expr <- .provider
  }
  
  extra_args <- list(...)
  return(dispatch_to_provider(provider_expr, "list_models", extra_args, validate = FALSE))
}


#' Upload a File to a Provider's File Store
#'
#' @param .provider A provider function call (e.g. `claude()`, `gemini()`, `openai()`).
#' @param .path Path to the local file to upload.
#' @param ... Additional provider-specific arguments.
#' @return A `tidyllm_file` object.
#' @export
upload_file <- function(.provider, .path, ...) {
  if (is.null(.provider)) stop("You need to specify a .provider function in upload_file().")
  if (rlang::is_function(.provider)) .provider <- .provider()
  if (!rlang::is_call(.provider)) {
    provider_expr <- rlang::quo_get_expr(rlang::enquo(.provider))
  } else {
    provider_expr <- .provider
  }
  extra_args <- c(list(.path = .path), list(...))
  dispatch_to_provider(provider_expr, "upload_file", extra_args, validate = FALSE)
}


#' List Files Stored on a Provider
#'
#' @param .provider A provider function call.
#' @param ... Additional provider-specific arguments.
#' @return A tibble of file metadata.
#' @export
list_files <- function(.provider, ...) {
  if (is.null(.provider)) stop("You need to specify a .provider function in list_files().")
  if (rlang::is_function(.provider)) .provider <- .provider()
  if (!rlang::is_call(.provider)) {
    provider_expr <- rlang::quo_get_expr(rlang::enquo(.provider))
  } else {
    provider_expr <- .provider
  }
  dispatch_to_provider(provider_expr, "list_files", list(...), validate = FALSE)
}


#' Get Metadata for a File Stored on a Provider
#'
#' @param .provider A provider function call.
#' @param .file_id The file ID string to look up, or a `tidyllm_file` object returned by `upload_file()`.
#' @param ... Additional provider-specific arguments.
#' @return A single-row tibble of file metadata.
#' @export
file_info <- function(.provider, .file_id, ...) {
  if (is.null(.provider)) stop("You need to specify a .provider function in file_info().")
  if (S7::S7_inherits(.file_id, tidyllm_file)) .file_id <- .file_id@id
  if (rlang::is_function(.provider)) .provider <- .provider()
  if (!rlang::is_call(.provider)) {
    provider_expr <- rlang::quo_get_expr(rlang::enquo(.provider))
  } else {
    provider_expr <- .provider
  }
  extra_args <- c(list(.file_id = .file_id), list(...))
  dispatch_to_provider(provider_expr, "file_info", extra_args, validate = FALSE)
}


#' Delete a File from a Provider's File Store
#'
#' @param .provider A provider function call.
#' @param .file_id The file ID string to delete, or a `tidyllm_file` object returned by `upload_file()`.
#' @param ... Additional provider-specific arguments.
#' @return Invisibly NULL; prints a confirmation message.
#' @export
delete_file <- function(.provider, .file_id, ...) {
  if (is.null(.provider)) stop("You need to specify a .provider function in delete_file().")
  if (S7::S7_inherits(.file_id, tidyllm_file)) .file_id <- .file_id@id
  if (rlang::is_function(.provider)) .provider <- .provider()
  if (!rlang::is_call(.provider)) {
    provider_expr <- rlang::quo_get_expr(rlang::enquo(.provider))
  } else {
    provider_expr <- .provider
  }
  extra_args <- c(list(.file_id = .file_id), list(...))
  dispatch_to_provider(provider_expr, "delete_file", extra_args, validate = FALSE)
}


#' Validate Attachments in the Last User Message
#'
#' Called by chat() before dispatch. Raises errors for provider mismatches or
#' unsupported media types in the current (last) user message only.
#' Historical messages are not validated here; to_api_format() silently skips them.
#'
#' @noRd
validate_message_attachments <- function(.llm, provider_name) {
  history <- .llm@message_history
  user_msgs <- which(vapply(history, function(m) identical(m$role, "user"), logical(1)))
  if (length(user_msgs) == 0) return(invisible(NULL))

  last_user_idx <- user_msgs[length(user_msgs)]
  last_msg <- history[[last_user_idx]]

  # Validate remote file references
  if (!is.null(last_msg$files) && length(last_msg$files) > 0) {
    providers_with_files <- c("claude", "gemini", "openai")
    if (!provider_name %in% providers_with_files) {
      stop(glue::glue(
        "The '{provider_name}' provider does not support file references in messages.\n",
        "Use pdf_file() / audio_file() for inline binary instead."
      ))
    }
    for (f in last_msg$files) {
      if (f@provider != provider_name) {
        stop(glue::glue(
          "File '{f@filename}' (id={f@id}) was uploaded to '{f@provider}' but this conversation ",
          "is using '{provider_name}'. Files are stored on the provider's servers and cannot be ",
          "shared across providers. Re-upload with upload_file({provider_name}(), .path = ...)."
        ))
      }
    }
  }

  # Validate inline media types
  providers_supporting_audio  <- c("gemini", "openrouter", "llamacpp", "mistral")
  providers_supporting_video  <- c("gemini", "openrouter")
  providers_supporting_binary_pdf <- c("gemini", "openrouter")

  if (!is.null(last_msg$media) && length(last_msg$media) > 0) {
    for (m in last_msg$media) {
      if (S7_inherits(m, tidyllm_audio) && !provider_name %in% providers_supporting_audio) {
        stop(glue::glue(
          "The '{provider_name}' provider does not support inline audio.\n",
          "Use gemini() or openrouter() for audio transcription."
        ))
      }
      if (S7_inherits(m, tidyllm_video) && !provider_name %in% providers_supporting_video) {
        stop(glue::glue(
          "The '{provider_name}' provider does not support inline video.\n",
          "Use gemini() or openrouter() for video analysis."
        ))
      }
      # pdf_file() is exempt: it carries a text fallback used automatically
    }
  }

  invisible(NULL)
}














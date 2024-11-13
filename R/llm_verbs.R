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
  # Capture the named functions as a list
  function_map <- list(...)
  
  # Create the provider function
  provider_function <- function(..., .called_from = NULL) {
    args <- list(...)
    
    if (!is.null(.called_from)) {
      # Check if the .called_from matches one of the named functions
      if (.called_from %in% names(function_map)) {
        # Call the appropriate function
        return(do.call(function_map[[.called_from]], args))
      } else {
        stop(glue::glue("'{.called_from}' is not available for API-provider '{.name}'."), call. = FALSE)
      }
    }
    
    # Check if any argument is an LLMMessage
    if (any(purrr::map_lgl(args, ~ inherits(.x, "LLMMessage")))) {
      lifecycle::deprecate_warn(
        when = "0.2.3",
        what = glue::glue("{.name}(.llm)"),
        details = glue::glue(
          "Passing an `LLMMessage` directly to `{.name}()` is deprecated.\n",
          "Please use either `chat({.name}(...))` for a verb-based approach, or `{.name}_chat()`."
        ))
      return(do.call(function_map[["chat"]], args))
      
    }
    
    # Construct and return an unevaluated call to the provider function
    return(rlang::call2(.name, !!!args))
  }
  
  attr(provider_function, "name") <- .name
  provider_function
}


#' Chat with a Language Model
#'
#' The `chat()` function allows you to send a message to a language model via a specified provider.
#' It routes the `LLMMessage` object to the appropriate provider-specific chat function.
#'
#' @param .llm An `LLMMessage` object containing the message or conversation history to send to the language model.
#' @param .provider A function or function call specifying the language model provider and any additional parameters.
#'   This should be a call to a provider function like `openai()`, `claude()`, etc. 
#'   You can also set a default provider function via the `tidyllm_chat_default` option.
#' @return An `LLMMessage`  object containing the response from the language model.
#' @examples
#' \dontrun{
#' llm_message("Hello World") |>
#'    chat(openai(.model = "gpt-4o"))
#' }
#' @export
chat <- function(.llm, .provider = getOption("tidyllm_chat_default")) {
  # Ensure .llm is an LLMMessage object
  if (!inherits(.llm, "LLMMessage")) {
    stop("Input .llm must be an LLMMessage object.")
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
    provider_expr ,
    .llm = .llm,
    .called_from = "chat" 
  )
  
  # Evaluate the modified provider call
  return(rlang::eval_tidy(modified_call))
}

#' Generate embeddings
#'
#' The `embed()` function allows you to embed a text via a specified provider.
#' It routes the input to the appropriate provider-specific embedding function.
#'
#' @param .llm An existing `LLMMessage` object (or a character vector of texts to embed)
#' @param .provider A function or function call specifying the language model provider and any additional parameters.
#'   This should be a call to a provider function like `openai()`, `ollama()`, etc. 
#'   You can also set a default provider function via the `tidyllm_embed_default` option.
#'@return A matrix where each column corresponds to an vector embedding of a sent text
#' @examples
#' \dontrun{
#'c("What is the meaning of life?",
#'  "How much wood would a woodchuck chuck?",
#'  "How does the brain work?") |>
#'  embed(gemini())
#'  }
#' @export
embed <- function(.llm, .provider = getOption("tidyllm_embed_default")) {
  
  # Validate the inputs
  c(
    "Input .llm must be an LLMMessage object or a character vector" = inherits(.llm, "LLMMessage") | is.character(.llm)
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
  
  # Modify the provider call by injecting .llm and .called_from
  modified_call <- rlang::call_modify(
    provider_expr ,
    .llm = .llm,
    .called_from = "embed" 
  )
  
  # Evaluate the modified provider call
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
#' @return An updated and named list of `.llms` with identifiers that align with batch responses, including a `batch_id` attribute.
send_batch <- function(.llms, .provider = getOption("tidyllm_sbatch_default")) {
  
  # Validate the inputs
  c(
    ".llms must be a list of LLMMessage objects" = is.list(.llms) && all(sapply(.llms, inherits, "LLMMessage"))
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
  
  # Modify the provider call by injecting .llm and .called_from
  modified_call <- rlang::call_modify(
    provider_expr ,
    .llms = .llms,
    .called_from = "send_batch" 
  )
  
  # Evaluate the modified provider call
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
#' @return A tibble with information about the status of batch processing.
check_batch <- function(.llms,
                        .provider = getOption("tidyllm_cbatch_default")) {

  # Validate the inputs
  c(
    ".llms must be a list of LLMMessage objects or a character vector with a Batch ID" = (is.list(.llms) && all(sapply(.llms, inherits, "LLMMessage"))) | is.character(.llms)
  ) |> validate_inputs()
  
  
  # Extract batch_id
  if((is.list(.llms) && all(sapply(.llms, inherits, "LLMMessage")))){
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
  
  # Modify the provider call by injecting .llm and .called_from
  modified_call <- rlang::call_modify(
    provider_expr ,
    .batch_id = batch_id,
    .called_from = "check_batch" 
  )
  
  # Evaluate the modified provider call
  return(rlang::eval_tidy(modified_call))
}

#' List all Batch Requests on a Batch API
#'
#' @param .provider A function or function call specifying the language model provider and any additional parameters.
#'   This should be a call to a provider function like `openai()`, `claude()`, etc. 
#'   You can also set a default provider function via the `tidyllm_lbatch_default` option.
#' @return A tibble with information about the status of batch processing.
list_batches <- function(.provider = getOption("tidyllm_lbatch_default")) {
  

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
#' @return A list of updated `LLMMessage` objects, each with the assistant's response added if successful.
fetch_batch <- function(.llms, .provider = getOption("tidyllm_fbatch_default")) {
  
  # Validate the inputs
  c(
    ".llms must be a list of LLMMessage objects with names as custom IDs" = is.list(.llms) && all(sapply(.llms, inherits, "LLMMessage"))
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
  
  # Modify the provider call by injecting .llm and .called_from
  modified_call <- rlang::call_modify(
    provider_expr ,
    .llms = .llms,
    .called_from = "fetch_batch" 
  )
  
  # Evaluate the modified provider call
  return(rlang::eval_tidy(modified_call))
}















MEDIA_TYPES <- c("image", "pdf", "audio", "video", "files")
EMBED_MEDIA_TYPES <- c("image", "audio", "video")
DECIDE_MEDIA_TYPES <- c("image", "audio", "video", "pdf")

#' The message a provider gets when it cannot do a verb at all
#' @noRd
unsupported_verb_message <- function(provider_name, verb_label) {
  base <- glue::glue("`{verb_label}()` is not available for the `{provider_name}()` provider.")
  hint <- if (identical(provider_name, "ellmer")) {
    paste(
      "`chat_ellmer()` hands the conversation to an ellmer `Chat` object instead of",
      "building an HTTP request, so tidyllm has nothing to send or stream on its own.",
      "Use ellmer's own async interface for that, or any other tidyllm provider."
    )
  } else {
    "Use `chat()` with this provider, or pick a provider that supports this verb."
  }
  paste(base, hint)
}

dispatch_to_provider <- function(provider_expr, verb_name, common_args,
                                  validate = TRUE, env = rlang::caller_env(),
                                  verb_label = verb_name) {
  if (validate) {
    meta      <- rlang::call_modify(provider_expr, .called_from = "metadata") |> rlang::eval_tidy()
    supported <- meta$supported_args[[verb_name]]
    # A provider that registered no function for this verb cannot do it at all.
    # Said plainly here, because the argument-level message below would otherwise
    # list every argument as unsupported and explain nothing.
    if (is.null(supported)) {
      stop(unsupported_verb_message(meta$provider_name, verb_label), call. = FALSE)
    }
    if ("..." %in% supported) {
      # The provider function forwards through `...`, so it accepts any common
      # argument; the wrapped function validates them.
      valid_args <- common_args
    } else {
      unsupported <- setdiff(names(common_args), supported)
      if (length(unsupported) > 0) {
        stop(glue::glue(
          "The following arguments are not supported by the provider's `{verb_name}()` function: {paste(unsupported, collapse = ', ')}."
        ))
      }
      valid_args <- common_args[names(common_args) %in% supported]
    }
  } else {
    valid_args <- common_args
  }
  valid_args$.called_from <- verb_name
  rlang::eval_tidy(rlang::call_modify(provider_expr, !!!valid_args), env = env)
}

#' Create a Provider Function for Routing LLM Actions
#'
#' This function generates a provider-specific function that dynamically routes
#' different LLM-related actions (e.g., chat, generate embeddings, batch operations)
#' based on the `.called_from` argument or the presence of an `LLMMessage` object.
#'
#' @param .name A string representing the name of the provider (e.g., "openai").
#' @param ... Named functions corresponding to the various actions the provider supports
#'   (e.g., `chat`, `embed`, `send_batch`).
#' @param .media Character vector of the media types the provider accepts in a message:
#'   any of `"image"`, `"pdf"`, `"audio"`, `"video"` and `"files"` (remote file
#'   references). Read by the attachment check and by `provider_capabilities()`.
#' @param .embed_media Character vector of the media types the provider embeds:
#'   any of `"image"`, `"audio"` and `"video"`. Read by `embed()` and by
#'   `provider_capabilities()`.
#' @param .decide_media Character vector of the media types the provider reads in a
#'   `decide()` state: any of `"image"`, `"audio"`, `"video"` and `"pdf"`. Read by
#'   `decide()` and by `provider_capabilities()`.
#'
#' @return A function that dynamically routes to the appropriate action based on its
#'   inputs.
#' @noRd
create_provider_function <- function(.name, ..., .media = character(), .embed_media = character(),
                                     .decide_media = character()) {
  function_map <- list(...)
  function_names <- vapply(
    as.list(substitute(list(...)))[-1],
    function(e) paste(deparse(e), collapse = ""),
    character(1)
  )
  c(".media must contain only: image, pdf, audio, video, files" =
      all(.media %in% MEDIA_TYPES),
    ".embed_media must contain only: image, audio, video" =
      all(.embed_media %in% EMBED_MEDIA_TYPES),
    ".decide_media must contain only: image, audio, video, pdf" =
      all(.decide_media %in% DECIDE_MEDIA_TYPES)) |> validate_inputs()
  
  if ("metadata" %in% names(function_map)) {
    stop(glue::glue("'metadata' is a reserved keyword and cannot be used as a function name in provider '{.name}'."))
  }
  
  # Capture the arguments used in the provider function
  supported_args <- purrr::map(function_map, formals) |> 
    purrr::map(names)
  supported_defaults <- purrr::map(function_map, function(f) {
    purrr::map_chr(formals(f), function(d) {
      if (rlang::is_missing(d)) NA_character_ else paste(deparse(d), collapse = " ")
    })
  })
  
  # Create the provider function
  provider_function <- function(..., .called_from = NULL) {
    args <- list(...)
    
    if (!is.null(.called_from)) {
      if (.called_from == "metadata") {
        # Return provider metadata
        return(list(
          provider_name = .name,
          supported_args = supported_args,
          supported_defaults = supported_defaults,
          functions = function_names,
          media = .media,
          embed_media = .embed_media,
          decide_media = .decide_media
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
  
  attr(provider_function, "tidyllm_provider") <- .name
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
#' @param .input  A character vector of texts, a list mixing texts with `img()`, `audio_file()`
#'   and `video_file()` objects, or an `LLMMessage` object. Each element of a list is embedded
#'   as its own row. A nested list, such as `list(list("A cat", img("cat.jpg")))`, is one
#'   fused item that gets a single vector. `provider_capabilities(.what = "embed_media")` shows
#'   which provider embeds which media type.
#' @param .provider A function or function call specifying the language model provider and any additional parameters.
#'   This should be a call to a provider function like `openai()`, `ollama()`, etc. 
#'   You can also set a default provider function via the `tidyllm_embed_default` option.
#' @param .model The embedding model to use
#' @param .truncate Whether to truncate inputs to fit the model's context length
#' @param .timeout Timeout for the API request in seconds
#' @param .dry_run If TRUE, perform a dry run and return the request object.
#' @param .max_tries Maximum retry attempts for requests
#' @param .task `"query"` or `"document"`. Adds the task prefix that EmbeddingGemma 2 and
#'   `gemini-embedding-2` read from the text (and sets `input_type` for `voyage()`).
#'   Media get no prefix. Leave `NULL` for models that take no prefix.
#' @param .dimensions Number of dimensions to return, for providers and models that
#'   can shorten their vectors: `gemini()`, `openrouter()`, `ollama()`, `openai()` (`text-embedding-3` models) and `mistral()` (`codestral-embed`).
#'   `voyage()` calls this argument `.output_dimension`. `llamacpp()` has no such argument, because llama-server ignores it.
#' @param .media_columns If `TRUE`, add a `modality` and a `path` column after `input`.
#'   `modality` is `"text"`, `"image"`, `"audio"` or `"video"`; a fused item lists its parts
#'   joined by `" + "`, such as `"text + image"`. `path` holds the file path of the media
#'   in the item (joined by `" + "` for several files) and is `NA` for text. Has no effect with
#'   `.dry_run = TRUE`, which returns the request. Defaults to `FALSE`.
#' @return A tibble with the columns `input` and `embeddings`, plus `modality` and `path`
#' when `.media_columns = TRUE`.
#' The `input` column contains the texts sent to embed, or a label such as
#' `[IMG] cat.jpg` for a media file. A fused item shows its parts joined by `" + "`, for
#' example `A cat + [IMG] cat.jpg`. The `embeddings` column
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
                  .max_tries=NULL,
                  .task=NULL,
                  .dimensions=NULL,
                  .media_columns=FALSE) {

  c(
    "You need to specify a .provider function in embed()" = !is.null(.provider),
    "Input .media_columns must be TRUE or FALSE" = isTRUE(.media_columns) || isFALSE(.media_columns)
  ) |> validate_inputs()
  items <- embed_items(.input)
  
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
    .dry_run = .dry_run,
    .task = .task,
    .dimensions = .dimensions
  )
  common_args <- common_args[!sapply(common_args, is.null)]

  provider_meta <- rlang::call_modify(provider_expr, .called_from = "metadata") |> rlang::eval_tidy()
  check_embed_media(items, provider_meta)
  if (embed_is_text_only(items) && !S7_inherits(.input, LLMMessage)) {
    common_args$.input <- embed_labels(items)
  }

  result <- dispatch_to_provider(provider_expr, "embed", common_args)
  if (.media_columns && is.data.frame(result)) {
    result <- embed_media_columns(result, items)
  }
  return(result)
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
#' Currently supported: OpenAI (deep research models via the background API).
#'
#' @param .llm An `LLMMessage` object containing the research question.
#' @param .provider A function or function call specifying the provider (e.g., `openai()`).
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


#' Stop on a research job that does not name a supported provider
#'
#' @param .job A `tidyllm_research_job`.
#' @param .caller Name of the calling verb, for the message.
#' @return `NULL`, invisibly; called for the error.
#' @noRd
stop_unless_openai_job <- function(.job, .caller) {
  if (!identical(.job$provider, "openai")) {
    stop(.caller, " needs a research job that records its provider (\"openai\"). ",
         "Jobs from the removed perplexity() provider are no longer supported; ",
         "start the research again with deep_research(.provider = openai()).",
         call. = FALSE)
  }
  invisible(NULL)
}

#' Check the Status of a Job
#'
#' `check_job()` reports on anything tidyllm dispatched and did not wait for: a
#' batch from `send_batch()`, a background research job from
#' `deep_research(.background = TRUE)`, or a chat from `send_chat()`.
#'
#' What it costs differs by job, and knowingly so: on a batch it is an HTTP call
#' to the provider, on a chat it is a turn of this session's event loop.
#'
#' @param .job A `tidyllm_chat_job`, a `tidyllm_research_job`, or a batch object
#'   from `send_batch()`.
#' @param ... Additional arguments passed to the underlying function.
#' @return Status information; type depends on `.job`.
#' @export
check_job <- function(.job, ...) UseMethod("check_job")

#' @export
check_job.default <- function(.job, ...) {
  # Batch objects are a list of `LLMMessage`s carrying a `batch_id` attribute
  # rather than a class of their own, so they are recognised here rather than by
  # dispatch. Giving them a class is worth doing, but it touches all six batch
  # providers and both ends of the round trip, so it is not folded into this.
  if (!is.null(attr(.job, "batch_id"))) return(check_batch(.job, ...))

  stop("check_job() expects a job from send_chat(), send_batch() or deep_research(.background = TRUE).",
       call. = FALSE)
}

#' @export
check_job.tidyllm_research_job <- function(.job, ...) {
  stop_unless_openai_job(.job, "check_job()")
  openai_check_research(.job, ...)
}


#' Fetch the Results of a Job
#'
#' `fetch_job()` collects what [check_job()] reports on: the messages of a
#' finished batch, the report of a background research job, or the `LLMMessage`
#' of a chat from `send_chat()`. On a chat it waits for the reply if it has not
#' arrived yet.
#'
#' @param .job A `tidyllm_chat_job`, a `tidyllm_research_job`, or a batch object
#'   from `send_batch()`.
#' @param .provider A provider function; required for batch jobs and ignored by
#'   the others.
#' @param ... Additional arguments passed to the underlying function.
#' @return Fetched results; type depends on `.job`.
#' @export
fetch_job <- function(.job, .provider = NULL, ...) UseMethod("fetch_job")

#' @export
fetch_job.default <- function(.job, .provider = NULL, ...) {
  if (!is.null(attr(.job, "batch_id"))) return(fetch_batch(.job, .provider, ...))

  stop("fetch_job() expects a job from send_chat(), send_batch() or deep_research(.background = TRUE).",
       call. = FALSE)
}

#' @export
fetch_job.tidyllm_research_job <- function(.job, .provider = NULL, ...) {
  stop_unless_openai_job(.job, "fetch_job()")
  openai_fetch_research(.job, ...)
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
validate_message_attachments <- function(.llm, provider) {
  meta <- provider_metadata(provider)
  provider_name <- meta$provider_name
  media <- meta$media
  history <- .llm@message_history
  user_msgs <- which(vapply(history, function(m) identical(m$role, "user"), logical(1)))
  if (length(user_msgs) == 0) return(invisible(NULL))

  last_user_idx <- user_msgs[length(user_msgs)]
  last_msg <- history[[last_user_idx]]

  # Validate remote file references
  if (!is.null(last_msg$files) && length(last_msg$files) > 0) {
    if (!"files" %in% media) {
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
  if (!is.null(last_msg$media) && length(last_msg$media) > 0) {
    for (m in last_msg$media) {
      if (S7_inherits(m, tidyllm_audio) && !"audio" %in% media) {
        stop(glue::glue(
          "The '{provider_name}' provider does not support inline audio.\n",
          "Use gemini() or openrouter() for audio transcription."
        ))
      }
      if (S7_inherits(m, tidyllm_image) && !"image" %in% media) {
        stop(glue::glue(
          "The '{provider_name}' provider does not support images.\n",
          "Use a provider that lists \"image\" in provider_capabilities(.what = \"media\")."
        ))
      }
      if (S7_inherits(m, tidyllm_video) && !"video" %in% media) {
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















#' Answer questions about a text, an image or a recording with a decision model
#'
#' `decide()` sends a state (a text, a picture, a recording, a PDF or a mix) and a
#' set of closed questions to a decision model and returns, for each question, the
#' answer and the probability the model gives it. Use it to classify, screen or
#' score texts and images where you want a calibrated number rather than free
#' text. Give it many states at once and it runs the requests in parallel.
#'
#' Questions are the fields of a [tidyllm_schema()]. `field_lgl()` is a yes/no
#' question, `field_fct(.levels = )` is a choice question, and
#' `field_fct(.levels = , .ordered = TRUE)` is a score question over ordered
#' levels. The field description is the question text. `.level_descriptions` in
#' `field_fct()` tells the model what each option or level means; for a score
#' question the level order is the scale. `field_lgl(.true = , .false = )` defines
#' what yes and no mean; give both or neither.
#'
#' @section Media:
#' A state can hold images, audio, video and PDFs next to text. A media object
#' ([img()], [audio_file()], [video_file()], [pdf_file()]) is an item, and so is
#' an `LLMMessage` (the text of its last user message, then its media). To put
#' several parts into one item, wrap them in a list inside `.state`:
#' `list(list(photo_a = img("a.jpg"), photo_b = img("b.jpg")))`. Names inside the
#' item label the parts, so a question can say "the animal in photo_a". The
#' providers read:
#'
#' \tabular{lll}{
#'   Provider \tab Reads \tab Image option\cr
#'   `jev()` \tab image \tab none\cr
#'   `openai()` \tab image \tab `.detail`\cr
#'   `gemini()` \tab image, audio, video, PDF \tab `.resolution`\cr
#'   `ollama()` \tab image (not yet tested against a live server) \tab none
#' }
#'
#' `provider_capabilities(.what = "decide_media")` gives the same table as a
#' tibble. A provider that cannot read a media type stops with a message before
#' any request; `decide()` also checks image size, format and count against the
#' provider's limits and never resizes an image. A PDF sent to a provider that
#' does not read PDFs is replaced by its extracted text; a PDF made with
#' `pdf_file(.text_extract = TRUE)` always goes as text.
#'
#' Images, audio and video leave your machine and go to the provider. Do not send
#' restricted or personal data, such as survey photos with faces.
#'
#' @section Many states:
#' Every element of `.state` is one item. An item is a string, a media object, an
#' `LLMMessage`, or a list of parts that belong together (strings and media
#' objects, one level only; names label the parts). `.state` itself is one item
#' passed bare, a character vector (one item per string), or a list of items (one
#' item per element). So `list(img("a.jpg"), img("b.jpg"))` is two items, and
#' `list(list("Caption", img("c.jpg")))` is one item with two parts.
#'
#' A single item, however it is given, returns the plain tibble. Two or more items
#' give one result tibble with a leading `state` column (the item's name when the
#' outer list is named, else its text cut to 60 characters or a short description
#' of its media) and an integer `state_id`, then the usual columns.
#'
#' The requests run in parallel, at most `.max_active` at a time, and are retried
#' like a single request. A state that fails after its retries, or that the model
#' refuses, gives NA rows and one warning; the other states are kept. A malformed
#' item stops the call before any request and names its position.
#'
#' @param .state What to decide about. Every element is one item: a non-empty
#'   string, a media object, an `LLMMessage`, or a list of parts that belong
#'   together (one level only). Pass one item bare, a character vector, or a list
#'   of items; two or more items are decided in parallel. See the Many states
#'   section.
#' @param .questions A [tidyllm_schema()] whose fields are the questions.
#' @param .provider A provider call such as `jev()`.
#' @param .dry_run If `TRUE`, return the httr2 request without sending it (a list
#'   of requests, one per state, for many states).
#' @param .timeout Timeout in seconds. Provider default if `NULL`.
#' @param .max_tries Maximum attempts. Provider default if `NULL`.
#' @param .max_active Most requests in flight at once when `.state` holds many
#'   states. The default is 5; a free Gemini key allows about 200 requests a
#'   minute, so lower it if you see rate-limit warnings.
#' @param .progress Show a progress bar when `.state` holds many states.
#'
#' @return A tibble with one row per question and the columns `question`, `type`
#'   (`"yes_no"`, `"choice"` or `"score"`), `answer` (`"yes"` or `"no"`, the chosen
#'   option, or the score level with the highest probability), `probability` (of
#'   that answer), `score` (the expected level position as the backend reports
#'   it, counted from 0; `NA` for other types), `confidence` (the backend's
#'   confidence in the answer; for yes/no questions the larger of the probability
#'   of yes and of no when the backend gives none) and `probabilities` (a list
#'   column with one named numeric vector over the options for each row). For many
#'   states, one block of rows per state with `state` and `state_id` in front.
#'
#'   The attributes `usage`, `latency` and `model` describe the call. `usage` is a
#'   list with `input_tokens` and `output_tokens` (what the provider billed; `NA`
#'   when it does not report them), plus `cached_tokens` and `cache_write_tokens` for OpenAI and
#'   `tokens_by_modality` for Gemini. `latency` is the elapsed seconds. For many
#'   states, `usage` sums the states that succeeded, `latency` is the time of the
#'   whole parallel run, and `model` is the model the first reply names. With
#'   `.dry_run = TRUE`, the request.
#'
#' @examples
#' \dontrun{
#' questions <- tidyllm_schema(
#'   late = field_lgl("Was the payment late?"),
#'   tone = field_fct("Tone of the message", .levels = c("polite", "angry")),
#'   severity = field_fct("How severe is the complaint?",
#'     .levels = c("low", "medium", "high"), .ordered = TRUE)
#' )
#' decide("The invoice arrived three weeks late and nobody answers my mails.",
#'        questions, jev())
#'
#' # Many texts: one tibble with a state and state_id column
#' mails <- c("Payment is three weeks late.", "Thanks, all fine.")
#' decide(mails, questions, openai())
#'
#' # An image, and two labelled images in one item
#' cat_q <- tidyllm_schema(cat = field_lgl("Is the animal in the picture a cat?"))
#' decide(img("cat.jpg"), cat_q, openai(.detail = "low"))
#' pets <- tidyllm_schema(
#'   a_dog = field_lgl("Is the animal in photo_a a dog?"),
#'   b_dog = field_lgl("Is the animal in photo_b a dog?")
#' )
#' decide(list(list(photo_a = img("cat.jpg"), photo_b = img("dog.jpg"))), pets, jev())
#'
#' # Many images: one item per element
#' files <- c("cat.jpg", "dog.jpg")
#' decide(purrr::map(files, img), cat_q, gemini(.resolution = "low"))
#' }
#' @export
decide <- function(.state,
                   .questions,
                   .provider = getOption("tidyllm_decide_default"),
                   .dry_run = NULL,
                   .timeout = NULL,
                   .max_tries = NULL,
                   .max_active = 5,
                   .progress = FALSE) {
  c("You need to specify a .provider function in decide()" = !is.null(.provider),
    ".max_active must be a whole number of at least 1" =
      is.numeric(.max_active) && length(.max_active) == 1 && !is.na(.max_active) &&
        .max_active >= 1 && .max_active == round(.max_active),
    ".progress must be TRUE or FALSE" = isTRUE(.progress) || isFALSE(.progress)) |>
    validate_inputs()
  states <- decide_states(.state)
  questions <- decide_questions(.questions)

  if (rlang::is_function(.provider)) {
    .provider <- .provider()
  }
  if (!rlang::is_call(.provider)) {
    provider_expr <- rlang::quo_get_expr(rlang::enquo(.provider))
  } else {
    provider_expr <- .provider
  }
  meta <- provider_metadata(provider_expr)
  if (is.null(meta$supported_args$decide)) {
    msg <- unsupported_verb_message(meta$provider_name, "decide")
    cli::cli_abort("{msg}", call = NULL)
  }
  states <- purrr::imap(states, \(s, i) at_state(i, length(states), \() decide_checked(s, questions, meta)))

  common_args <- list(
    .state = if (length(states) == 1) states[[1]] else decide_many(states, .max_active, .progress),
    .questions = questions,
    .dry_run = .dry_run,
    .timeout = .timeout,
    .max_tries = .max_tries
  )
  common_args <- common_args[!purrr::map_lgl(common_args, is.null)]
  dispatch_to_provider(provider_expr, "decide", common_args)
}

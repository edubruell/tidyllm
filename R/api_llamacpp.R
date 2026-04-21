
#' The llama.cpp API provider class (inherits from OpenAI)
#'
#' @noRd
api_llamacpp <- new_class("LlamaCpp", api_chat_completions)


#' Extract metadata from llama.cpp chat responses
#'
#' Extends the OpenAI metadata extractor to also capture logprobs and
#' reasoning_content (thinking traces from Qwen3 and similar models).
#'
#' @noRd
method(extract_metadata, list(api_llamacpp, class_list)) <- function(.api, .response) {
  list(
    model             = .response$model,
    timestamp         = lubridate::as_datetime(.response$created),
    prompt_tokens     = .response$usage$prompt_tokens,
    completion_tokens = .response$usage$completion_tokens,
    total_tokens      = .response$usage$total_tokens,
    stream            = FALSE,
    specific_metadata = list(
      system_fingerprint = .response$system_fingerprint,
      logprobs           = .response$choices[[1]]$logprobs,
      thinking           = .response$choices[[1]]$message$reasoning_content
    )
  )
}


#' Send LLM Messages to a llama.cpp Server
#'
#' @description
#' Sends a message history to a local llama.cpp server using the OpenAI-compatible
#' Chat Completions API. Supports BNF grammar constraints (a llama.cpp-specific
#' feature that enforces output format at the token sampling level) and token
#' logprobs for uncertainty quantification.
#'
#' @param .llm An `LLMMessage` object containing the conversation history.
#' @param .model The model name (default: `"local-model"`). llama.cpp ignores this
#'   value and serves whatever GGUF is loaded; the default is a clear placeholder.
#' @param .max_tokens Maximum tokens in the response (default: 1024).
#' @param .temperature Controls randomness (0–2, optional).
#' @param .top_p Nucleus sampling parameter (0–1, optional).
#' @param .stop One or more stop sequences (optional).
#' @param .stream Logical; if TRUE, streams the response (default: FALSE).
#' @param .tools Either a single TOOL object or a list of TOOL objects.
#' @param .tool_choice Tool-calling behavior: `"none"`, `"auto"`, or `"required"` (optional).
#' @param .json_schema A JSON schema as an R list to enforce structured output. Passed
#'   as `response_format = {type: "json_schema", ...}` — enforced at the sampler level.
#' @param .grammar A BNF grammar string to constrain sampling to any formal language
#'   (e.g. `"root ::= [0-9]{4}"` for a 4-digit year). Takes precedence over
#'   `.json_schema` when both are set; use one or the other.
#' @param .logprobs Logical; if TRUE, returns token log-probabilities (default: FALSE).
#' @param .top_logprobs Integer; number of top-alternative log-prob entries to return
#'   per token (1–20, optional). Requires `.logprobs = TRUE`.
#' @param .seed Integer; random seed for reproducible outputs (optional).
#' @param .thinking Logical; if TRUE, enables extended reasoning/thinking mode for models
#'   that support it (e.g. Qwen3). `NULL` (default) lets the server decide; `FALSE`
#'   explicitly disables it for faster responses; `TRUE` forces it on. The thinking
#'   trace is accessible via `get_metadata(result)$api_specific$thinking`.
#' @param .thinking_budget Integer; maximum tokens the model may use for thinking when
#'   `.thinking = TRUE`. `NULL` uses the server default (optional).
#' @param .server Base URL of the llama.cpp server. Defaults to the `LLAMACPP_SERVER`
#'   environment variable, falling back to `"http://localhost:8080"`.
#' @param .api_key API key for the llama.cpp server. Defaults to the `LLAMACPP_API_KEY`
#'   environment variable. Leave unset if the server was started without `--api-key`.
#' @param .timeout Request timeout in seconds (default: 120).
#' @param .verbose If TRUE, displays additional information (default: FALSE).
#' @param .dry_run If TRUE, returns the request object without executing it (default: FALSE).
#' @param .max_tries Maximum retries (default: 3).
#' @param .max_tool_rounds Maximum tool use iterations (default: 10).
#'
#' @return A new `LLMMessage` object containing the original messages plus the
#'   assistant's response.
#'
#' @export
llamacpp_chat <- function(.llm,
                          .model          = "local-model",
                          .max_tokens     = 1024,
                          .temperature    = NULL,
                          .top_p          = NULL,
                          .stop           = NULL,
                          .stream         = FALSE,
                          .tools          = NULL,
                          .tool_choice    = NULL,
                          .json_schema    = NULL,
                          .grammar        = NULL,
                          .logprobs        = FALSE,
                          .top_logprobs    = NULL,
                          .seed            = NULL,
                          .thinking        = NULL,
                          .thinking_budget = NULL,
                          .server          = Sys.getenv("LLAMACPP_SERVER", "http://localhost:8080"),
                          .api_key        = Sys.getenv("LLAMACPP_API_KEY", ""),
                          .timeout        = 120,
                          .verbose        = FALSE,
                          .dry_run        = FALSE,
                          .max_tries      = 3,
                          .max_tool_rounds = 10) {

  c(
    "Input .llm must be an LLMMessage object" = S7_inherits(.llm, LLMMessage),
    "Input .model must be a non-empty string" = is.character(.model) && nzchar(.model),
    "Input .max_tokens must be a positive integer" = is_integer_valued(.max_tokens) && .max_tokens > 0,
    "Input .temperature must be between 0 and 2 if provided" = is.null(.temperature) || (is.numeric(.temperature) && .temperature >= 0 && .temperature <= 2),
    "Input .top_p must be between 0 and 1 if provided" = is.null(.top_p) || (is.numeric(.top_p) && .top_p >= 0 && .top_p <= 1),
    "Input .stop must be NULL, a string, or a list" = is.null(.stop) || is.character(.stop) || is.list(.stop),
    "Input .stream must be logical" = is.logical(.stream),
    "Input .tools must be NULL, a TOOL object, or a list of TOOL objects" = is.null(.tools) || S7_inherits(.tools, TOOL) || (is.list(.tools) && all(purrr::map_lgl(.tools, ~ S7_inherits(.x, TOOL)))),
    "Input .tool_choice must be NULL or one of 'none', 'auto', 'required'" = is.null(.tool_choice) || (.tool_choice %in% c("none", "auto", "required")),
    "Streaming is not supported for requests with tool calls" = is.null(.tools) || !isTRUE(.stream),
    "Input .json_schema must be NULL or a list" = is.null(.json_schema) || is.list(.json_schema),
    "Input .grammar must be NULL or a non-empty string" = is.null(.grammar) || (is.character(.grammar) && nzchar(.grammar)),
    "Input .logprobs must be logical" = is.logical(.logprobs),
    "Input .top_logprobs must be NULL or an integer between 1 and 20" = is.null(.top_logprobs) || (is_integer_valued(.top_logprobs) && .top_logprobs >= 1 && .top_logprobs <= 20),
    "Input .seed must be an integer-valued numeric if provided" = is.null(.seed) || is_integer_valued(.seed),
    "Input .thinking must be NULL or logical" = is.null(.thinking) || is.logical(.thinking),
    "Input .thinking_budget must be NULL or a positive integer" = is.null(.thinking_budget) || (is_integer_valued(.thinking_budget) && .thinking_budget > 0),
    "Input .server must be a non-empty string" = is.character(.server) && nzchar(.server),
    "Input .timeout must be a positive integer" = is_integer_valued(.timeout) && .timeout > 0,
    "Input .verbose must be logical" = is.logical(.verbose),
    "Input .dry_run must be logical" = is.logical(.dry_run),
    "Input .max_tries must be integer-valued numeric" = is_integer_valued(.max_tries),
    ".max_tool_rounds must be a positive integer" = is_integer_valued(.max_tool_rounds) && .max_tool_rounds >= 1
  ) |> validate_inputs()

  api_obj <- api_llamacpp(short_name = "llamacpp",
                          long_name  = "llama.cpp",
                          api_key_env_var = "LLAMACPP_API_KEY")

  messages <- to_api_format(.llm, api_obj, FALSE)

  tools_def <- if (!is.null(.tools)) {
    if (S7_inherits(.tools, TOOL)) list(.tools) else .tools
  } else {
    NULL
  }

  json <- FALSE
  response_format <- NULL

  if (!is.null(.json_schema)) {
    json <- TRUE
    response_format <- list(
      type = "json_schema",
      json_schema = list(name = "output", schema = .json_schema, strict = TRUE)
    )
  }

  request_body <- list(
    model            = .model,
    messages         = messages,
    max_tokens       = .max_tokens,
    temperature      = .temperature,
    top_p            = .top_p,
    stop             = .stop,
    stream           = .stream,
    seed             = .seed,
    response_format  = response_format,
    grammar          = .grammar,
    logprobs         = if (.logprobs) TRUE else NULL,
    top_logprobs     = .top_logprobs,
    enable_thinking  = .thinking,
    thinking_budget  = if (!is.null(.thinking_budget) && isTRUE(.thinking)) .thinking_budget else NULL,
    tools            = if (!is.null(tools_def)) tools_to_api(api_obj, tools_def) else NULL,
    tool_choice      = .tool_choice
  ) |> purrr::compact()

  auth_header <- if (nzchar(.api_key)) {
    list(Authorization = sprintf("Bearer %s", .api_key))
  } else {
    list()
  }

  request <- httr2::request(.server) |>
    httr2::req_url_path("/v1/chat/completions") |>
    httr2::req_headers(!!!auth_header, `Content-Type` = "application/json") |>
    httr2::req_body_json(data = request_body)

  if (.dry_run) {
    return(request)
  }

  response <- perform_chat_request(request, api_obj, .stream, .timeout, .max_tries)

  if (.stream == FALSE && !is.null(tools_def)) {
    response <- process_tool_loop(
      .api             = api_obj,
      .response        = response,
      .tools_def       = tools_def,
      .request_body    = request_body,
      .request         = request,
      .timeout         = .timeout,
      .max_tries       = .max_tries,
      .max_tool_rounds = .max_tool_rounds
    )
  }

  logprobs <- parse_logprobs(api_obj, response$raw)

  add_message(.llm      = .llm,
              .role     = "assistant",
              .content  = response$assistant_reply,
              .json     = json,
              .meta     = response$meta,
              .logprobs = logprobs)
}


#' Generate Embeddings Using a llama.cpp Server
#'
#' @description
#' Sends text to the `/v1/embeddings` endpoint of a running llama.cpp server
#' and returns embedding vectors.
#'
#' @param .input A character vector of texts to embed, or an `LLMMessage` object.
#' @param .model The model name (default: `"local-model"`). llama.cpp ignores this
#'   and serves the loaded embedding model.
#' @param .server Base URL of the llama.cpp server. Defaults to the `LLAMACPP_SERVER`
#'   environment variable, falling back to `"http://localhost:8080"`.
#' @param .api_key API key for the server (default: `LLAMACPP_API_KEY` env var).
#' @param .timeout Request timeout in seconds (default: 120).
#' @param .dry_run If TRUE, returns the request object without executing it (default: FALSE).
#' @param .max_tries Maximum retries (default: 3).
#'
#' @return A tibble with columns `input` (text) and `embeddings` (list of numeric vectors).
#'
#' @export
llamacpp_embedding <- function(.input,
                               .model   = "local-model",
                               .server  = Sys.getenv("LLAMACPP_SERVER", "http://localhost:8080"),
                               .api_key = Sys.getenv("LLAMACPP_API_KEY", ""),
                               .timeout = 120,
                               .dry_run = FALSE,
                               .max_tries = 3) {
  c(
    "Input .input must be an LLMMessage object or a character vector" = S7_inherits(.input, LLMMessage) || is.character(.input),
    "Input .model must be a non-empty string" = is.character(.model) && nzchar(.model),
    "Input .server must be a non-empty string" = is.character(.server) && nzchar(.server),
    "Input .timeout must be a positive number" = is.numeric(.timeout) && .timeout > 0,
    "Input .dry_run must be logical" = is.logical(.dry_run)
  ) |> validate_inputs()

  input_texts <- parse_embedding_input(.input)

  auth_header <- if (nzchar(.api_key)) {
    list(Authorization = sprintf("Bearer %s", .api_key))
  } else {
    list()
  }

  request <- httr2::request(.server) |>
    httr2::req_url_path("/v1/embeddings") |>
    httr2::req_headers(!!!auth_header, `Content-Type` = "application/json") |>
    httr2::req_body_json(list(model = .model, input = input_texts))

  if (.dry_run) {
    return(request)
  }

  extract_embeddings_fn <- function(response_content, error, response_headers) {
    if (error) {
      paste0("llama.cpp embedding error: ", response_content$error$message %||% "unknown") |> stop()
    }
    response_content$data |>
      purrr::map("embedding") |>
      purrr::map(unlist)
  }

  perform_embedding_request(
    .request               = request,
    .timeout               = .timeout,
    .max_tries             = .max_tries,
    .input_texts           = input_texts,
    .fn_extract_embeddings = extract_embeddings_fn
  )
}


#' List Models Loaded in the llama.cpp Server
#'
#' @description
#' Calls the `/v1/models` endpoint of a running llama.cpp server and returns
#' the currently loaded model(s) as a tibble. In normal operation this is one
#' row; two rows appear when speculative decoding is active (main + draft model).
#'
#' @param .server Base URL of the llama.cpp server. Defaults to `LLAMACPP_SERVER`
#'   env var, falling back to `"http://localhost:8080"`.
#' @param .api_key API key for the server (default: `LLAMACPP_API_KEY` env var).
#' @param .timeout Request timeout in seconds (default: 30).
#' @param .max_tries Maximum retries (default: 3).
#'
#' @return A tibble with columns `id`, `object`, and `created`.
#'
#' @export
llamacpp_list_models <- function(.server  = Sys.getenv("LLAMACPP_SERVER", "http://localhost:8080"),
                                 .api_key = Sys.getenv("LLAMACPP_API_KEY", ""),
                                 .timeout = 30,
                                 .max_tries = 3) {
  auth_header <- if (nzchar(.api_key)) {
    list(Authorization = sprintf("Bearer %s", .api_key))
  } else {
    list()
  }

  response <- httr2::request(.server) |>
    httr2::req_url_path("/v1/models") |>
    httr2::req_headers(!!!auth_header) |>
    httr2::req_timeout(.timeout) |>
    httr2::req_retry(max_tries = .max_tries) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (is.null(response$data) || length(response$data) == 0) {
    return(tibble::tibble(id = character(), object = character(), created = integer()))
  }

  tibble::tibble(
    id      = vapply(response$data, function(m) m$id %||% NA_character_, character(1)),
    object  = vapply(response$data, function(m) m$object %||% NA_character_, character(1)),
    created = vapply(response$data, function(m) {
      v <- m$created; if (is.null(v)) NA_integer_ else as.integer(v)
    }, integer(1))
  )
}


#' Check Health of the llama.cpp Server
#'
#' @description
#' Calls the `/health` endpoint of a running llama.cpp server.
#' Returns the status string (`"ok"`, `"loading model"`, `"no model loaded"`,
#' or `"error"`) along with the full parsed response body as a named list.
#'
#' @param .server Base URL of the llama.cpp server. Defaults to `LLAMACPP_SERVER`
#'   env var, falling back to `"http://localhost:8080"`.
#' @param .timeout Request timeout in seconds (default: 10).
#'
#' @return A named list with at least a `status` element.
#'
#' @export
llamacpp_health <- function(.server  = Sys.getenv("LLAMACPP_SERVER", "http://localhost:8080"),
                            .timeout = 10) {
  resp <- tryCatch(
    httr2::request(.server) |>
      httr2::req_url_path("/health") |>
      httr2::req_timeout(.timeout) |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform() |>
      httr2::resp_body_json(),
    error = function(e) list(status = paste0("connection error: ", e$message))
  )
  resp
}


#' List Local GGUF Model Files
#'
#' @description
#' Scans a directory for `.gguf` files and returns a tibble with their names,
#' sizes, and modification times. No server needed.
#'
#' @param .path Directory to scan (default: `LLAMACPP_MODEL_DIR` env var, then
#'   `"~/models"`). The path is expanded with `path.expand()`.
#'
#' @return A tibble with columns `filename`, `size_gb`, `modified`, and `path`.
#'
#' @export
llamacpp_list_local_models <- function(.path = Sys.getenv("LLAMACPP_MODEL_DIR", "~/models")) {
  .path <- path.expand(.path)

  c(
    "Path must be an existing directory" = dir.exists(.path)
  ) |> validate_inputs()

  files <- list.files(.path, pattern = "\\.gguf$", full.names = TRUE, ignore.case = TRUE)

  if (length(files) == 0) {
    return(tibble::tibble(
      filename = character(),
      size_gb  = numeric(),
      modified = as.POSIXct(character()),
      path     = character()
    ))
  }

  info <- file.info(files)
  tibble::tibble(
    filename = basename(files),
    size_gb  = round(info$size / 1e9, 2),
    modified = info$mtime,
    path     = files
  )
}


#' List GGUF Files Available in a Hugging Face Repository
#'
#' @description
#' Calls the Hugging Face Hub API to list all `.gguf` files in a repository,
#' along with their sizes. Useful for choosing a quantization level before
#' calling `llamacpp_download_model()`.
#'
#' @param .repo Hugging Face repository ID in `"owner/model"` format
#'   (e.g. `"Qwen/Qwen3-8B-GGUF"`).
#' @param .timeout Request timeout in seconds (default: 30).
#'
#' @return A tibble with columns `filename`, `size_gb`, and `url`.
#'
#' @export
list_hf_gguf_files <- function(.repo, .timeout = 30) {
  c(
    "Input .repo must be a non-empty string in 'owner/model' format" = is.character(.repo) && nzchar(.repo) && grepl("/", .repo)
  ) |> validate_inputs()

  response <- httr2::request(glue::glue("https://huggingface.co/api/models/{.repo}/tree/main")) |>
    httr2::req_timeout(.timeout) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  gguf_files <- purrr::keep(response, ~ grepl("\\.gguf$", .x$path %||% "", ignore.case = TRUE))

  if (length(gguf_files) == 0) {
    return(tibble::tibble(filename = character(), size_gb = numeric(), url = character()))
  }

  tibble::tibble(
    filename = vapply(gguf_files, function(f) basename(f$path %||% ""), character(1)),
    size_gb  = vapply(gguf_files, function(f) {
      v <- f$size; if (is.null(v)) NA_real_ else round(as.numeric(v) / 1e9, 2)
    }, numeric(1)),
    url = vapply(gguf_files, function(f) {
      glue::glue("https://huggingface.co/{.repo}/resolve/main/{f$path}")
    }, character(1))
  )
}


#' Download a GGUF Model from Hugging Face
#'
#' @description
#' Downloads a single GGUF file from a Hugging Face repository with a streaming
#' progress bar. Use `list_hf_gguf_files()` first to browse available quantizations.
#'
#' @param .repo Hugging Face repository ID (e.g. `"Qwen/Qwen3-8B-GGUF"`).
#' @param .filename The exact filename to download (e.g. `"Qwen3-8B-Q4_K_M.gguf"`).
#'   Use `list_hf_gguf_files(.repo)` to see what is available.
#' @param .dir Destination directory. Defaults to `LLAMACPP_MODEL_DIR` env var,
#'   then `"~/models"`. Created if it does not exist.
#' @param .timeout Download timeout in seconds (default: 3600).
#'
#' @return Invisibly returns the full path of the downloaded file.
#'
#' @export
llamacpp_download_model <- function(.repo,
                                    .filename,
                                    .dir     = Sys.getenv("LLAMACPP_MODEL_DIR", "~/models"),
                                    .timeout = 3600) {
  c(
    "Input .repo must be a non-empty string in 'owner/model' format" = is.character(.repo) && nzchar(.repo) && grepl("/", .repo),
    "Input .filename must be a non-empty string" = is.character(.filename) && nzchar(.filename)
  ) |> validate_inputs()

  .dir <- path.expand(.dir)
  if (!dir.exists(.dir)) dir.create(.dir, recursive = TRUE)

  dest <- file.path(.dir, .filename)
  url  <- glue::glue("https://huggingface.co/{.repo}/resolve/main/{.filename}")

  progress_bar <- cli::cli_progress_bar(
    format      = "{cli::pb_name} {cli::pb_bar} {cli::pb_percent} | {cli::pb_current_bytes}/{cli::pb_total_bytes}",
    format_done = "{cli::pb_name} {cli::pb_bar} downloaded in {cli::pb_elapsed}",
    total       = NA,
    auto_terminate = FALSE,
    type        = "download"
  )

  con <- file(dest, "wb")
  on.exit({ close(con); cli::cli_progress_done(id = progress_bar) }, add = TRUE)

  httr2::request(url) |>
    httr2::req_timeout(.timeout) |>
    httr2::req_perform_stream(
      callback = function(chunk) {
        writeBin(chunk, con)
        TRUE
      },
      buffer_kb = 512
    )

  invisible(dest)
}


#' Delete a Local GGUF Model File
#'
#' @description
#' Deletes a GGUF file from disk. Issues a warning confirming the deletion.
#' Mirrors the `ollama_delete_model()` pattern.
#'
#' @param .path Full path to the `.gguf` file to delete.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @export
llamacpp_delete_model <- function(.path) {
  .path <- path.expand(.path)
  c(
    "Input .path must point to an existing file" = file.exists(.path)
  ) |> validate_inputs()

  filename <- basename(.path)
  unlink(.path)
  warning(paste0("Model ", filename, " deleted"))
  invisible(TRUE)
}


#' Rerank Documents Using a llama.cpp Server
#'
#' @description
#' Calls the `/v1/reranking` endpoint of a running llama.cpp server to score
#' documents by relevance to a query. Useful for building fully-local RAG
#' pipelines (embed → cosine search → rerank → chat, all with `llamacpp()`).
#'
#' @param .query A single query string.
#' @param .documents A character vector of documents to rerank.
#' @param .model The model name (default: `"local-model"`).
#' @param .server Base URL of the llama.cpp server. Defaults to `LLAMACPP_SERVER`
#'   env var, falling back to `"http://localhost:8080"`.
#' @param .api_key API key for the server (default: `LLAMACPP_API_KEY` env var).
#' @param .timeout Request timeout in seconds (default: 60).
#' @param .max_tries Maximum retries (default: 3).
#' @param .dry_run If TRUE, returns the request object without executing it (default: FALSE).
#'
#' @return A tibble with columns `index` (original position), `document` (text),
#'   and `relevance_score`, sorted by descending score.
#'
#' @export
llamacpp_rerank <- function(.query,
                            .documents,
                            .model   = "local-model",
                            .server  = Sys.getenv("LLAMACPP_SERVER", "http://localhost:8080"),
                            .api_key = Sys.getenv("LLAMACPP_API_KEY", ""),
                            .timeout = 60,
                            .max_tries = 3,
                            .dry_run = FALSE) {
  c(
    "Input .query must be a non-empty string" = is.character(.query) && length(.query) == 1 && nzchar(.query),
    "Input .documents must be a non-empty character vector" = is.character(.documents) && length(.documents) >= 1,
    "Input .dry_run must be logical" = is.logical(.dry_run)
  ) |> validate_inputs()

  auth_header <- if (nzchar(.api_key)) {
    list(Authorization = sprintf("Bearer %s", .api_key))
  } else {
    list()
  }

  request <- httr2::request(.server) |>
    httr2::req_url_path("/v1/reranking") |>
    httr2::req_headers(!!!auth_header, `Content-Type` = "application/json") |>
    httr2::req_body_json(list(
      model     = .model,
      query     = .query,
      documents = as.list(.documents)
    ))

  if (.dry_run) {
    return(request)
  }

  response <- request |>
    httr2::req_timeout(.timeout) |>
    httr2::req_retry(max_tries = .max_tries) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  results <- response$results %||% response$data %||% list()

  if (length(results) == 0) {
    return(tibble::tibble(index = integer(), document = character(), relevance_score = numeric()))
  }

  tbl <- tibble::tibble(
    index           = vapply(results, function(r) as.integer(r$index), integer(1)),
    document        = .documents[vapply(results, function(r) as.integer(r$index) + 1L, integer(1))],
    relevance_score = vapply(results, function(r) {
      v <- r$relevance_score %||% r$score
      if (is.null(v)) NA_real_ else as.numeric(v)
    }, numeric(1))
  )

  tbl[order(-tbl$relevance_score), ]
}


#' llama.cpp Provider Function
#'
#' @description
#' The `llamacpp()` provider connects tidyllm to a locally running
#' [llama.cpp](https://github.com/ggml-org/llama.cpp) server. It exposes
#' the same verb/provider pattern as every other tidyllm provider while
#' also offering llama.cpp-specific features: BNF grammar constraints
#' (`.grammar`), token logprobs (`.logprobs`), and model management helpers.
#'
#' The server must be started separately before calling `llamacpp()`. See
#' `llamacpp_health()` to verify the server is running, and
#' `llamacpp_download_model()` / `list_hf_gguf_files()` to obtain models.
#'
#' @param ... Parameters passed to the appropriate llama.cpp-specific function.
#' @param .called_from An internal argument specifying which verb invoked this
#'   function. Managed automatically by tidyllm verbs; do not set manually.
#'
#' @return The result of the requested action (e.g., an updated `LLMMessage`
#'   for `chat()`, a tibble for `embed()` or `list_models()`).
#'
#' @export
llamacpp <- create_provider_function(
  .name       = "llamacpp",
  chat        = llamacpp_chat,
  embed       = llamacpp_embedding,
  list_models = llamacpp_list_models
)

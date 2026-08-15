# Offline replay of recorded streaming wire bytes.
#
# Serves the fixtures in local_tests/fixtures/streams/ back over a real HTTP
# connection with a local webfakes server, so httr2 opens a genuine streaming
# response and the real `handle_stream()` runs against it. That covers the
# transport, the chunk boundaries and the termination contract, none of which a
# parser-level fake would touch.
#
# webfakes is a local-test-only tool. It is deliberately NOT in DESCRIPTION:
# the CRAN test suite gains no dependency from any of this.
#
# Used by local_tests/features/stream_replay.R and by
# local_tests/record_stream_baseline.R.

STREAM_FIXTURE_DIR <- "local_tests/fixtures/streams"

#' Read every recorded fixture into a named list.
load_stream_fixtures <- function(.dir = STREAM_FIXTURE_DIR) {
  files <- list.files(.dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) {
    stop("No stream fixtures in ", .dir,
         ". Record them first with local_tests/record_stream_fixtures.R")
  }
  out <- lapply(files, readRDS)
  names(out) <- sub("\\.rds$", "", basename(files))
  out
}

#' Start a webfakes process replaying the fixtures.
#'
#' Routes:
#'   GET  /stream/<name>              replay every recorded chunk
#'   GET  /stream/<name>?chunks=<n>   replay the first n chunks, then close
#'   POST /stream/<name>              the same, for the tool loop
#'
#' The truncating route is how abnormal termination gets tested: the connection
#' ends without the provider's terminal event, which is exactly the case four of
#' the six `handle_stream()` loops spin on forever today.
#'
#' POST exists because `process_tool_loop()` re-performs the real request object,
#' which carries a JSON body. The same fixture answers every round, so the loop
#' keeps finding tool calls and runs to its `.max_tool_rounds` ceiling; that is
#' what makes a streamed multi-round conversation testable without a key.
start_stream_replay_server <- function(.fixtures = load_stream_fixtures()) {
  rlang::check_installed("webfakes", reason = "to replay recorded streams offline.")

  # `force()` matters: the app is serialised into a subprocess, and a promise
  # left unevaluated here fails to resolve there.
  force(.fixtures)

  replay <- function(req, res) {
    fx <- .fixtures[[req$params$name]]
    if (is.null(fx)) {
      res$set_status(404L)$send("no such fixture")
      return()
    }

    n <- suppressWarnings(as.integer(req$query$chunks %||% NA))
    chunks <- fx$wire_chunks
    if (!is.na(n)) chunks <- utils::head(chunks, n)

    res$set_header("content-type", fx$content_type)
    for (ch in chunks) res$send_chunk(ch)
  }

  app <- webfakes::new_app()
  app$get(webfakes::new_regexp("^/stream/(?<name>[^/?]+)$"), replay)
  app$post(webfakes::new_regexp("^/stream/(?<name>[^/?]+)$"), replay)

  proc <- webfakes::new_app_process(app)
  structure(
    list(
      process  = proc,
      fixtures = .fixtures,
      url      = function(name, chunks = NULL) {
        path <- paste0("/stream/", name)
        if (!is.null(chunks)) path <- paste0(path, "?chunks=", chunks)
        proc$url(path)
      },
      stop     = function() proc$stop()
    ),
    class = "tidyllm_stream_replay"
  )
}

#' Open a streaming httr2 response against a replayed fixture.
#'
#' `blocking = FALSE` mirrors what `perform_chat_request()` does on the
#' streaming path today, so the replay exercises the same poll-loop behaviour
#' as a live request.
replay_stream_response <- function(.server, .name, .chunks = NULL, .blocking = FALSE) {
  httr2::request(.server$url(.name, .chunks)) |>
    httr2::req_perform_connection(blocking = .blocking)
}

#' The api object each fixture's `handle_stream()` method dispatches on.
stream_fixture_api <- function(.fixture) {
  switch(
    .fixture$provider,
    claude  = api_claude(short_name = "claude", long_name = "Anthropic Claude",
                         api_key_env_var = "ANTHROPIC_API_KEY"),
    openai  = api_openai(short_name = "openai", long_name = "OpenAI",
                         api_key_env_var = "OPENAI_API_KEY"),
    gemini  = api_gemini(short_name = "gemini", long_name = "Google Gemini",
                         api_key_env_var = "GOOGLE_API_KEY"),
    ollama  = api_ollama(short_name = "ollama", long_name = "Ollama",
                              stream_transport = "lines"),
    groq    = api_chat_completions(short_name = "groq", long_name = "Groq",
                                   api_key_env_var = "GROQ_API_KEY"),
    mistral = api_chat_completions(short_name = "mistral", long_name = "Mistral",
                                   api_key_env_var = "MISTRAL_API_KEY"),
    stop("No api object mapping for provider ", .fixture$provider)
  )
}

#' Run one fixture through the current streaming implementation.
#'
#' Returns the reply, the event count and the metadata the package derives from
#' the stream; that triple is what the pre/post-refactor baseline compares.
replay_fixture <- function(.server, .name, .chunks = NULL) {
  fx   <- .server$fixtures[[.name]]
  api  <- stream_fixture_api(fx)
  resp <- replay_stream_response(.server, .name, .chunks)

  out  <- handle_stream(api, resp)

  # Mirrors `perform_chat_request()`: the events become a body, and the reply
  # and metadata are read back out of it. `out$reply` is kept alongside because
  # the two now have to agree, and a disagreement means the assembler lost
  # something the sink had already shown the user.
  body <- assemble_stream_body(api, out$raw_data)
  meta <- tryCatch(extract_metadata(api, body),
                   error = function(e) list(error = conditionMessage(e)))
  reply <- tryCatch(parse_chat_response(api, body),
                    error = function(e) NULL)
  if (is.null(meta$error)) meta$stream <- TRUE

  list(
    reply        = reply %||% "",
    sink_reply   = out$reply,
    n_events     = length(out$raw_data),
    metadata     = meta
  )
}

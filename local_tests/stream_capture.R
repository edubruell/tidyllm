# Shared capture helpers for the streaming fixture recorders.
#
# Used by record_stream_fixtures.R (plain streams) and
# record_stream_tool_fixtures.R (streams that call tools). Kept separate so
# either recorder can be sourced for its helpers without running the other's
# recordings, which cost live API calls.

FIXTURE_DIR <- "local_tests/fixtures/streams"
dir.create(FIXTURE_DIR, showWarnings = FALSE, recursive = TRUE)

# ── Capture ───────────────────────────────────────────────────────────────────

#' Perform a prebuilt streaming request and keep every byte off the wire.
#'
#' Deliberately bypasses `perform_chat_request()` and `handle_stream()`: the
#' point is to record what the provider sent, not what tidyllm made of it.
capture_stream_bytes <- function(request) {
  resp <- httr2::req_perform_connection(request, blocking = TRUE)
  on.exit(try(close(resp), silent = TRUE), add = TRUE)

  bytes  <- raw()
  chunks <- list()
  repeat {
    chunk <- httr2::resp_stream_raw(resp, kb = 0.5)
    if (length(chunk) > 0) {
      bytes  <- c(bytes, chunk)
      chunks <- c(chunks, list(chunk))
      next
    }
    if (httr2::resp_stream_is_complete(resp)) break
  }

  list(
    status       = httr2::resp_status(resp),
    content_type = httr2::resp_header(resp, "content-type") %||% NA_character_,
    bytes        = bytes,
    # Kept separately because chunk boundaries are where the multibyte and
    # partial-event bugs live; a replay that re-chunks differently proves less.
    wire_chunks  = chunks
  )
}

record_fixture <- function(name, request, meta = list()) {
  cat(sprintf("  recording %-28s", name))
  out <- tryCatch(capture_stream_bytes(request), error = function(e) {
    cat(sprintf("FAILED (%s)\n", conditionMessage(e)))
    NULL
  })
  if (is.null(out)) return(invisible(NULL))

  fixture <- c(
    list(
      name          = name,
      recorded_at   = as.character(Sys.time()),
      tidyllm_sha   = tryCatch(
        system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE),
        error = function(e) NA_character_
      ),
      url           = request$url
    ),
    meta,
    out
  )

  saveRDS(fixture, file.path(FIXTURE_DIR, paste0(name, ".rds")))
  cat(sprintf("%6d bytes, %3d chunks, %s\n",
              length(out$bytes), length(out$wire_chunks), out$content_type))
  invisible(fixture)
}


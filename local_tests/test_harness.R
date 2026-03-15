# local_tests/test_harness.R
# Lightweight test harness for live-API local tests.
# Designed for unattended runs — no browser() calls.
# Uses dump.frames() post-mortem dumps for Claude Code debugging.

.llt_env <- new.env(parent = emptyenv())
.llt_env$results <- list()

#' Start a named test suite
llt_suite <- function(.provider) {
  .llt_env$results <- list()
  .llt_env$provider <- .provider
  width <- 70
  header <- sprintf("── %s test suite ", tools::toTitleCase(.provider))
  cat("\n", header, paste(rep("─", max(0, width - nchar(header))), collapse = ""), "\n", sep = "")
}

#' Run a named test block; catches errors, records pass/fail, writes debug dump on failure
llt_test <- function(name, expr) {
  start <- proc.time()
  .trace <- NULL

  result <- tryCatch(
    withCallingHandlers(
      force(expr),
      error = function(e) {
        .trace <<- tryCatch(
          paste(capture.output(traceback(5)), collapse = "\n"),
          error = function(e2) "(traceback unavailable)"
        )
        dump_dir <- "local_tests/debug_state"
        dir.create(dump_dir, showWarnings = FALSE, recursive = TRUE)
        safe_name <- gsub("[^a-zA-Z0-9_]", "_", name)
        dump_path <- file.path(dump_dir, paste0(safe_name, ".rda"))
        tryCatch(
          dump.frames(dumpto = dump_path, to.file = TRUE),
          error = function(e2) NULL
        )
      }
    ),
    error = function(e) list(error = conditionMessage(e), trace = .trace)
  )

  elapsed <- round((proc.time() - start)["elapsed"], 1)

  if (is.list(result) && !is.null(result$error)) {
    cat(sprintf("  \u2717 %-50s [%.1fs]\n    Error: %s\n", name, elapsed, result$error))
    safe_name <- gsub("[^a-zA-Z0-9_]", "_", name)
    .llt_env$results[[length(.llt_env$results) + 1]] <- list(
      name = name,
      status = "fail",
      duration_s = elapsed,
      error = result$error,
      traceback = if (!is.null(result$trace)) strsplit(result$trace, "\n")[[1]] else NULL,
      debug_dump = file.path("local_tests/debug_state", paste0(safe_name, ".rda"))
    )
  } else {
    cat(sprintf("  \u2713 %-50s [%.1fs]\n", name, elapsed))
    .llt_env$results[[length(.llt_env$results) + 1]] <- list(
      name = name,
      status = "pass",
      duration_s = elapsed
    )
  }

  invisible(NULL)
}

#' Assert a condition is TRUE
llt_expect_true <- function(condition, msg = NULL) {
  if (!isTRUE(condition)) {
    stop(msg %||% paste("Expected TRUE, got:", deparse(substitute(condition))))
  }
}

#' Assert an object inherits from an S7 class
llt_expect_s7 <- function(obj, class) {
  if (!S7::S7_inherits(obj, class)) {
    stop(paste("Expected S7 class", class@name, "- got:", paste(class(obj), collapse = "/")))
  }
}

#' Assert get_reply() returns a non-empty string
llt_expect_reply <- function(msg) {
  reply <- get_reply(msg)
  if (!is.character(reply) || nchar(trimws(reply)) == 0) {
    stop("get_reply() returned empty or non-character result")
  }
}

#' Assert get_metadata() contains expected fields
llt_expect_metadata <- function(msg, fields) {
  meta <- get_metadata(msg)
  missing <- setdiff(fields, names(meta))
  if (length(missing) > 0) {
    stop(paste("get_metadata() missing fields:", paste(missing, collapse = ", ")))
  }
}

#' Assert x is a tibble with at least min_rows rows
llt_expect_tibble <- function(x, min_rows = 1) {
  if (!tibble::is_tibble(x)) stop(paste("Expected a tibble, got:", class(x)[1]))
  if (nrow(x) < min_rows) stop(paste("Expected at least", min_rows, "rows, got:", nrow(x)))
}

#' Assert expr runs without throwing
llt_expect_no_error <- function(expr) {
  tryCatch(
    force(expr),
    error = function(e) stop(paste("Unexpected error:", conditionMessage(e)))
  )
}

#' Write results JSON and print console summary
llt_report <- function(.provider = .llt_env$provider %||% "unknown") {
  results <- .llt_env$results
  n_pass  <- sum(sapply(results, function(r) r$status == "pass"))
  n_fail  <- sum(sapply(results, function(r) r$status == "fail"))

  cat(sprintf("\n── Results: %d passed · %d failed", n_pass, n_fail))
  if (n_fail > 0) cat(" ──  see local_tests/results/latest_run.json")
  cat("\n")

  output <- list(
    run_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    provider = .provider,
    summary  = list(passed = n_pass, failed = n_fail, total = length(results)),
    tests    = results
  )

  dir.create("local_tests/results", showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(output, "local_tests/results/latest_run.json",
                       auto_unbox = TRUE, pretty = TRUE)

  ts_file <- file.path("local_tests/results",
                       paste0(.provider, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"))
  jsonlite::write_json(output, ts_file, auto_unbox = TRUE, pretty = TRUE)

  .llt_env$results <- list()
  invisible(output)
}

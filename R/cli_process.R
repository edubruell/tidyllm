#' Find a provider's command line tool, without relying on the PATH alone
#'
#' `Sys.which()` on its own is not enough. A GUI R session does not inherit the
#' PATH from the user's shell profile: RStudio on macOS starts from the launch
#' environment, so `~/.local/bin`, where these CLIs install themselves by
#' default, is frequently absent even though the command runs fine in the same
#' user's terminal. Reported from RStudio on 2026-09-16, with the binary sitting
#' in `~/.local/bin/claude` the whole time.
#'
#' The order is: an explicit setting, then the PATH, then the handful of places
#' the installers actually use. A caller who passes a path of their own gets that
#' path and no searching.
#'
#' @param .binary The command name, or a path to it.
#' @param .option,.env_var The option and environment variable that can hold a
#'   path.
#' @param .fn The provider call named in messages, such as "claude_cli()".
#' @param .install_hint One sentence on how to install and sign in.
#' @param .extra_dirs Installer targets specific to this CLI.
#'
#' @noRd
cli_binary <- function(.binary, .option, .env_var, .fn, .install_hint,
                       .extra_dirs = character()) {
  # A path rather than a bare command name is taken at face value: the user has
  # said where it is, so a search would only second-guess them.
  if (grepl("/", .binary, fixed = TRUE)) {
    expanded <- path.expand(.binary)
    if (file.access(expanded, mode = 1L) == 0) return(expanded)
    stop(glue::glue("`{.binary}` is not an executable file."), call. = FALSE)
  }

  configured <- getOption(.option, Sys.getenv(.env_var))
  if (is.character(configured) && length(configured) == 1 && nzchar(configured)) {
    configured <- path.expand(configured)
    if (file.access(configured, mode = 1L) == 0) return(configured)
    stop(glue::glue(
      "The `{.binary}` command was set to `{configured}`, which is not an executable file.\n",
      "Fix the `{.option}` option or the {.env_var} environment variable."
    ), call. = FALSE)
  }

  found <- Sys.which(.binary)[[1]]
  if (nzchar(found)) return(found)

  # Returned as found, not through `normalizePath()`: on this machine
  # `~/.local/bin/claude` is a symlink into a versioned directory, and resolving
  # it would pin the call to one build of a CLI that updates itself.
  for (candidate in cli_search_paths(.binary, .extra_dirs)) {
    if (file.access(candidate, mode = 1L) == 0) return(candidate)
  }

  stop(glue::glue(
    "The `{.binary}` command was not found.\n",
    "`{.fn}` runs your own installed copy, so it has to be installed first: {.install_hint}\n\n",
    "If `{.binary}` does work in your terminal, this R session simply has a different PATH, ",
    "which is usual in RStudio and other GUI front ends. Run `which {.binary}` in a terminal and then either\n",
    "  options({.option} = \"/the/path/it/printed\")\n",
    "in your .Rprofile, or pass it directly with {sub('()', '', .fn, fixed = TRUE)}(.binary = \"/the/path/it/printed\")."
  ), call. = FALSE)
}

#' Where the command line tools install themselves
#'
#' Checked only after the PATH has failed, so a normal session never reaches
#' them. Each entry is a real installer target: install scripts and `uv tool`
#' write to `~/.local/bin`, Homebrew to its prefix, and a global npm install
#' lands in whichever prefix npm is configured with.
#'
#' @noRd
cli_search_paths <- function(.binary, .extra_dirs = character()) {
  home <- path.expand("~")
  dirs <- c(
    file.path(home, ".local", "bin"),
    .extra_dirs,
    file.path(home, "bin"),
    "/opt/homebrew/bin",
    "/usr/local/bin"
  )
  candidates <- file.path(dirs, .binary)
  if (.Platform$OS.type == "windows") {
    candidates <- c(
      candidates,
      file.path(Sys.getenv("APPDATA"), "npm", paste0(.binary, ".cmd")),
      file.path(Sys.getenv("LOCALAPPDATA"), "Programs", .binary, paste0(.binary, ".exe"))
    )
  }
  candidates
}

#' @noRd
check_processx_installed <- function(.fn = "This provider") {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop(paste(
      .fn, "needs the processx package to run the CLI and read its output.",
      "Install it with install.packages(\"processx\")."
    ), call. = FALSE)
  }
}

#' Flatten a conversation into the single prompt a CLI takes
#'
#' The CLIs accept one prompt, not a message list, so a multi-turn history has
#' to be written out as text. Turns are labelled because without labels a
#' two-turn history reads as one run-on user message and the model loses track
#' of who said what.
#'
#' With `.history = FALSE` only the newest user turn is sent, for a CLI that
#' resumes its own session and already holds the rest.
#'
#' @noRd
cli_prompt_text <- function(.llm, .history = TRUE, .label = "the CLI") {
  turns <- filter_roles(.llm@message_history, c("user", "assistant"))

  if (!isTRUE(.history)) {
    user_turns <- Filter(function(m) identical(m$role, "user"), turns)
    if (length(user_turns) == 0) {
      stop(glue::glue("There is no user message to send to {.label}."), call. = FALSE)
    }
    return(format_message(user_turns[[length(user_turns)]])$content)
  }

  if (length(turns) == 1) return(format_message(turns[[1]])$content)

  labelled <- vapply(turns, function(m) {
    label <- if (identical(m$role, "assistant")) "Assistant" else "User"
    paste0(label, ": ", format_message(m)$content)
  }, character(1))

  paste(labelled, collapse = "\n\n")
}

#' The most recent CLI session id in a conversation's metadata
#'
#' @noRd
cli_last_session_id <- function(.llm) {
  history <- .llm@message_history
  for (i in rev(seq_along(history))) {
    meta <- history[[i]]$meta
    sid  <- meta$specific_metadata$session_id
    if (!is.null(sid) && !is.na(sid) && nzchar(sid)) return(sid)
  }
  NULL
}

#' The command line a request runs, and what `.dry_run` hands back
#'
#' A `tidyllm_cli_command` rather than a bare character vector so that printing
#' it shows the command a user could paste into a terminal, which is the CLI
#' equivalent of inspecting an httr2 request. `args` stays a vector: the command
#' is never run through a shell, so nothing here is ever re-parsed and there is
#' no quoting to get wrong. `env` holds variables added to the child's
#' environment. `env_from` maps a child variable to the R session variable it is
#' copied from when the process starts, so a key never sits in the object.
#'
#' @noRd
new_cli_command <- function(.binary, .args, .stdin = NULL, .env = NULL, .env_from = NULL) {
  structure(
    list(binary = .binary, args = .args, stdin = .stdin, env = .env, env_from = .env_from),
    class = "tidyllm_cli_command"
  )
}

#' @export
print.tidyllm_cli_command <- function(x, ...) {
  cat("<tidyllm CLI command>\n")
  if (length(x$env)) cat(paste0(names(x$env), "=", x$env, collapse = " "), "")
  if (length(x$env_from)) cat(paste0(names(x$env_from), "=$", x$env_from, collapse = " "), "")
  cat(paste(c(x$binary, x$args), collapse = " "), "\n")
  if (!is.null(x$stdin)) {
    preview <- substr(x$stdin, 1, 400)
    cat("\n-- prompt on stdin ------------------------------------------\n")
    cat(preview)
    if (nchar(x$stdin) > 400) cat("\n[... ", nchar(x$stdin) - 400, " more characters]", sep = "")
    cat("\n")
  }
  invisible(x)
}

#' @noRd
cli_start_process <- function(.command, .fn = "This provider") {
  check_processx_installed(.fn)
  child_env <- c(.command$env, vapply(.command$env_from, Sys.getenv, character(1)))
  proc <- processx::process$new(
    command = .command$binary,
    args    = .command$args,
    env     = if (length(child_env)) c("current", child_env) else NULL,
    stdin   = if (is.null(.command$stdin)) NULL else "|",
    stdout  = "|",
    stderr  = "|"
  )
  if (!is.null(.command$stdin)) {
    proc$write_input(paste0(.command$stdin, "\n"))
    proc$get_input_connection() |> close()
  }
  proc
}

#' Run a CLI to completion and hand back its output lines
#'
#' The output is drained while the child runs rather than read at the end. A
#' pipe holds only a fixed number of bytes, so a reply larger than the buffer
#' deadlocks a wait-then-read: the child blocks writing, the parent blocks
#' waiting, and neither moves. Long answers are exactly the case these providers
#' are for.
#'
#' @noRd
cli_run_blocking <- function(.command, .timeout, .label, .fn = "This provider") {
  proc     <- cli_start_process(.command, .fn)
  deadline <- Sys.time() + .timeout
  out      <- character(0)
  err      <- character(0)

  repeat {
    proc$poll_io(250)
    out <- c(out, proc$read_output_lines())
    err <- c(err, proc$read_error_lines())

    if (!proc$is_alive() && !proc$is_incomplete_output()) break

    if (Sys.time() > deadline) {
      proc$kill()
      stop(sprintf("The %s produced no result within %g seconds; giving up.", .label, .timeout),
           call. = FALSE)
    }
  }
  out <- c(out, proc$read_all_output_lines())
  err <- c(err, proc$read_all_error_lines())
  cli_check_output(out, err, proc$get_exit_status(), .label)
}

#' Stop with the CLI's own error text when it wrote nothing to stdout
#'
#' @noRd
cli_check_output <- function(.out, .err, .status, .label) {
  if (!length(.out) || !nzchar(paste(.out, collapse = ""))) {
    stop(glue::glue(
      "The {.label} exited with status {.status} and produced no output.\n",
      "{substr(paste(.err, collapse = '\n'), 1, 500)}"
    ), call. = FALSE)
  }
  .out
}

#' Run one round, blocking or streaming, and interpret it
#'
#' A closure on the built request rather than a branch inside
#' `perform_chat_request()`, because that function is written against httr2 from
#' its first line. `new_chat_request(.perform_fn =)` is the seam the pipeline
#' already provides for a provider that performs its own requests.
#'
#' @param .read_output Turns the complete stdout lines of a blocking run into the
#'   response body.
#'
#' @noRd
cli_performer <- function(.read_output) {
  function(.built) {
    if (chat_request_streams(.built)) {
      if (!isTRUE(.built$quiet)) {
        message("\n---------\nStart ", .built$api@long_name, " streaming: \n---------\n")
      }
      proc <- cli_start_process(.built$request, paste0(.built$api@short_name, "()"))
      stream_response <- handle_stream(.built$api, proc,
                                       .on_chunk     = .built$on_chunk,
                                       .idle_timeout = .built$timeout,
                                       .verbose      = !isTRUE(.built$quiet))
      content <- assemble_stream_body(.built$api, stream_response$raw_data)
      interpreted <- interpret_chat_response(
        .built$api,
        list(content = content, headers = list(), status = 200L)
      )
      interpreted$meta$stream <- TRUE
      return(interpreted)
    }

    lines <- cli_run_blocking(.built$request, .built$timeout, .built$api@long_name,
                              paste0(.built$api@short_name, "()"))
    interpret_chat_response(
      .built$api,
      list(content = .read_output(lines), headers = list(), status = 200L)
    )
  }
}

#' Run a non-streaming CLI call in the background of the session
#'
#' The httr2 default cannot serve this: there is no request to promise. A child
#' process is already non-blocking, so the driver only has to look in on it. The
#' poll interval is a compromise: short enough that a quick answer is not left
#' sitting, long enough that a two-minute agent run costs a few hundred wake-ups
#' rather than a few thousand.
#'
#' @noRd
cli_start_async <- function(.api, .job, .read_output) {
  check_later_installed()
  env <- .job$env
  proc <- cli_start_process(env$built$request, paste0(.api@short_name, "()"))
  deadline <- Sys.time() + env$built$timeout
  out <- character(0)
  err <- character(0)

  env$headers     <- list()
  env$http_status <- 200L
  env$cancel_fn   <- function() if (proc$is_alive()) proc$kill()

  poll <- function() {
    if (!identical(env$status, "running")) {
      if (proc$is_alive()) proc$kill()
      return(invisible(NULL))
    }
    out <<- c(out, proc$read_output_lines())
    err <<- c(err, proc$read_error_lines())
    if (proc$is_alive() || proc$is_incomplete_output()) {
      if (Sys.time() > deadline) {
        proc$kill()
        env$status <- "error"
        env$error  <- simpleError(sprintf("The %s produced no result within %g seconds; giving up.",
                                          .api@long_name, env$built$timeout))
        return(invisible(NULL))
      }
      later::later(poll, delay = 0.1)
      return(invisible(NULL))
    }

    outcome <- tryCatch({
      lines <- cli_check_output(c(out, proc$read_all_output_lines()),
                                c(err, proc$read_all_error_lines()),
                                proc$get_exit_status(), .api@long_name)
      .read_output(lines)
    }, error = function(e) e)

    if (inherits(outcome, "condition")) {
      env$status <- "error"
      env$error  <- outcome
      return(invisible(NULL))
    }
    chat_job_finish(.job, outcome)
  }

  later::later(poll, delay = 0.05)
  invisible(NULL)
}

#' Turn a reply into the schema-constrained JSON a CLI is asked for
#'
#' @noRd
cli_schema <- function(.json_schema) {
  if (is.null(.json_schema)) return(NULL)
  if (requireNamespace("ellmer", quietly = TRUE) &&
      S7_inherits(.json_schema, ellmer::TypeObject)) {
    .json_schema <- to_schema(.json_schema)
  }
  .json_schema
}

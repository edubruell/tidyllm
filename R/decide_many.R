#' Split `.state` into its items: one per string, one per list element
#' @noRd
decide_state_items <- function(.state) {
  if (is.character(.state)) return(as.list(.state))
  if (is.list(.state) && !is.object(.state)) return(.state)
  list(.state)
}

#' Run a step for state `.i` of `.n` and name the state in any error
#' @param .f A function of no arguments.
#' @noRd
at_state <- function(.i, .n, .f) {
  if (.n == 1) return(.f())
  position <- paste0("Item ", .i, " of ", .n)
  tryCatch(.f(), error = \(e) cli::cli_abort("{position}: {conditionMessage(e)}", call = NULL))
}

#' Parse `.state` into one parsed state per item, once, at the boundary
#'
#' A character vector and a list give one state per element; anything else is one
#' state. Names of the outer vector or list become the `state` labels.
#' @param .state As accepted by `decide()`.
#' @return A list of parsed states from `decide_parts()`.
#' @noRd
decide_states <- function(.state) {
  items <- decide_state_items(.state)
  c("`.state` must hold at least one item" = length(items) > 0) |> validate_inputs()
  labels <- names(items) %||% rep("", length(items))
  parsed <- purrr::imap(items, \(s, i) at_state(i, length(items), \() decide_parts(s)))
  purrr::map2(parsed, labels, \(s, l) if (!is.na(l) && nzchar(l)) utils::modifyList(s, list(label = l)) else s)
}

#' Apply the PDF fallback and the media and limit checks to one parsed state
#' @param .state A parsed state.
#' @param .questions The normalised questions.
#' @param .meta Provider metadata from `provider_metadata()`.
#' @return The state, with PDF parts replaced by text where needed.
#' @noRd
decide_checked <- function(.state, .questions, .meta) {
  check_decide_box(.state, .questions, .meta)
  state <- decide_pdf_fallback(.state, .meta)
  check_decide_media(state, .meta)
  check_decide_state(state, .questions, .meta)
  state
}

#' Wrap several parsed states for the provider functions
#' @param .states A list of parsed states.
#' @param .max_active Requests in flight at once.
#' @param .progress Show a progress bar.
#' @return An object of class `decide_many`.
#' @noRd
decide_many <- function(.states, .max_active = 5, .progress = FALSE) {
  structure(list(states = .states, max_active = .max_active, progress = .progress), class = "decide_many")
}

#' Prepare one request for a run: timeout, status as data, retries
#' @noRd
decide_prepare_request <- function(.request, .timeout, .max_tries) {
  .request |>
    httr2::req_timeout(.timeout) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_retry(
      max_tries = .max_tries,
      retry_on_failure = TRUE,
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 503, 529)
    )
}

#' The result of one state: rows, usage, model and the error text if it failed
#'
#' A transport failure, an HTTP error or an unreadable reply gives one NA row
#' per question, as a refused question does.
#' @param .response An httr2 response, or the condition `req_perform_parallel()` returns.
#' @return `list(rows, usage, model, error)`; `error` is `NULL` on success.
#' @noRd
decide_state_result <- function(.api, .response, .questions) {
  tryCatch({
    if (inherits(.response, "error")) cli::cli_abort("{conditionMessage(.response)}", call = NULL)
    parsed <- suppressWarnings(
      parse_decide_response(.api, decide_reply_content(.api, .response), .questions)
    )
    c(parsed, list(error = NULL))
  }, error = \(e) {
    list(
      rows = purrr::map(.questions, refused_row) |> purrr::list_rbind(),
      usage = decide_usage(list()), model = NA_character_, error = conditionMessage(e)
    )
  })
}

#' Add one count across states: a number, or a named vector summed by name
#' @param .values The values of one usage field, one per state.
#' @noRd
decide_sum_field <- function(.values) {
  flat <- unlist(.values)
  if (length(flat) == 0 || all(is.na(flat))) return(NA_integer_)
  if (is.null(names(flat))) return(as.integer(sum(flat, na.rm = TRUE)))
  sums <- tapply(flat, names(flat), sum, na.rm = TRUE)
  stats::setNames(as.numeric(sums), names(sums))
}

#' Usage of many states: every field summed over the states that succeeded
#' @param .results Results from `decide_state_result()`.
#' @noRd
decide_usage_total <- function(.results) {
  usages <- purrr::map(Filter(\(r) is.null(r$error), .results), "usage")
  fields <- unique(unlist(lapply(usages, names)))
  stats::setNames(lapply(fields, \(f) decide_sum_field(lapply(usages, `[[`, f))), fields)
}

#' Stack the rows of all states behind a `state` label and an integer `state_id`
#' @param .results Results from `decide_state_result()`.
#' @param .states The parsed states, in the same order.
#' @noRd
decide_many_rows <- function(.results, .states) {
  purrr::imap(.results, \(r, i) tibble::add_column(r$rows, state = .states[[i]]$label,
                                                    state_id = as.integer(i), .before = 1)) |>
    purrr::list_rbind()
}

#' One warning each for the failed states and for the questions left unanswered
#' @param .results Results from `decide_state_result()`.
#' @noRd
warn_many <- function(.results) {
  errors <- purrr::map_chr(.results, \(r) r$error %||% NA_character_)
  failed <- which(!is.na(errors))
  if (length(failed) > 0) {
    shown <- paste0("state ", failed, ": ", errors[failed])[seq_len(min(3, length(failed)))]
    cli::cli_warn("{length(failed)} of {length(.results)} state{?s} failed and give NA rows. First errors: {shown}")
  }
  refused <- sum(purrr::map_int(.results[is.na(errors)], \(r) sum(decide_unanswered(r$rows))))
  if (refused > 0) {
    cli::cli_warn("The model gave no answer for {refused} question{?s} across the states that ran.")
  }
  invisible(.results)
}

#' Build, perform in parallel and assemble the requests of many states
#'
#' @return A list of httr2 requests when `.dry_run`, otherwise one tibble with a
#'   leading `state` and `state_id`. Its `usage` attribute sums the states that
#'   succeeded, `latency` is the wall-clock seconds of the whole run and `model`
#'   is the first model a reply names.
#' @noRd
run_decide_many <- function(.api, .many, .questions, .dry_run = FALSE, .timeout = 60, .max_tries = 3) {
  requests <- purrr::map(.many$states, \(s) decide_request(.api, s, .questions, .dry_run))
  if (.dry_run) return(requests)
  started <- Sys.time()
  responses <- requests |>
    purrr::map(decide_prepare_request, .timeout, .max_tries) |>
    httr2::req_perform_parallel(on_error = "continue", progress = .many$progress,
                                max_active = .many$max_active)
  results <- purrr::map(responses, \(r) decide_state_result(.api, r, .questions))
  warn_many(results)
  out <- decide_many_rows(results, .many$states)
  models <- stats::na.omit(purrr::map_chr(results, "model"))
  attr(out, "usage") <- decide_usage_total(results)
  attr(out, "latency") <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  attr(out, "model") <- if (length(models) > 0) models[[1]] else .api@model
  out
}

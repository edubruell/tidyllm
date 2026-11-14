DECIDE_TYPES <- c("yes_no", "choice", "score", "box")
DECIDE_BOX_PROVIDERS <- "jev"
BOX_EDGES <- c("top", "bottom", "left", "right")

#' Turn one schema property into a normalised question
#'
#' @param .name Question name.
#' @param .prop The JSON schema fragment of the field.
#' @param .meta The `decide_meta` hint for this field, or NULL.
#' @return A list with `name`, `type`, `instructions`, `options`, `descriptions`
#'   and, for yes/no questions with definitions, `true` and `false`.
#' @noRd
decide_question <- function(.name, .prop, .meta) {
  if (isTRUE(.meta$box)) {
    return(list(name = .name, type = "box", instructions = .prop$description %||% .name,
                options = character(0), descriptions = character(0)))
  }
  stats::setNames(
    .prop$type %in% c("boolean", "string") && is.null(.prop$items),
    glue::glue("Question `{.name}` must be field_lgl() or field_fct(); nested objects and vectors are not questions.")
  ) |> validate_inputs()
  options <- as.character(unlist(.prop$enum))
  stats::setNames(
    !(identical(.prop$type, "string") && length(options) == 0),
    glue::glue("Question `{.name}` is free text. decide() asks yes/no, choice or score questions only: use field_lgl() or field_fct().")
  ) |> validate_inputs()
  type <- if (identical(.prop$type, "boolean")) "yes_no" else if (isTRUE(.meta$ordered)) "score" else "choice"
  stats::setNames(
    type == "yes_no" || length(options) >= 2,
    glue::glue("Question `{.name}` needs field_fct(.levels = ) with at least two levels.")
  ) |> validate_inputs()
  list(
    name = .name,
    type = type,
    instructions = .prop$description %||% .name,
    options = options,
    descriptions = if (length(.meta$level_descriptions) > 0) .meta$level_descriptions else options,
    true = if (length(.meta$true) > 0) .meta$true,
    false = if (length(.meta$false) > 0) .meta$false
  )
}

#' Parse a `tidyllm_schema()` into the question list once, at the boundary
#'
#' Backends never read the schema; they read this list.
#' @param .questions A `tidyllm_schema()` whose fields are the questions.
#' @return A list of normalised questions, named by question name.
#' @noRd
decide_questions <- function(.questions) {
  c("`.questions` must be a tidyllm_schema() with at least one field" =
      is.list(.questions) && length(.questions$properties) > 0) |>
    validate_inputs()
  props <- .questions$properties
  meta <- attr(.questions, "decide_meta")
  out <- lapply(names(props), \(n) decide_question(n, props[[n]], meta[[n]]))
  stats::setNames(out, names(props))
}

#' One result row
#' @param .confidence The backend's confidence in the answer, or `NA`.
#' @param .probabilities A named numeric vector over the options.
#' @param .box For box questions, the named numeric vector `top`, `bottom`, `left`, `right`.
#' @return A one-row tibble in the `decide()` column layout.
#' @noRd
decide_row <- function(.question, .type, .answer, .probability,
                       .confidence = NA_real_, .probabilities = numeric(0), .box = NULL) {
  row <- tibble::tibble(
    question = .question,
    type = .type,
    answer = .answer,
    probability = as.numeric(.probability),
    confidence = as.numeric(.confidence),
    probabilities = list(.probabilities)
  )
  if (is.null(.box)) row else tibble::as_tibble(c(as.list(row), as.list(.box)))
}

#' Confidence the reply reports, or NA when absent or null
#' @noRd
decide_confidence <- function(.answer) {
  value <- suppressWarnings(as.numeric(.answer$confidence))
  if (length(value) == 1) value else NA_real_
}

#' Probabilities of a yes/no answer as a named vector
#' @param .p Probability of yes.
#' @noRd
yes_no_probs <- function(.p) {
  c(yes = .p, no = 1 - .p)
}

#' Result row of a yes/no answer; a missing confidence falls back to the probability of the answer
#' @param .p Probability of yes.
#' @param .confidence The reply's confidence, or `NA`.
#' @noRd
decide_yes_no_row <- function(.q, .p, .confidence) {
  top <- max(.p, 1 - .p)
  decide_row(.q$name, "yes_no", if (.p >= 0.5) "yes" else "no", top,
             .confidence = if (is.na(.confidence)) top else .confidence, .probabilities = yes_no_probs(.p))
}

#' Name System One probabilities by option and order them as the question lists them
#'
#' Score probabilities come keyed by level position counted from 0, in any order.
#' @param .q A normalised question.
#' @param .probs A named numeric vector as the reply gives it.
#' @return A named numeric vector; empty when the reply has none.
#' @noRd
decide_option_probs <- function(.q, .probs) {
  if (length(.probs) == 0 || is.null(names(.probs))) return(stats::setNames(numeric(0), character(0)))
  if (.q$type == "score" && !all(names(.probs) %in% .q$options)) {
    index <- suppressWarnings(as.integer(names(.probs))) + 1L
    if (!anyNA(index) && all(index <= length(.q$options))) names(.probs) <- .q$options[index]
  }
  known <- intersect(.q$options, names(.probs))
  if (length(known) == length(.probs)) .probs[known] else .probs
}

#' Check a decoded reply is a list with an answers list covering every question
#'
#' A question whose answer is present but null counts as covered; it is read as
#' a refusal by the row builders.
#' @noRd
validate_decide_reply <- function(.body, .questions) {
  c("The reply is not a JSON object" = is.list(.body)) |> validate_inputs()
  c("The reply has no `answers` object" = is.list(.body$answers)) |> validate_inputs()
  missing <- setdiff(names(.questions), names(.body$answers))
  if (length(missing) > 0) {
    cli::cli_abort("The reply has no answer for: {paste(missing, collapse = ', ')}.", call = NULL)
  }
  invisible(.body)
}

#' Is this answer null, an error or a refusal, or without its key field (an empty object counts as without)
#' @param .answer One decoded answer.
#' @param .field The field that carries the answer for this question type.
#' @noRd
decide_refused <- function(.answer, .field) {
  is.null(.answer) || !is.list(.answer) || !is.null(.answer$error) ||
    identical(.answer$type, "refusal") || length(.answer[[.field]]) == 0
}

#' The result row for a question the model did not answer
#' @noRd
refused_row <- function(.q) {
  box <- if (.q$type == "box") stats::setNames(rep(NA_real_, 4), BOX_EDGES)
  decide_row(.q$name, .q$type, NA_character_, NA_real_, .box = box)
}

#' Which result rows are unanswered: no answer, and for box rows no coordinates
#' @param .rows The result rows.
#' @noRd
decide_unanswered <- function(.rows) {
  boxed <- (.rows[["type"]] %||% "") == "box" & !is.na(.rows[["top"]] %||% NA_real_)
  is.na(.rows$answer) & !boxed
}

#' Warn once about the questions that came back refused
#' @param .rows The result rows.
#' @noRd
warn_refused <- function(.rows) {
  refused <- .rows$question[decide_unanswered(.rows)]
  if (length(refused) > 0) {
    cli::cli_warn("The model gave no answer for {length(refused)} question{?s}: {.val {refused}}.")
  }
  invisible(.rows)
}

#' Usage block with NA for whatever the backend does not report
#' @noRd
decide_usage <- function(.usage) {
  list(
    input_tokens = as_token_count(.usage$input_tokens),
    output_tokens = as_token_count(.usage$output_tokens)
  )
}

#' Criteria of a yes/no question: Gemini and the System One servers name the keys differently
#' @param .q A normalised yes/no question.
#' @param .gemini Whether to use Gemini's key names.
#' @return A named list, or NULL when the question has no definitions.
#' @noRd
yes_no_criteria <- function(.q, .gemini) {
  if (is.null(.q$true)) return(NULL)
  keys <- if (.gemini) c("true_description", "false_description") else c("true", "false")
  stats::setNames(list(.q$true, .q$false), keys)
}

#' System One question body for one question (Jev, Clef, Gemini, Ollama)
#' @noRd
system_one_question <- function(.q, .gemini = FALSE) {
  switch(.q$type,
    yes_no = purrr::compact(list(type = "noul", instructions = .q$instructions,
                                 criteria = yes_no_criteria(.q, .gemini))),
    choice = list(type = "choice", instructions = .q$instructions,
                  criteria = stats::setNames(as.list(.q$descriptions), .q$options)),
    score = list(type = "score", instructions = .q$instructions,
                 criteria = as.list(unname(.q$descriptions))),
    box = list(type = "box", instructions = .q$instructions)
  )
}

#' System One request body; Gemini names the state `input`
#' @param .extra Further top-level fields, such as Jev's `assets`.
#' @noRd
system_one_body <- function(.state, .questions, .model = NULL, .state_key = "state", .extra = list()) {
  gemini <- identical(.state_key, "input")
  body <- list(model = .model, questions = purrr::map(unname(.questions), system_one_question, gemini) |>
                 stats::setNames(names(.questions)))
  body[[.state_key]] <- .state
  purrr::compact(c(body, .extra))
}

#' Probability of the chosen option, from the option table or the reply confidence
#' @noRd
choice_probability <- function(.q, .answer, .probs) {
  found <- c(unname(.probs[.answer$choice]), suppressWarnings(as.numeric(.answer$confidence)))
  found <- found[!is.na(found)]
  stats::setNames(
    length(found) > 0,
    glue::glue("Question `{.q$name}`: the reply names no probability for the chosen option.")
  ) |> validate_inputs()
  found[[1]]
}

#' The four edges of a box answer, with a warning when they are not a sensible box
#'
#' Values are kept as the model returns them; the vendor documents fractions
#' but the schema does not bound them.
#' @param .q The box question.
#' @param .box The decoded `box` object of the reply.
#' @return A named numeric vector `top`, `bottom`, `left`, `right`.
#' @noRd
decide_box_edges <- function(.q, .box) {
  box <- if (is.list(.box)) .box else list()
  edges <- vapply(BOX_EDGES, \(e) suppressWarnings(as.numeric(box[[e]] %||% NA_real_))[1], numeric(1))
  stats::setNames(
    !anyNA(edges),
    glue::glue("Question `{.q$name}`: the reply box lacks a numeric top, bottom, left or right.")
  ) |> validate_inputs()
  sensible <- all(edges >= 0 & edges <= 1) && edges[["top"]] < edges[["bottom"]] && edges[["left"]] < edges[["right"]]
  if (!sensible) {
    cli::cli_warn("Question {.val {(.q$name)}}: the box {.val {round(edges, 3)}} is not inside the image or has no area; the values are kept as returned.")
  }
  edges
}

#' Result row of a box question
#' @noRd
decide_box_row <- function(.q, .answer) {
  decide_row(.q$name, "box", NA_character_, NA_real_, .confidence = decide_confidence(.answer),
             .box = decide_box_edges(.q, .answer$box))
}

#' Read one System One answer into a result row
#' @noRd
system_one_row <- function(.q, .answer) {
  field <- c(yes_no = "noul", choice = "choice", score = "probabilities", box = "box")[[.q$type]]
  if (decide_refused(.answer, field)) return(refused_row(.q))
  if (.q$type == "box") return(decide_box_row(.q, .answer))
  probs <- unlist(.answer$probabilities)
  confidence <- decide_confidence(.answer)
  if (.q$type == "yes_no") {
    return(decide_yes_no_row(.q, as.numeric(.answer$noul), confidence))
  }
  by_option <- decide_option_probs(.q, probs)
  if (.q$type == "choice") {
    return(decide_row(.q$name, "choice", .answer$choice, choice_probability(.q, .answer, probs),
                      .confidence = confidence, .probabilities = by_option))
  }
  best <- which.max(by_option)
  decide_row(.q$name, "score", names(by_option)[best], by_option[[best]],
             .confidence = confidence, .probabilities = by_option)
}

#' Parse a System One reply into rows, usage and model
#' @return `list(rows, usage, model)`.
#' @noRd
parse_system_one <- function(.body, .questions) {
  validate_decide_reply(.body, .questions)
  rows <- purrr::map(.questions, \(q) system_one_row(q, .body$answers[[q$name]])) |>
    purrr::list_rbind() |>
    warn_refused()
  list(rows = rows, usage = decide_usage(.body$usage), model = .body$model %||% NA_character_)
}

#' Readable text from an error field that may be a string, an object or a list
#'
#' Covers a plain string, an object with `message` or `msg` (Jev, OpenAI), and
#' the FastAPI list of validation entries with `loc` and `msg`.
#' @return A single string, or NULL when the field is empty.
#' @noRd
decide_error_text <- function(.x) {
  if (length(.x) == 0) return(NULL)
  if (is.character(.x)) return(paste(.x, collapse = "; "))
  if (!is.list(.x)) return(as.character(.x))
  entries <- if (is.null(names(.x))) .x else list(.x)
  text <- purrr::map_chr(entries, \(e) {
    msg <- if (is.list(e)) (e$message %||% e$msg) else e
    loc <- if (is.list(e) && !is.null(e$loc)) paste0(paste(unlist(e$loc), collapse = "."), " ") else ""
    paste0(loc, as.character(msg %||% jsonlite::toJSON(e, auto_unbox = TRUE)))
  })
  paste(text, collapse = "; ")
}

#' Message for a failed decision request, with the provider's detail when present
#' @noRd
decide_error_message <- function(.api, .content, .status) {
  text <- decide_error_text(.content$detail) %||%
    decide_error_text(.content$error) %||% "no detail"
  glue::glue("{.api@long_name} returned HTTP {.status}: {text}")
}

#' Decode a JSON body, or NULL when it is empty or not JSON
#' @noRd
decide_body_json <- function(.response) {
  tryCatch(httr2::resp_body_json(.response), error = \(e) NULL)
}

#' The first 200 characters of a plain-text body, or NULL when it is empty
#'
#' OpenAI answers a rejected credential with plain text, not JSON.
#' @noRd
decide_plain_body <- function(.response) {
  text <- tryCatch(trimws(httr2::resp_body_string(.response)), error = \(e) "")
  if (nzchar(text)) substr(text, 1, 200) else NULL
}

#' Turn an httr2 response into the decoded body, or stop with a readable message
#'
#' The status is checked before the body is trusted, so an HTML or empty error
#' page reports the HTTP status instead of a JSON parse error.
#' @return The decoded reply body.
#' @noRd
decide_reply_content <- function(.api, .response) {
  status <- httr2::resp_status(.response)
  content <- decide_body_json(.response)
  if (status >= 400) {
    fallback <- list(detail = decide_plain_body(.response))
    cli::cli_abort("{decide_error_message(.api, if (is.list(content)) content else fallback, status)}", call = NULL)
  }
  stats::setNames(
    is.list(content),
    glue::glue("{.api@long_name} returned HTTP {status} with a body that is not JSON")
  ) |> validate_inputs()
  content
}

#' Build, time and perform one decision request, then assemble the tibble
#'
#' @param .state The parsed state from `decide_parts()`, or a `decide_many`.
#' @param .questions The normalised question list from `decide_questions()`.
#' @param .dry_run Return the httr2 request without sending it.
#' @param .timeout Timeout in seconds.
#' @param .max_tries Maximum attempts.
#' @return The httr2 request when `.dry_run`, otherwise a tibble with the
#'   attributes `usage`, `latency` and `model`.
#' @noRd
run_decide <- function(.api, .state, .questions, .dry_run = FALSE, .timeout = 60, .max_tries = 3) {
  if (inherits(.state, "decide_many")) {
    return(run_decide_many(.api, .state, .questions, .dry_run, .timeout, .max_tries))
  }
  request <- decide_request(.api, .state, .questions, .dry_run)
  if (.dry_run) return(request)
  started <- Sys.time()
  response <- request |> decide_prepare_request(.timeout, .max_tries) |> httr2::req_perform()
  parsed <- parse_decide_response(.api, decide_reply_content(.api, response), .questions)
  out <- parsed$rows
  attr(out, "usage") <- parsed$usage
  attr(out, "latency") <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  attr(out, "model") <- if (is.na(parsed$model)) .api@model else parsed$model
  out
}

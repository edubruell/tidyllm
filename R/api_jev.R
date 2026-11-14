#' The Jev provider (TypeSafe AI System One API and compatible servers)
#' @noRd
api_jev <- new_class("Jev", APIProvider, properties = list(
  server = class_character,
  model = class_character
))

#' Jev decision request: System One body, bearer key
#' @noRd
method(decide_request, api_jev) <- function(.api, .state, .questions, .dry_run = FALSE) {
  parts <- jev_state_assets(.state)
  request <- httr2::request(paste0(sub("/+$", "", .api@server), "/v1/systemone")) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_headers_redacted(Authorization = paste("Bearer", get_api_key(.api, .dry_run))) |>
    httr2::req_body_json(
      system_one_body(parts$state, .questions, .api@model, .extra = list(assets = parts$assets)),
      auto_unbox = TRUE
    )
  if (length(parts$assets) == 0) return(request)
  httr2::req_headers(request, "TypeSafe-Danger-Zone-Images" = "true")
}

#' Jev reply: System One answers keyed by question name
#' @noRd
method(parse_decide_response, api_jev) <- function(.api, .body, .questions) {
  parse_system_one(.body, .questions)
}

#' Answer yes/no, choice and score questions with a decision model (Jev)
#'
#' Provider-level implementation behind `decide(.provider = jev())`.
#'
#' @param .server Base URL of a System One server; `NULL` uses `https://api.typesafe.ai`.
#' @param .model Model name; default `"jev-latest"`.
#' @inheritParams run_decide
#' @noRd
jev_decide <- function(.state, .questions, .server = NULL, .model = "jev-latest",
                       .dry_run = FALSE, .timeout = 60, .max_tries = 3) {
  api <- api_jev(
    short_name = "jev",
    long_name = "Jev",
    api_key_env_var = "TYPESAFE_API_KEY",
    server = .server %||% "https://api.typesafe.ai",
    model = .model
  )
  run_decide(api, .state, .questions, .dry_run, .timeout, .max_tries)
}

#' Jev and System One compatible servers
#'
#' Provider for `decide()`. The default server is TypeSafe AI's hosted Jev, read
#' with the key in `TYPESAFE_API_KEY`. Pass `.server` to reach any server that
#' speaks the same API, such as a Clef or CLM deployment.
#'
#' @param .server Base URL of the server, without a path. `NULL` (default) is
#'   `https://api.typesafe.ai`.
#' @param .model Model name; default `"jev-latest"`.
#' @return A provider call to pass as `.provider` to `decide()`.
#' @examples
#' \dontrun{
#' decide("The invoice was paid late.",
#'        tidyllm_schema(late = field_lgl("Was the payment late?")), jev())
#' }
#' @export
jev <- create_provider_function(
  .name = "jev",
  decide = jev_decide,
  .decide_media = c("image")
)

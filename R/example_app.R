#' Run a tidyllm example Shiny app
#'
#' The apps ship with the installed package, so they always match the version of
#' tidyllm you have. Each one defaults to a local `ollama()` model and therefore
#' runs with no API key and no spend; a dropdown switches to the cloud providers.
#'
#' Available apps:
#'
#' `"model_explainer"`: fits a linear model to a small public dataset and streams
#' two explanations of the coefficients side by side, one plain-English and one
#' adversarial. Every number is computed in R; the model only narrates the
#' coefficient table it is shown.
#'
#' @param .app Name of the app to run. With no argument the available apps are
#'   listed.
#' @param ... Passed to [shiny::runApp()].
#' @return Invisibly, the path to the app that was run.
#'
#' @examples
#' \dontrun{
#' tidyllm_example_app()
#' tidyllm_example_app("model_explainer")
#' }
#'
#' @export
tidyllm_example_app <- function(.app = NULL, ...) {
  root <- system.file("examples", package = "tidyllm")
  apps <- list.dirs(root, full.names = FALSE, recursive = FALSE)

  if (is.null(.app)) {
    cli::cli_inform(c(
      "Available example apps:",
      stats::setNames(apps, rep("*", length(apps))),
      "i" = 'Run one with {.code tidyllm_example_app("{apps[1]}")}.'
    ))
    return(invisible(apps))
  }

  if (!.app %in% apps) {
    stop("No example app called '", .app, "'. Available: ",
         paste(apps, collapse = ", "), call. = FALSE)
  }

  rlang::check_installed("shiny", reason = "to run a tidyllm example app.")

  path <- file.path(root, .app)
  shiny::runApp(path, ...)
  invisible(path)
}

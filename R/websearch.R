#' Give a Model Web Search
#'
#' @description
#' Creates a tool that lets any model search the web while it answers. Pass it to
#' `chat()` with `.tools`, and the model decides when to search and what to search
#' for. It works the same way with every provider that supports tools, including
#' local models through `ollama()` or `llamacpp()` that have no web access of their
#' own.
#'
#' The model only chooses the search query. Everything else, such as how many
#' results come back and whether full page text is included, is fixed when you
#' create the tool, so a model cannot run up your search bill by asking for more.
#'
#' @param .backend The search service to use. Currently only `"tavily"`, which
#'   needs a `TAVILY_API_KEY` environment variable. Tavily's free plan gives
#'   1,000 credits per month without a credit card; one basic search costs one
#'   credit.
#' @param .max_results How many results each search returns, between 1 and 20.
#' @param .include_content If `TRUE`, each result also carries the text of the
#'   page, cut to `.max_chars` characters. This helps with questions a short
#'   excerpt cannot answer, but it makes every tool result much longer.
#' @param .max_chars The maximum number of characters of page text per result
#'   when `.include_content = TRUE`.
#' @param .timeout Seconds to wait for the search service before giving up.
#' @param ... Further search options passed to the search service by name. For
#'   Tavily these include `search_depth` (`"basic"`, `"advanced"`, `"fast"` or
#'   `"ultra-fast"`; `"advanced"` costs two credits), `topic` (`"general"`,
#'   `"news"` or `"finance"`), `time_range` (`"day"`, `"week"`, `"month"` or
#'   `"year"`), `start_date` and `end_date` (`"YYYY-MM-DD"`), `include_domains`,
#'   `exclude_domains`, `country` and `include_answer`.
#'
#' @return A tool object to pass to the `.tools` argument of `chat()`.
#'
#' @details
#' Each search returns one block of text to the model: the query, the date of the
#' search, and a numbered list of results with title, URL, publication date where
#' the service knows it, and an excerpt. The tool asks the model to cite the URLs
#' it relies on.
#'
#' If a search fails, for example because the key is wrong or the monthly credits
#' are used up, the model receives the error message as the search result instead
#' of the conversation stopping. It will usually tell you what went wrong.
#'
#' @examples
#' \dontrun{
#' llm_message("What changed in the latest R release?") |>
#'   chat(ollama(), .tools = websearch_tool())
#'
#' news_search <- websearch_tool(.max_results = 8, topic = "news", time_range = "week")
#' llm_message("Summarise this week's news on EU AI regulation.") |>
#'   chat(claude(), .tools = news_search)
#' }
#'
#' @export
websearch_tool <- function(.backend = c("tavily"),
                           .max_results = 5,
                           .include_content = FALSE,
                           .max_chars = 4000,
                           .timeout = 30,
                           ...) {
  .backend <- match.arg(.backend)
  backend  <- websearch_backends[[.backend]]
  options  <- list(...)

  c(
    ".max_results must be a whole number between 1 and 20" =
      is.numeric(.max_results) && length(.max_results) == 1 &&
      .max_results >= 1 && .max_results <= 20 && .max_results == round(.max_results),
    ".include_content must be TRUE or FALSE" =
      is.logical(.include_content) && length(.include_content) == 1 && !is.na(.include_content),
    ".max_chars must be a positive number" =
      is.numeric(.max_chars) && length(.max_chars) == 1 && .max_chars > 0,
    ".timeout must be a positive number of seconds" =
      is.numeric(.timeout) && length(.timeout) == 1 && .timeout > 0,
    "Search options in ... must all be named" =
      length(options) == 0 || (!is.null(names(options)) && all(nzchar(names(options))))
  ) |> validate_inputs()

  backend$check_options(options)

  if (!nzchar(Sys.getenv(backend$key_env))) {
    stop(sprintf(
      "%s is not set. Please set it with: Sys.setenv(%s = \"YOUR-KEY-GOES-HERE\")",
      backend$key_env, backend$key_env
    ))
  }

  settings <- list(
    max_results     = as.integer(.max_results),
    include_content = .include_content,
    max_chars       = as.integer(.max_chars),
    timeout         = .timeout,
    options         = options
  )

  search <- function(query) {
    websearch_run(backend, query, settings)
  }

  TOOL(
    description = paste(
      "Search the web for current information.",
      sprintf("Today's date is %s.", format(Sys.Date(), "%Y-%m-%d")),
      "Returns numbered results with title, URL, publication date where known,",
      "and a text excerpt. Search again with a different query if the results",
      "do not answer the question. Cite the URLs of the results you use."
    ),
    input_schema = list(
      query = field_chr("The search query, phrased the way you would type it into a search engine")
    ),
    func    = search,
    name    = "tidyllm_web_search",
    builtin = list()
  )
}

websearch_run <- function(.backend, .query, .settings) {
  if (!is.character(.query) || length(.query) != 1 || nchar(trimws(.query)) < 2) {
    return("Web search failed: the query must be a single string of at least two characters.")
  }

  response <- tryCatch(
    .backend$build_request(.query, .settings) |>
      httr2::req_timeout(.settings$timeout) |>
      httr2::req_retry(
        max_tries    = 3,
        is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503)
      ) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) e
  )

  if (inherits(response, "error")) {
    return(paste("Web search failed:", conditionMessage(response)))
  }
  if (httr2::resp_status(response) >= 400) {
    return(sprintf("Web search failed (HTTP %d): %s",
                   httr2::resp_status(response),
                   .backend$error_message(response)))
  }

  parsed <- .backend$parse_response(httr2::resp_body_json(response), .settings)
  format_search_results(.query, parsed, Sys.Date())
}

#' Turn parsed search results into the text a model receives
#'
#' @param .parsed A list with `answer` (a string or `NULL`) and `results`, a
#'   tibble with columns `title`, `url`, `published`, `snippet` and `text`.
#' @noRd
format_search_results <- function(.query, .parsed, .date) {
  results <- .parsed$results
  header  <- sprintf("Web search results for \"%s\" (searched on %s).",
                     .query, format(.date, "%Y-%m-%d"))

  if (nrow(results) == 0) {
    return(paste(header, "No results were found. Try a different query."))
  }

  blocks <- purrr::map_chr(seq_len(nrow(results)), function(i) {
    lines <- c(
      sprintf("[%d] %s", i, results$title[i]),
      paste("URL:", results$url[i]),
      if (!is.na(results$published[i])) paste("Published:", results$published[i]),
      results$snippet[i],
      if (!is.na(results$text[i])) c("Page text:", results$text[i])
    )
    paste(lines, collapse = "\n")
  })

  answer <- if (!is.null(.parsed$answer) && nzchar(.parsed$answer)) {
    paste("Summary written by the search service:", .parsed$answer)
  }

  paste(c(header, "Cite the URLs of the results you use.", answer, blocks),
        collapse = "\n\n")
}

#' Reduce a publication date to YYYY-MM-DD where it can be read
#'
#' Tavily sends RFC 1123 dates ("Tue, 22 Sep 2026 13:00:00 GMT"). Month names are
#' matched against `month.abb`, which is English in every locale, because
#' `strptime()` with `%b` follows the session locale.
#' @noRd
normalize_published_date <- function(.x) {
  if (is.null(.x) || length(.x) == 0 || is.na(.x) || !nzchar(.x)) return(NA_character_)
  if (grepl("^\\d{4}-\\d{2}-\\d{2}", .x)) return(substr(.x, 1, 10))
  parts <- regmatches(.x, regexec("(\\d{1,2}) ([A-Za-z]{3}) (\\d{4})", .x))[[1]]
  if (length(parts) != 4) return(.x)
  month <- match(tolower(parts[3]), tolower(month.abb))
  if (is.na(month)) return(.x)
  sprintf("%s-%02d-%02d", parts[4], month, as.integer(parts[2]))
}

truncate_text <- function(.x, .max_chars) {
  if (is.na(.x) || nchar(.x) <= .max_chars) return(.x)
  paste0(substr(.x, 1, .max_chars), " [truncated]")
}

tavily_options <- c(
  "search_depth", "chunks_per_source", "topic", "time_range", "start_date",
  "end_date", "include_answer", "include_domains", "exclude_domains",
  "include_domains_mode", "country", "language", "filter_by_language",
  "filter_by_published_date", "auto_parameters", "exact_match", "safe_search"
)

websearch_backends <- list(
  tavily = list(
    key_env = "TAVILY_API_KEY",

    check_options = function(.options) {
      unknown <- setdiff(names(.options), tavily_options)
      if (length(unknown) > 0) {
        stop(sprintf(
          "Unknown Tavily search option(s): %s. Supported options: %s",
          paste(unknown, collapse = ", "), paste(tavily_options, collapse = ", ")
        ))
      }
      enum_ok <- function(value, allowed) {
        is.null(value) || (is.character(value) && length(value) == 1 && value %in% allowed)
      }
      c(
        "search_depth must be one of \"basic\", \"advanced\", \"fast\" or \"ultra-fast\"" =
          enum_ok(.options$search_depth, c("basic", "advanced", "fast", "ultra-fast")),
        "topic must be one of \"general\", \"news\" or \"finance\"" =
          enum_ok(.options$topic, c("general", "news", "finance")),
        "time_range must be one of \"day\", \"week\", \"month\" or \"year\"" =
          enum_ok(.options$time_range, c("day", "week", "month", "year", "d", "w", "m", "y"))
      ) |> validate_inputs()
    },

    build_request = function(.query, .settings) {
      body <- c(
        list(
          query                  = .query,
          max_results            = .settings$max_results,
          include_published_date = TRUE,
          include_raw_content    = if (.settings$include_content) "markdown" else FALSE
        ),
        .settings$options
      )
      if (!is.null(body$include_domains)) body$include_domains <- as.list(body$include_domains)
      if (!is.null(body$exclude_domains)) body$exclude_domains <- as.list(body$exclude_domains)

      httr2::request("https://api.tavily.com/search") |>
        httr2::req_auth_bearer_token(Sys.getenv("TAVILY_API_KEY")) |>
        httr2::req_body_json(body)
    },

    error_message = function(.response) {
      body <- tryCatch(httr2::resp_body_json(.response), error = function(e) NULL)
      detail <- body$detail
      if (is.list(detail) && !is.null(detail$error)) return(detail$error)
      if (is.list(detail) && length(detail) > 0 && !is.null(detail[[1]]$msg)) {
        return(paste(purrr::map_chr(detail, ~ .x$msg %||% ""), collapse = "; "))
      }
      httr2::resp_status_desc(.response) %||% "unknown error"
    },

    parse_response = function(.body, .settings) {
      results <- .body$results %||% list()
      field <- function(name) purrr::map_chr(results, ~ .x[[name]] %||% "")
      text <- purrr::map_chr(results, function(r) {
        if (.settings$include_content && !is.null(r$raw_content)) {
          truncate_text(r$raw_content, .settings$max_chars)
        } else {
          NA_character_
        }
      })
      list(
        answer  = .body$answer,
        results = tibble::tibble(
          title     = field("title"),
          url       = field("url"),
          published = purrr::map_chr(results, ~ normalize_published_date(.x$published_date)),
          snippet   = field("content"),
          text      = text
        )
      )
    }
  )
)

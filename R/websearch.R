#' Search the Web
#'
#' @description
#' Runs one web search and returns the results as a tibble. Use it to collect
#' search results as data, for example sources for a list of companies, or to see
#' exactly what a model would receive from [websearch_tool()], which takes the same
#' arguments.
#'
#' @param .query The search query, a single string of at least two characters.
#' @param .backend The search service to use. Currently only `"tavily"`, which
#'   needs a `TAVILY_API_KEY` environment variable. Tavily's free plan gives
#'   1,000 credits per month without a credit card; one basic search costs one
#'   credit.
#' @param .max_results How many results a search returns, between 1 and 20.
#' @param .include_content If `TRUE`, each result also carries the text of the
#'   page, cut to `.max_chars` characters.
#' @param .max_chars The maximum number of characters of page text per result
#'   when `.include_content = TRUE`. Use `Inf` to keep the whole page.
#' @param .timeout Seconds to wait for the search service before giving up.
#' @param ... Further search options passed to the search service by name. For
#'   Tavily these include `search_depth` (`"basic"`, `"advanced"`, `"fast"` or
#'   `"ultra-fast"`; `"advanced"` costs two credits), `topic` (`"general"`,
#'   `"news"` or `"finance"`), `time_range` (`"day"`, `"week"`, `"month"` or
#'   `"year"`), `start_date` and `end_date` (`"YYYY-MM-DD"`), `include_domains`,
#'   `exclude_domains`, `country` and `include_answer`.
#'
#' @return A tibble with one row per result and the columns `query`, `title`,
#'   `url`, `published` (a date, `NA` where the service does not know it),
#'   `snippet` and `text` (`NA` unless `.include_content = TRUE`). If you ask
#'   Tavily for a summary with `include_answer = TRUE`, it is attached as the
#'   attribute `"answer"`.
#'
#' @details
#' A failed search, for example with a wrong key or used-up credits, stops with
#' an error. To search for many queries, map over them and bind the results; the
#' `query` column keeps them apart.
#'
#' @examples
#' \dontrun{
#' websearch("tidyllm R package")
#'
#' websearch("EU AI Act", .max_results = 10, topic = "news", time_range = "month")
#'
#' c("ZEW Mannheim", "ifo Institut") |>
#'   purrr::map(websearch, .max_results = 3) |>
#'   purrr::list_rbind()
#' }
#'
#' @export
websearch <- function(.query,
                      .backend = c("tavily"),
                      .max_results = 5,
                      .include_content = FALSE,
                      .max_chars = 4000,
                      .timeout = 30,
                      ...) {
  setup  <- websearch_setup(match.arg(.backend), .max_results, .include_content,
                            .max_chars, .timeout, list(...))
  parsed <- websearch_perform(setup$backend, .query, setup$settings)

  results <- tibble::tibble(
    query     = rep(.query, nrow(parsed$results)),
    title     = parsed$results$title,
    url       = parsed$results$url,
    published = as.Date(parsed$results$published, format = "%Y-%m-%d"),
    snippet   = parsed$results$snippet,
    text      = parsed$results$text
  )
  if (!is.null(parsed$answer)) attr(results, "answer") <- parsed$answer
  results
}

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
#' The arguments are the same as for [websearch()], which runs a single search
#' directly and returns the results as a tibble.
#'
#' @inheritParams websearch
#' @param .max_results How many results each search returns, between 1 and 20.
#' @param .include_content If `TRUE`, each result also carries the text of the
#'   page, cut to `.max_chars` characters. This helps with questions a short
#'   excerpt cannot answer, but it makes every tool result much longer.
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
  setup <- websearch_setup(match.arg(.backend), .max_results, .include_content,
                           .max_chars, .timeout, list(...))

  search <- function(query, ...) {
    tryCatch(
      format_search_results(query, websearch_perform(setup$backend, query, setup$settings), Sys.Date()),
      error = function(e) conditionMessage(e)
    )
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

websearch_setup <- function(.backend, .max_results, .include_content, .max_chars,
                            .timeout, .options) {
  backend <- websearch_backends[[.backend]]

  is_number <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x)

  c(
    ".max_results must be a whole number between 1 and 20" =
      is_number(.max_results) && .max_results >= 1 && .max_results <= 20 &&
      is_integer_valued(.max_results),
    ".include_content must be TRUE or FALSE" =
      is.logical(.include_content) && length(.include_content) == 1 && !is.na(.include_content),
    ".max_chars must be a whole number of at least 1, or Inf for no limit" =
      is_number(.max_chars) && .max_chars >= 1 &&
      (is.infinite(.max_chars) || is_integer_valued(.max_chars)),
    ".timeout must be a positive number of seconds" =
      is_number(.timeout) && .timeout > 0,
    "Search options in ... must all be named" =
      length(.options) == 0 || (!is.null(names(.options)) && all(nzchar(names(.options))))
  ) |> validate_inputs()

  backend$check_options(.options)

  if (!nzchar(Sys.getenv(backend$key_env))) {
    stop(sprintf(
      "%s is not set. Please set it with: Sys.setenv(%s = \"YOUR-KEY-GOES-HERE\")",
      backend$key_env, backend$key_env
    ))
  }

  list(
    backend  = backend,
    settings = list(
      max_results     = as.integer(.max_results),
      include_content = .include_content,
      max_chars       = .max_chars,
      timeout         = .timeout,
      options         = .options
    )
  )
}

websearch_perform <- function(.backend, .query, .settings) {
  if (!is.character(.query) || length(.query) != 1 || is.na(.query) || nchar(trimws(.query)) < 2) {
    stop("Web search failed: the query must be a single string of at least two characters.",
         call. = FALSE)
  }

  response <- tryCatch(
    .backend$build_request(.query, .settings) |>
      httr2::req_timeout(.settings$timeout) |>
      httr2::req_retry(
        max_tries    = 3,
        is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503),
        max_seconds  = 60
      ) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) stop(paste("Web search failed:", conditionMessage(e)), call. = FALSE)
  )

  if (httr2::resp_status(response) >= 400) {
    stop(sprintf("Web search failed (HTTP %d): %s",
                 httr2::resp_status(response),
                 .backend$error_message(response)),
         call. = FALSE)
  }

  body <- tryCatch(
    httr2::resp_body_json(response),
    error = function(e) {
      stop(sprintf("Web search failed: expected a JSON reply, got %s.",
                   httr2::resp_content_type(response) %||% "an unreadable reply"),
           call. = FALSE)
    }
  )

  .backend$parse_response(body, .settings)
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
      detail <- if (is.list(body)) body$detail
      if (is.character(detail) && length(detail) == 1) return(detail)
      if (is.list(detail) && is.character(detail$error)) return(detail$error)
      if (is.list(detail) && is.null(names(detail)) && length(detail) > 0 &&
          all(purrr::map_lgl(detail, ~ is.list(.x) && is.character(.x$msg)))) {
        return(paste(purrr::map_chr(detail, "msg"), collapse = "; "))
      }
      status <- httr2::resp_status_desc(.response)
      if (is.null(status) || is.na(status)) "unknown error" else status
    },

    parse_response = function(.body, .settings) {
      results <- .body$results %||% list()
      field <- function(name) purrr::map_chr(results, ~ as.character(.x[[name]] %||% "")[1])
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

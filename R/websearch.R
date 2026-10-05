#' Search the Web
#'
#' @description
#' Runs one web search and returns the results as a tibble. Use it to collect
#' search results as data, for example sources for a list of companies, or to see
#' exactly what a model would receive from [websearch_tool()], which takes the same
#' arguments.
#'
#' @param .query The search query, a single string of at least two characters.
#' @param .backend The search service to use: `"tavily"` (the default) or
#'   `"searxng"`. See the section on search services below.
#' @param .server The address of your SearXNG server, such as
#'   `"http://localhost:8888"`. If `NULL`, the `SEARXNG_SERVER` environment
#'   variable is used. Only for `.backend = "searxng"`.
#' @param .max_results The most results a search returns, between 1 and 20.
#'   SearXNG returns one page of results, which may hold fewer.
#' @param .include_content If `TRUE`, each result also carries the text of the
#'   page, cut to `.max_chars` characters. Only Tavily can do this.
#' @param .max_chars The maximum number of characters of page text per result
#'   when `.include_content = TRUE`. Use `Inf` to keep the whole page.
#' @param .timeout Seconds to wait for each request to the search service.
#'   Busy or failing services are asked up to three times, for at most a
#'   minute in total.
#' @param ... Further search options passed to the search service by name. The
#'   options each service accepts are listed in the section on search services
#'   below; a misspelled option is an error.
#'
#' @return A tibble with one row per result and the columns `query`, `title`,
#'   `url`, `published` (a date, `NA` where the service does not know it),
#'   `snippet` and `text` (`NA` unless `.include_content = TRUE`). If the
#'   service writes a short summary (Tavily with `include_answer = TRUE`, or a
#'   SearXNG instant answer), it is attached as the attribute `"answer"`. If
#'   some SearXNG engines did not answer, their names and reasons are attached
#'   as the attribute `"unresponsive_engines"`.
#'
#' @details
#' A failed search, for example with a wrong Tavily key, used-up credits or an
#' unreachable SearXNG server, stops with an error. To search for many queries, map over them and bind the results; the
#' `query` column keeps them apart.
#'
#' @section Search services:
#' **Tavily** is a paid search API with a free plan of 1,000 credits per month,
#' no credit card needed; one basic search costs one credit. It needs a
#' `TAVILY_API_KEY` environment variable. Options: `search_depth` (`"basic"`,
#' `"advanced"`, `"fast"` or `"ultra-fast"`; `"advanced"` costs two credits),
#' `topic` (`"general"`, `"news"` or `"finance"`), `time_range` (`"day"`,
#' `"week"`, `"month"` or `"year"`, or the short forms `"d"`, `"w"`, `"m"` and
#' `"y"`), `start_date` and `end_date`
#' (`"YYYY-MM-DD"`), `include_domains`, `exclude_domains`, `country`,
#' `include_answer` and others from Tavily's search API.
#'
#' **SearXNG** is a free search engine you run yourself, which collects results
#' from Google, Brave and other engines. Use your own server: public SearXNG
#' servers usually refuse programs. Its `settings.yml` must list `json` under
#' `search: formats:`, and for a server only you use, `server: limiter: false`
#' stops it from blocking repeated searches. Set the address with `.server` or
#' the `SEARXNG_SERVER` environment variable. SearXNG returns no page text.
#' Options: `categories` (such as `"general"` or `"news"`), `engines` (such as
#' `c("google", "brave")`), `language` (such as `"de"`), `time_range` (`"day"`,
#' `"week"`, `"month"` or `"year"`), `safesearch` (`0`, `1` or `2`) and
#' `pageno` (which page of results, for more than one page). A search fails if
#' it finds nothing and at least one engine did not answer, for example because
#' it asked for a CAPTCHA.
#'
#' @examples
#' \dontrun{
#' websearch("tidyllm R package")
#'
#' websearch("EU AI Act", .max_results = 10, topic = "news", time_range = "month")
#'
#' websearch("EU AI Act", .backend = "searxng", .server = "http://localhost:8888",
#'           categories = "news", time_range = "week")
#'
#' c("ZEW Mannheim", "ifo Institut") |>
#'   purrr::map(websearch, .max_results = 3) |>
#'   purrr::list_rbind()
#' }
#'
#' @export
websearch <- function(.query,
                      .backend = c("tavily", "searxng"),
                      .server = NULL,
                      .max_results = 5,
                      .include_content = FALSE,
                      .max_chars = 4000,
                      .timeout = 30,
                      ...) {
  setup  <- websearch_setup(websearch_backends[[match.arg(.backend)]], .server,
                            .max_results, .include_content, .max_chars, .timeout,
                            list(...))
  search_results_tibble(.query, websearch_perform(setup$backend, .query, setup$settings))
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
#'   excerpt cannot answer, but it makes every tool result much longer. Only
#'   Tavily can do this.
#'
#' @return A tool object to pass to the `.tools` argument of `chat()`.
#'
#' @inheritSection websearch Search services
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
#' The tool is named `tidyllm_web_search`, which is the name you will see in
#' the tool calls of a conversation. The Tavily key or SearXNG address is read
#' once, when the tool is created: set it before calling `websearch_tool()`,
#' and create the tool again after changing it. The key is stored inside the
#' tool, so do not save the tool object to a file you share.
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
websearch_tool <- function(.backend = c("tavily", "searxng"),
                           .server = NULL,
                           .max_results = 5,
                           .include_content = FALSE,
                           .max_chars = 4000,
                           .timeout = 30,
                           ...) {
  setup <- websearch_setup(websearch_backends[[match.arg(.backend)]], .server,
                           .max_results, .include_content, .max_chars, .timeout,
                           list(...))

  search <- function(query, ...) {
    tryCatch(
      format_search_results(query, websearch_perform(setup$backend, query, setup$settings), Sys.Date()),
      error = function(e) conditionMessage(e)
    )
  }

  TOOL(
    description = function() {
      paste(
        "Search the web for current information.",
        sprintf("Today's date is %s.", format(Sys.Date(), "%Y-%m-%d")),
        "Returns numbered results with title, URL, publication date where known,",
        "and a text excerpt. Search again with a different query if the results",
        "do not answer the question. Cite the URLs of the results you use."
      )
    },
    input_schema = list(
      query = field_chr("The search query, phrased the way you would type it into a search engine")
    ),
    func    = search,
    name    = "tidyllm_web_search",
    builtin = list()
  )
}

websearch_setup <- function(.backend, .server, .max_results, .include_content,
                            .max_chars, .timeout, .options) {
  is_number <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x)

  c(
    stats::setNames(
      is_number(.max_results) && .max_results >= 1 &&
        .max_results <= .backend$max_results_limit && is_integer_valued(.max_results),
      sprintf(".max_results must be a whole number between 1 and %d", .backend$max_results_limit)
    ),
    ".include_content must be TRUE or FALSE" =
      is.logical(.include_content) && length(.include_content) == 1 && !is.na(.include_content),
    ".max_chars must be a whole number of at least 1, or Inf for no limit" =
      is_number(.max_chars) && .max_chars >= 1 &&
      (is.infinite(.max_chars) || is_integer_valued(.max_chars)),
    ".timeout must be a positive number of seconds" =
      is_number(.timeout) && .timeout > 0,
    ".server must be NULL or a single string" =
      is.null(.server) || (is.character(.server) && length(.server) == 1 && !is.na(.server)),
    "Search options in ... must all be named" =
      length(.options) == 0 || (!is.null(names(.options)) && all(nzchar(names(.options))))
  ) |> validate_inputs()

  if (.include_content && !.backend$supports_content) {
    stop(sprintf("%s does not return page text, so .include_content must be FALSE", .backend$label),
         call. = FALSE)
  }
  check_search_options(.backend, .options)

  list(
    backend  = .backend,
    settings = list(
      max_results     = as.integer(.max_results),
      include_content = .include_content,
      max_chars       = .max_chars,
      timeout         = .timeout,
      options         = .options,
      access          = .backend$resolve(.server)
    )
  )
}

check_search_options <- function(.backend, .options) {
  unknown <- setdiff(names(.options), .backend$options)
  if (length(unknown) > 0) {
    stop(sprintf(
      "Unknown %s search option(s): %s. Supported options: %s",
      .backend$label, paste(unknown, collapse = ", "), paste(.backend$options, collapse = ", ")
    ), call. = FALSE)
  }
  for (name in names(.options)) {
    value <- .options[[name]]
    if (is.null(value) || length(value) == 0 || anyNA(value)) {
      stop(sprintf("Search option %s must not be NULL, empty or NA", name), call. = FALSE)
    }
    if (name %in% .backend$single_value_options && length(value) != 1) {
      stop(sprintf("Search option %s takes a single value", name), call. = FALSE)
    }
  }
  for (name in names(.backend$option_values)) {
    value   <- .options[[name]]
    allowed <- .backend$option_values[[name]]
    if (!is.null(value) && !((is.character(value) || is.numeric(value)) && length(value) == 1 &&
                             as.character(value) %in% allowed)) {
      stop(sprintf("%s must be one of %s", name, or_list(sprintf("\"%s\"", allowed))),
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

or_list <- function(.x) {
  if (length(.x) == 1) return(.x)
  paste(paste(utils::head(.x, -1), collapse = ", "), "or", utils::tail(.x, 1))
}

websearch_perform <- function(.backend, .query, .settings) {
  if (!is.character(.query) || length(.query) != 1 || is.na(.query) || nchar(trimws(.query)) < 2) {
    stop("Web search failed: the query must be a single string of at least two characters.",
         call. = FALSE)
  }

  response <- tryCatch(
    search_request(.backend, .query, .settings) |> httr2::req_perform(),
    error = function(e) stop(paste("Web search failed:", conditionMessage(e)), call. = FALSE)
  )

  body   <- read_search_response(.backend, response)
  parsed <- tryCatch(
    .backend$parse_response(body, .settings),
    error = function(e) stop("Web search failed: the reply had an unexpected shape.", call. = FALSE)
  )
  finish_search_results(parsed, .settings)
}

search_request <- function(.backend, .query, .settings) {
  .backend$build_request(.query, .settings) |>
    httr2::req_user_agent(sprintf("tidyllm/%s", utils::packageVersion("tidyllm"))) |>
    httr2::req_timeout(.settings$timeout) |>
    httr2::req_retry(
      max_tries    = 3,
      is_transient = function(resp) httr2::resp_status(resp) %in% .backend$retry_statuses,
      max_seconds  = 60
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)
}

search_failure <- function(.message) {
  stop(paste0("Web search failed", .message), call. = FALSE)
}

read_search_response <- function(.backend, .response) {
  status <- httr2::resp_status(.response)
  if (status >= 400) {
    search_failure(sprintf(" (HTTP %d): %s", status, .backend$error_message(.response)))
  }
  content_type <- httr2::resp_content_type(.response)
  if (is.na(content_type) || !grepl("^application/([^/]+\\+)?json$", content_type)) {
    search_failure(sprintf(": expected a JSON reply, got %s.",
                           if (is.na(content_type)) "a reply without a content type" else content_type))
  }
  tryCatch(
    httr2::resp_body_json(.response),
    error = function(e) search_failure(": the JSON reply could not be read.")
  )
}

finish_search_results <- function(.parsed, .settings) {
  results <- .parsed$results
  if (nrow(results) == 0 && length(.parsed$unresponsive) > 0) {
    stop(sprintf("Web search failed: no results, and these search engines did not answer: %s. Try again or rephrase the query.",
                 paste(.parsed$unresponsive, collapse = ", ")),
         call. = FALSE)
  }
  results <- results[seq_len(min(nrow(results), .settings$max_results)), ]
  results$published <- purrr::map_chr(results$published, normalize_published_date)
  results$text <- if (isTRUE(.settings$include_content)) {
    purrr::map_chr(results$text, ~ truncate_text(.x, .settings$max_chars))
  } else {
    rep(NA_character_, nrow(results))
  }
  .parsed$results <- results
  .parsed
}

search_results_tibble <- function(.query, .parsed) {
  results <- tibble::tibble(
    query     = rep(.query, nrow(.parsed$results)),
    title     = .parsed$results$title,
    url       = .parsed$results$url,
    published = as.Date(.parsed$results$published, format = "%Y-%m-%d"),
    snippet   = .parsed$results$snippet,
    text      = .parsed$results$text
  )
  if (!is.null(.parsed$answer)) attr(results, "answer") <- .parsed$answer
  if (length(.parsed$unresponsive) > 0) attr(results, "unresponsive_engines") <- .parsed$unresponsive
  results
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

#' Search services behind websearch() and websearch_tool()
#'
#' Each entry describes one service. Data fields: `label` (the name used in
#' messages), `max_results_limit`, `supports_content` (can it return page
#' text), `options` (the names allowed in `...`), `single_value_options`,
#' `option_values` (allowed values for enumerated options) and
#' `retry_statuses` (HTTP statuses retried as transient).
#'
#' Functions, called in this order:
#' - `resolve(.server)` runs once at setup and returns the `access` list
#'   (at least `server`; a key if the service needs one). It stops on missing
#'   configuration, so errors surface when a tool is created.
#' - `build_request(.query, .settings)` returns an httr2 request; `.settings`
#'   holds `max_results`, `include_content`, `max_chars`, `timeout`, `options`
#'   and `access`.
#' - `error_message(.response)` turns a reply with status 400 or above into
#'   one line of text.
#' - `parse_response(.body, .settings)` returns `list(answer, unresponsive,
#'   results)`: `answer` a string or `NULL`, `unresponsive` a character vector
#'   of failed engines (may be empty), `results` a tibble of character columns
#'   `title`, `url`, `published` (as the service sends it) and `snippet`, and
#'   `text` (the full page text, or `NA`). `finish_search_results()` then cuts
#'   to `max_results`, normalises dates and truncates or drops page text.
#' @noRd
websearch_backends <- list(
  tavily = list(
    label             = "Tavily",
    max_results_limit = 20,
    supports_content  = TRUE,
    options = c(
      "search_depth", "chunks_per_source", "topic", "time_range", "start_date",
      "end_date", "include_answer", "include_domains", "exclude_domains",
      "include_domains_mode", "country", "language", "filter_by_language",
      "filter_by_published_date", "auto_parameters", "exact_match", "safe_search"
    ),
    single_value_options = c("search_depth", "chunks_per_source", "topic", "time_range",
                             "start_date", "end_date", "country"),
    option_values = list(
      search_depth = c("basic", "advanced", "fast", "ultra-fast"),
      topic        = c("general", "news", "finance"),
      time_range   = c("day", "week", "month", "year", "d", "w", "m", "y")
    ),
    retry_statuses = c(429, 500, 502, 503),

    resolve = function(.server) {
      if (!is.null(.server)) {
        stop(".server only applies to a self-hosted search service such as SearXNG; Tavily has a fixed address",
             call. = FALSE)
      }
      key <- Sys.getenv("TAVILY_API_KEY")
      if (!nzchar(key)) {
        stop("TAVILY_API_KEY is not set. Please set it with: Sys.setenv(TAVILY_API_KEY = \"YOUR-KEY-GOES-HERE\")",
             call. = FALSE)
      }
      list(server = "https://api.tavily.com", key = key)
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

      httr2::request(.settings$access$server) |>
        httr2::req_url_path_append("search") |>
        httr2::req_auth_bearer_token(.settings$access$key) |>
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
      http_status_text(.response)
    },

    parse_response = function(.body, .settings) {
      results <- .body$results %||% list()
      list(
        answer  = .body$answer,
        results = tibble::tibble(
          title     = result_field(results, "title"),
          url       = result_field(results, "url"),
          published = result_field(results, "published_date", NA_character_),
          snippet   = result_field(results, "content"),
          text      = result_field(results, "raw_content", NA_character_)
        )
      )
    }
  ),

  searxng = list(
    label                = "SearXNG",
    max_results_limit    = 20,
    supports_content     = FALSE,
    options              = c("categories", "engines", "language", "time_range", "safesearch", "pageno"),
    single_value_options = c("language", "time_range", "safesearch", "pageno"),
    option_values        = list(
      time_range = c("day", "week", "month", "year"),
      safesearch = c("0", "1", "2")
    ),
    retry_statuses = c(500, 502, 503),

    resolve = function(.server) {
      server <- .server %||% Sys.getenv("SEARXNG_SERVER")
      if (!nzchar(server)) {
        stop("No SearXNG server given. Pass .server = \"http://localhost:8888\" or set it with: Sys.setenv(SEARXNG_SERVER = \"http://localhost:8888\")",
             call. = FALSE)
      }
      if (!grepl("^https?://", server)) {
        stop(sprintf("The SearXNG server address must start with http:// or https://, for example \"http://%s\"", server),
             call. = FALSE)
      }
      list(server = sub("/+$", "", server))
    },

    build_request = function(.query, .settings) {
      options <- purrr::map(.settings$options, ~ paste(.x, collapse = ","))
      httr2::request(.settings$access$server) |>
        httr2::req_url_path_append("search") |>
        httr2::req_url_query(q = .query, format = "json", !!!options)
    },

    error_message = function(.response) {
      status <- httr2::resp_status(.response)
      if (status == 403) {
        return("SearXNG refused JSON output. Add json under search: formats: in its settings.yml")
      }
      if (status == 429) {
        return("SearXNG's limiter blocked the request. For a server only you use, set server: limiter: false in its settings.yml")
      }
      body <- tryCatch(httr2::resp_body_json(.response), error = function(e) NULL)
      if (is.list(body) && is.character(body$error) && length(body$error) == 1) return(body$error)
      http_status_text(.response)
    },

    parse_response = function(.body, .settings) {
      results <- .body$results %||% list()
      answers <- purrr::map_chr(.body$answers %||% list(), function(a) {
        value <- if (is.list(a)) a$answer else a
        if (length(value) == 0) NA_character_ else as.character(value)[1]
      })
      answers <- answers[!is.na(answers) & nzchar(answers)]
      list(
        answer       = if (length(answers) > 0) paste(answers, collapse = "\n"),
        unresponsive = purrr::map_chr(.body$unresponsive_engines %||% list(), function(e) {
          parts <- as.character(unlist(e))
          if (length(parts) >= 2) sprintf("%s: %s", parts[1], parts[2]) else parts[1]
        }),
        results = tibble::tibble(
          title     = result_field(results, "title"),
          url       = result_field(results, "url"),
          published = result_field(results, "publishedDate", NA_character_),
          snippet   = result_field(results, "content"),
          text      = rep(NA_character_, length(results))
        )
      )
    }
  )
)

result_field <- function(.results, .name, .missing = "") {
  purrr::map_chr(.results, function(r) {
    value <- r[[.name]]
    if (length(value) == 0 || is.list(value)) .missing else as.character(value)[1]
  })
}

http_status_text <- function(.response) {
  status <- httr2::resp_status_desc(.response)
  if (is.null(status) || is.na(status)) "unknown error" else status
}

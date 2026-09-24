with_tavily_key <- function(code) {
  old <- Sys.getenv("TAVILY_API_KEY", unset = NA)
  Sys.setenv(TAVILY_API_KEY = "tvly-test")
  on.exit(if (is.na(old)) Sys.unsetenv("TAVILY_API_KEY") else Sys.setenv(TAVILY_API_KEY = old))
  code
}

test_that("websearch_tool builds a tool that only exposes a query", {
  ws <- with_tavily_key(websearch_tool())
  expect_true(S7::S7_inherits(ws, TOOL))
  expect_equal(ws@name, "tidyllm_web_search")
  expect_named(ws@input_schema, "query")
  expect_equal(names(formals(ws@func)), c("query", "..."))
  expect_match(ws@description, format(Sys.Date(), "%Y-%m-%d"), fixed = TRUE)
})

test_that("websearch_tool checks its arguments when it is created", {
  with_tavily_key({
    expect_error(websearch_tool(.max_results = 0), "between 1 and 20")
    expect_error(websearch_tool(.max_results = 21), "between 1 and 20")
    expect_error(websearch_tool(.include_content = "yes"), "TRUE or FALSE")
    expect_error(websearch_tool(serch_depth = "advanced"), "Unknown Tavily search option")
    expect_error(websearch_tool(topic = "sports"), "topic must be one of")
    expect_error(websearch_tool(time_range = "fortnight"), "time_range must be one of")
    expect_error(websearch_tool("tavily", NULL, 5, FALSE, 4000, 30, "news"), "must all be named")
    expect_no_error(websearch_tool(search_depth = "advanced", topic = "news", time_range = "week"))
  })
})

test_that("websearch_tool needs a Tavily key", {
  old <- Sys.getenv("TAVILY_API_KEY", unset = NA)
  Sys.unsetenv("TAVILY_API_KEY")
  on.exit(if (!is.na(old)) Sys.setenv(TAVILY_API_KEY = old))
  expect_error(websearch_tool(), "TAVILY_API_KEY is not set")
})

test_that("a too-short query returns a message instead of calling the API", {
  ws <- with_tavily_key(websearch_tool())
  expect_match(ws@func(query = "x"), "at least two characters", fixed = TRUE)
})

test_that("websearch() checks the same arguments and stops on a bad query", {
  with_tavily_key({
    expect_error(websearch("tidyllm", .max_results = 21), "between 1 and 20")
    expect_error(websearch("tidyllm", serch_depth = "advanced"), "Unknown Tavily search option")
    expect_error(websearch("x"), "at least two characters")
    expect_error(websearch(c("one query", "two queries")), "single string")
  })
  old <- Sys.getenv("TAVILY_API_KEY", unset = NA)
  Sys.unsetenv("TAVILY_API_KEY")
  on.exit(if (!is.na(old)) Sys.setenv(TAVILY_API_KEY = old))
  expect_error(websearch("tidyllm"), "TAVILY_API_KEY is not set")
})

test_that("numeric arguments reject missing values, fractions and bad limits", {
  with_tavily_key({
    expect_error(websearch_tool(.max_results = NA), "between 1 and 20")
    expect_error(websearch_tool(.max_results = 2.5), "between 1 and 20")
    expect_error(websearch_tool(.max_chars = 0.5), "at least 1")
    expect_error(websearch_tool(.max_chars = NA), "at least 1")
    expect_error(websearch_tool(.timeout = NA), "positive number of seconds")
    expect_error(websearch_tool(.timeout = 0), "positive number of seconds")
    expect_no_error(websearch_tool(.max_chars = Inf))
  })
})

test_that("the tool ignores extra arguments a model sends", {
  ws <- with_tavily_key(websearch_tool())
  expect_match(ws@func(query = "x", max_results = 50), "at least two characters", fixed = TRUE)
})

test_that("Tavily error bodies are turned into a readable message", {
  em <- websearch_backends$tavily$error_message
  reply <- function(code, body, type = "application/json") {
    httr2::response(status_code = code, headers = list(`Content-Type` = type),
                    body = charToRaw(body))
  }
  expect_equal(em(reply(401, '{"detail":{"error":"bad key"}}')), "bad key")
  expect_equal(em(reply(422, '{"detail":[{"msg":"a"},{"msg":"b"}]}')), "a; b")
  expect_equal(em(reply(403, '{"detail":"Forbidden plan"}')), "Forbidden plan")
  expect_equal(em(reply(403, '{"detail":{"message":"x"}}')), "Forbidden")
  expect_equal(em(reply(502, "<html></html>", "text/html")), "Bad Gateway")
  expect_equal(em(reply(599, "x", "text/plain")), "unknown error")
})

test_that("page text is kept only when asked for, and Inf keeps all of it", {
  body <- list(results = list(list(title = "T", url = "https://a.org", content = "S",
                                   raw_content = strrep("x", 50))))
  parsed <- websearch_backends$tavily$parse_response(body, list())
  off <- finish_search_results(parsed, list(max_results = 5L, include_content = FALSE, max_chars = 10))
  expect_equal(off$results$text, NA_character_)
  all <- finish_search_results(parsed, list(max_results = 5L, include_content = TRUE, max_chars = Inf))
  expect_equal(all$results$text, strrep("x", 50))
})

test_that("a non-string or missing field from the service does not break parsing", {
  body <- list(results = list(list(title = 5, url = "https://a.org", content = "S",
                                   published_date = list("a", "b"))))
  parsed <- websearch_backends$tavily$parse_response(body, list())
  expect_equal(parsed$results$title, "5")
  expect_equal(parsed$results$published, NA_character_)
  expect_equal(parsed$results$text, NA_character_)
})

test_that("the answer and page text appear in the text for the model", {
  parsed <- list(
    answer  = "Ans",
    results = tibble::tibble(title = "T", url = "https://a.org", published = NA_character_,
                             snippet = "S", text = "PAGE")
  )
  out <- format_search_results("q", parsed, as.Date("2026-09-24"))
  expect_match(out, "Summary written by the search service: Ans", fixed = TRUE)
  expect_match(out, "Page text:\nPAGE", fixed = TRUE)
  parsed$answer <- ""
  expect_no_match(format_search_results("q", parsed, as.Date("2026-09-24")), "Summary written by", fixed = TRUE)
})

test_that("truncate_text cuts only past the limit", {
  expect_equal(truncate_text("abcde", 5), "abcde")
  expect_equal(truncate_text("abcdef", 5), "abcde [truncated]")
  expect_equal(truncate_text(NA_character_, 5), NA_character_)
  expect_equal(truncate_text(character(0), 5), NA_character_)
})

test_that("publication dates are reduced to YYYY-MM-DD in any locale", {
  expect_equal(normalize_published_date("Tue, 22 Sep 2026 13:00:00 GMT"), "2026-09-22")
  expect_equal(normalize_published_date("Mon, 5 Oct 2025 07:00:00 GMT"), "2025-10-05")
  expect_equal(normalize_published_date("2026-09-08T09:00:02Z"), "2026-09-08")
  expect_equal(normalize_published_date(NULL), NA_character_)
  expect_equal(normalize_published_date(NA), NA_character_)
  expect_equal(normalize_published_date(""), NA_character_)
  expect_equal(normalize_published_date(c("2026-01-01", "2026-01-02")), NA_character_)
  expect_equal(normalize_published_date("sometime last week"), "sometime last week")
  expect_equal(normalize_published_date("5 Foo 2025"), "5 Foo 2025")
})

test_that("Tavily responses are parsed into a result table", {
  body <- list(
    answer  = "A short answer.",
    results = list(
      list(title = "First", url = "https://a.org", content = "Snippet one",
           published_date = "Tue, 22 Sep 2026 13:00:00 GMT",
           raw_content = strrep("x", 50)),
      list(title = "Second", url = "https://b.org", content = "Snippet two")
    )
  )
  parsed <- websearch_backends$tavily$parse_response(body, list())
  expect_equal(parsed$answer, "A short answer.")
  expect_s3_class(parsed$results, "tbl_df")
  expect_named(parsed$results, c("title", "url", "published", "snippet", "text"))

  done <- finish_search_results(parsed, list(max_results = 5L, include_content = TRUE, max_chars = 10L))
  expect_equal(done$results$published, c("2026-09-22", NA))
  expect_equal(done$results$text, c(paste0(strrep("x", 10), " [truncated]"), NA))

  empty <- websearch_backends$tavily$parse_response(list(results = list()), list())
  expect_equal(nrow(empty$results), 0)
})

test_that("search results are formatted as numbered text for the model", {
  parsed <- list(
    answer  = NULL,
    results = tibble::tibble(
      title = c("First", "Second"), url = c("https://a.org", "https://b.org"),
      published = c("2026-09-22", NA), snippet = c("Snippet one", "Snippet two"),
      text = c(NA_character_, NA_character_)
    )
  )
  out <- format_search_results("my query", parsed, as.Date("2026-09-24"))

  expect_match(out, "Web search results for \"my query\" (searched on 2026-09-24).", fixed = TRUE)
  expect_match(out, "[1] First\nURL: https://a.org\nPublished: 2026-09-22\nSnippet one", fixed = TRUE)
  expect_match(out, "[2] Second\nURL: https://b.org\nSnippet two", fixed = TRUE)
  expect_no_match(out, "Summary written by", fixed = TRUE)

  none <- format_search_results("q", list(answer = NULL, results = parsed$results[0, ]),
                                as.Date("2026-09-24"))
  expect_match(none, "No results were found", fixed = TRUE)
})

test_that("the Tavily request carries the fixed settings and the options", {
  settings <- list(max_results = 3L, include_content = FALSE, max_chars = 4000L,
                   timeout = 30, options = list(topic = "news", include_domains = "cran.r-project.org"),
                   access = list(server = "https://api.tavily.com", key = "tvly-test"))
  req  <- websearch_backends$tavily$build_request("some query", settings)
  body <- httr2::req_get_body(req)

  expect_equal(req$url, "https://api.tavily.com/search")
  expect_equal(body$query, "some query")
  expect_equal(body$max_results, 3L)
  expect_true(body$include_published_date)
  expect_false(body$include_raw_content)
  expect_equal(body$topic, "news")
  expect_equal(body$include_domains, list("cran.r-project.org"))

  settings$options <- list(include_domains = c("cran.r-project.org", "github.com"),
                           exclude_domains = c("a.org", "b.org"))
  body <- httr2::req_get_body(websearch_backends$tavily$build_request("some query", settings))
  expect_equal(body$include_domains, list("cran.r-project.org", "github.com"))
  expect_equal(body$exclude_domains, list("a.org", "b.org"))
})

test_that("the Tavily key is read once, when the search is set up", {
  setup <- with_tavily_key(websearch_setup(websearch_backends$tavily, NULL, 5, FALSE, 4000, 30, list()))
  expect_equal(setup$settings$access$key, "tvly-test")
  req <- websearch_backends$tavily$build_request("some query", setup$settings)
  expect_equal(httr2::req_get_headers(req, "reveal")$Authorization, "Bearer tvly-test")
})

test_that("each backend's limits and options are checked from its description", {
  toy <- list(
    label             = "Toy",
    max_results_limit = 10,
    supports_content  = FALSE,
    options           = c("language", "time_range"),
    option_values     = list(time_range = c("day", "month", "year")),
    resolve           = function(.server) list(server = "http://localhost:1")
  )
  expect_error(websearch_setup(toy, NULL, 11, FALSE, 4000, 30, list()), "between 1 and 10")
  expect_error(websearch_setup(toy, NULL, 5, TRUE, 4000, 30, list()),
               "Toy does not return page text")
  expect_error(websearch_setup(toy, NULL, 5, FALSE, 4000, 30, list(safesearch = 1)),
               "Unknown Toy search option(s): safesearch", fixed = TRUE)
  expect_error(websearch_setup(toy, NULL, 5, FALSE, 4000, 30, list(time_range = "week")),
               "time_range must be one of \"day\", \"month\" or \"year\"", fixed = TRUE)
  setup <- websearch_setup(toy, NULL, 5, FALSE, 4000, 30, list(language = "de", time_range = "day"))
  expect_equal(setup$settings$access$server, "http://localhost:1")
})

with_searxng_server <- function(value, code) {
  old <- Sys.getenv("SEARXNG_SERVER", unset = NA)
  if (is.na(value)) Sys.unsetenv("SEARXNG_SERVER") else Sys.setenv(SEARXNG_SERVER = value)
  on.exit(if (is.na(old)) Sys.unsetenv("SEARXNG_SERVER") else Sys.setenv(SEARXNG_SERVER = old))
  code
}

test_that("the SearXNG server comes from .server or SEARXNG_SERVER", {
  resolve <- websearch_backends$searxng$resolve
  with_searxng_server("http://env.example:8888/", {
    expect_equal(resolve(NULL)$server, "http://env.example:8888")
    expect_equal(resolve("http://arg.example:8080")$server, "http://arg.example:8080")
  })
  with_searxng_server(NA, {
    expect_error(websearch_tool("searxng"), "No SearXNG server given")
    expect_no_error(websearch_tool("searxng", .server = "http://localhost:8888"))
  })
  with_tavily_key(expect_error(websearch_tool(.server = "http://localhost:8888"),
                               "only applies to a self-hosted"))
  expect_error(websearch_tool("searxng", .server = c("a", "b")), ".server must be NULL")
  expect_error(websearch_tool("searxng", .server = "localhost:8888"), "must start with http")
  req <- websearch_backends$searxng$build_request(
    "q", list(options = list(), access = resolve("http://h.example/searx/"))
  )
  expect_equal(httr2::url_parse(req$url)$path, "/searx/search")
})

test_that("SearXNG options are checked when the tool is created", {
  with_searxng_server("http://localhost:8888", {
    expect_error(websearch_tool("searxng", .include_content = TRUE), "SearXNG does not return page text")
    expect_error(websearch_tool("searxng", topic = "news"), "Unknown SearXNG search option")
    expect_error(websearch_tool("searxng", time_range = "fortnight"), "time_range must be one of")
    expect_error(websearch_tool("searxng", safesearch = 5), "safesearch must be one of")
    expect_error(websearch_tool("searxng", language = c("de", "en")), "takes a single value")
    expect_error(websearch_tool("searxng", pageno = NA), "must not be NULL, empty or NA")
    expect_error(websearch_tool("searxng", categories = NULL), "must not be NULL, empty or NA")
    expect_error(websearch_tool("searxng", format = "html"), "Unknown SearXNG search option")
    expect_no_error(websearch_tool("searxng", safesearch = 1, time_range = "week",
                                   engines = c("google", "brave"), language = "de"))
  })
})

test_that("the SearXNG request is a GET with the query, JSON format and options", {
  settings <- list(max_results = 3L, include_content = FALSE, timeout = 30,
                   options = list(engines = c("google", "brave"), safesearch = 1),
                   access = list(server = "http://localhost:8888"))
  req <- websearch_backends$searxng$build_request("tidyllm R", settings)
  url <- httr2::url_parse(req$url)
  expect_equal(url$path, "/search")
  expect_equal(url$query$q, "tidyllm R")
  expect_equal(url$query$format, "json")
  expect_equal(url$query$engines, "google,brave")
  expect_equal(url$query$safesearch, "1")
  expect_null(httr2::req_get_body(req))
})

test_that("SearXNG replies are parsed into a result table", {
  body <- list(
    results = list(
      list(title = "First", url = "https://a.org", content = "Snippet one",
           publishedDate = "2026-09-16T19:49:54"),
      list(title = "Second", url = "https://b.org", content = "Snippet two", publishedDate = NULL)
    ),
    answers = list(list(answer = "83 million", url = "https://c.org"), "plain answer",
                   NULL, list(answer = NULL)),
    unresponsive_engines = list(list("duckduckgo", "CAPTCHA"), list("bing"))
  )
  parsed <- websearch_backends$searxng$parse_response(body, list())
  expect_named(parsed$results, c("title", "url", "published", "snippet", "text"))
  expect_equal(parsed$answer, "83 million\nplain answer")
  expect_equal(parsed$unresponsive, c("duckduckgo: CAPTCHA", "bing"))
  done <- finish_search_results(parsed, list(max_results = 5L, include_content = FALSE))
  expect_equal(done$results$published, c("2026-09-16", NA))
  expect_equal(done$results$text, c(NA_character_, NA_character_))

  empty <- websearch_backends$searxng$parse_response(list(results = list(), answers = list()), list())
  expect_null(empty$answer)
  expect_length(empty$unresponsive, 0)
})

test_that("an empty result fails only when engines failed, and results are cut to max_results", {
  settings <- list(max_results = 3L, include_content = FALSE)
  none <- tibble::tibble(title = character(), url = character(), published = character(),
                         snippet = character(), text = character())
  expect_error(finish_search_results(list(results = none, unresponsive = "duckduckgo: CAPTCHA"), settings),
               "these search engines did not answer: duckduckgo: CAPTCHA", fixed = TRUE)
  expect_equal(nrow(finish_search_results(list(results = none, unresponsive = character()), settings)$results), 0)
  many <- tibble::tibble(title = letters, url = letters, published = NA_character_,
                         snippet = letters, text = NA_character_)
  expect_equal(finish_search_results(list(results = many), settings)$results$title, c("a", "b", "c"))
})

test_that("SearXNG errors explain the settings that cause them", {
  em <- websearch_backends$searxng$error_message
  reply <- function(code, body, type = "application/json") {
    httr2::response(status_code = code, headers = list(`Content-Type` = type), body = charToRaw(body))
  }
  expect_match(em(reply(403, "<html>Forbidden</html>", "text/html")), "search: formats:", fixed = TRUE)
  expect_match(em(reply(429, "Too Many Requests", "text/plain")), "limiter: false", fixed = TRUE)
  expect_equal(em(reply(400, '{"error": "Invalid value 5 for parameter safesearch"}')),
               "Invalid value 5 for parameter safesearch")
  expect_equal(em(reply(502, "<html></html>", "text/html")), "Bad Gateway")
})

test_that("options in ... cannot override the fixed request fields", {
  with_tavily_key({
    expect_error(websearch_tool(max_results = 50), "Unknown Tavily search option")
    expect_error(websearch_tool(include_raw_content = TRUE), "Unknown Tavily search option")
    expect_error(websearch_tool(time_range = c("day", "week")), "takes a single value")
    expect_no_error(websearch_tool(time_range = "d"))
  })
})

test_that("the Tavily request asks for page text only when .include_content is TRUE", {
  settings <- list(max_results = 3L, include_content = TRUE, options = list(),
                   access = list(server = "https://api.tavily.com", key = "tvly-test"))
  body <- httr2::req_get_body(websearch_backends$tavily$build_request("q", settings))
  expect_equal(body$include_raw_content, "markdown")
})

test_that("replies are checked for status, content type and readable JSON", {
  tavily <- websearch_backends$tavily
  reply <- function(code, body, type = "application/json") {
    httr2::response(status_code = code, headers = list(`Content-Type` = type), body = charToRaw(body))
  }
  expect_error(read_search_response(tavily, reply(500, '{"detail":"down"}')),
               "Web search failed (HTTP 500): down", fixed = TRUE)
  expect_error(read_search_response(tavily, reply(200, "<html></html>", "text/html")),
               "Web search failed: expected a JSON reply, got text/html.", fixed = TRUE)
  expect_error(read_search_response(tavily, httr2::response(200, body = charToRaw("x"))),
               "a reply without a content type", fixed = TRUE)
  expect_error(read_search_response(tavily, reply(200, "{not json")),
               "the JSON reply could not be read", fixed = TRUE)
  expect_equal(read_search_response(tavily, reply(200, '{"results":[]}', "application/vnd.api+json")),
               list(results = list()))
})

test_that("a failure while building or parsing keeps the Web search failed prefix", {
  broken <- websearch_backends$searxng
  broken$build_request <- function(...) stop("boom")
  settings <- list(timeout = 1, max_results = 1L, include_content = FALSE, options = list())
  expect_error(websearch_perform(broken, "query", settings), "Web search failed: boom", fixed = TRUE)
})

test_that("each backend retries only statuses that can pass", {
  expect_true(429 %in% websearch_backends$tavily$retry_statuses)
  expect_false(429 %in% websearch_backends$searxng$retry_statuses)
  settings <- list(timeout = 7, options = list(), access = list(server = "http://localhost:8888"))
  req <- search_request(websearch_backends$searxng, "q", settings)
  expect_equal(req$policies$retry_max_tries, 3)
  expect_match(req$options$useragent, "^tidyllm/")
})

test_that("websearch() turns parsed results into a tibble with its attributes", {
  parsed <- list(
    answer = "Ans", unresponsive = "duckduckgo: CAPTCHA",
    results = tibble::tibble(title = "T", url = "https://a.org", published = "2026-09-22",
                             snippet = "S", text = NA_character_)
  )
  r <- search_results_tibble("q", parsed)
  expect_named(r, c("query", "title", "url", "published", "snippet", "text"))
  expect_s3_class(r$published, "Date")
  expect_equal(attr(r, "answer"), "Ans")
  expect_equal(attr(r, "unresponsive_engines"), "duckduckgo: CAPTCHA")
  plain <- search_results_tibble("q", list(results = parsed$results))
  expect_null(attr(plain, "answer"))
  expect_null(attr(plain, "unresponsive_engines"))
})

test_that("option errors name the option, not an internal call", {
  with_tavily_key({
    err <- tryCatch(websearch_tool(topic = "sports"), error = function(e) e)
    expect_null(conditionCall(err))
    expect_equal(conditionMessage(err), "topic must be one of \"general\", \"news\" or \"finance\"")
  })
  expect_equal(or_list("\"a\""), "\"a\"")
})

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
  expect_equal(names(formals(ws@func)), "query")
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
    expect_error(websearch_tool("tavily", 5, FALSE, 4000, 30, "news"), "must all be named")
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
  expect_match(ws@func(query = "x"), "^Web search failed")
})

test_that("publication dates are reduced to YYYY-MM-DD in any locale", {
  expect_equal(normalize_published_date("Tue, 22 Sep 2026 13:00:00 GMT"), "2026-09-22")
  expect_equal(normalize_published_date("Mon, 5 Oct 2025 07:00:00 GMT"), "2025-10-05")
  expect_equal(normalize_published_date("2026-09-08T09:00:02Z"), "2026-09-08")
  expect_equal(normalize_published_date(NULL), NA_character_)
  expect_equal(normalize_published_date(""), NA_character_)
  expect_equal(normalize_published_date("sometime last week"), "sometime last week")
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
  settings <- list(include_content = TRUE, max_chars = 10L)
  parsed <- websearch_backends$tavily$parse_response(body, settings)

  expect_equal(parsed$answer, "A short answer.")
  expect_s3_class(parsed$results, "tbl_df")
  expect_named(parsed$results, c("title", "url", "published", "snippet", "text"))
  expect_equal(parsed$results$published, c("2026-09-22", NA))
  expect_equal(parsed$results$text, c(paste0(strrep("x", 10), " [truncated]"), NA))

  empty <- websearch_backends$tavily$parse_response(list(results = list()), settings)
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
                   timeout = 30, options = list(topic = "news", include_domains = "cran.r-project.org"))
  req <- with_tavily_key(websearch_backends$tavily$build_request("some query", settings))
  body <- req$body$data

  expect_equal(req$url, "https://api.tavily.com/search")
  expect_equal(body$query, "some query")
  expect_equal(body$max_results, 3L)
  expect_true(body$include_published_date)
  expect_false(body$include_raw_content)
  expect_equal(body$topic, "news")
  expect_equal(body$include_domains, list("cran.r-project.org"))
})

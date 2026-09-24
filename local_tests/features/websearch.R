# local_tests/features/websearch.R
# Live tests for websearch() and websearch_tool() with the Tavily backend.
#
# What this tests:
#   - A direct search returns numbered results with URLs and readable dates
#   - Tavily options passed through ... reach the API (a bad value is rejected)
#   - A timeout comes back as text for the model
#   - .include_content cuts page text to .max_chars
#   - A failing search comes back as text for the model instead of an error
#   - websearch() returns the same results as a tibble, with dates and an answer
#   - The same tool object drives a cited answer in claude(), openai() and gemini()
#   - SearXNG: results cut to .max_results, options reach the server, an
#     unreachable server comes back as text, a cited answer through claude()
#
# The Tavily part needs TAVILY_API_KEY. The SearXNG part needs a running local
# instance (bash local_tests/searxng.sh start) and skips otherwise. A full run costs about 13 Tavily credits of the 1,000
# free monthly credits. Local models are left out on purpose: they are heavy on
# the maintainer's machine, so run them by hand when wanted.

devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("websearch")

count_searches <- function(.tool) {
  queries <- character(0)
  search  <- .tool@func
  .tool@func <- function(query, ...) {
    queries <<- c(queries, query)
    search(query, ...)
  }
  list(tool = .tool, queries = function() queries)
}

question <- "What is the current version of the tidyllm R package on CRAN, and when was it published? Cite your source."

if (!nzchar(Sys.getenv("TAVILY_API_KEY"))) {
  cat("  [skip] websearch - TAVILY_API_KEY is not set\n")
} else {

llt_test("websearch direct search returns numbered results", {
  out <- websearch_tool(.max_results = 3)@func(query = "tidyllm R package CRAN")
  llt_expect_true(is.character(out) && length(out) == 1, "result is not one string")
  llt_expect_true(grepl("[1]", out, fixed = TRUE), "no numbered result")
  llt_expect_true(grepl("URL: https?://", out), "no URL line")
  llt_expect_true(grepl(format(Sys.Date(), "%Y-%m-%d"), out, fixed = TRUE),
                  "the header does not carry today's date")
})

llt_test("websearch passes Tavily options through", {
  out <- websearch_tool(.max_results = 1, country = "narnia")@func(query = "central bank interest rate")
  llt_expect_true(grepl("^Web search failed \\(HTTP (400|422)\\)", out),
                  paste("an invalid country was not rejected by Tavily:", out))
})

llt_test("websearch returns a timeout as text", {
  out <- websearch_tool(.max_results = 1, .timeout = 0.001)@func(query = "weather Mannheim")
  llt_expect_true(grepl("^Web search failed:", out) && grepl("[Tt]ime", out),
                  paste("unexpected result for a timeout:", out))
})

llt_test("websearch cuts page text to .max_chars", {
  out <- websearch_tool(.max_results = 2, .include_content = TRUE, .max_chars = 300)@func(
    query = "tidyllm R package CRAN"
  )
  pages <- strsplit(out, "Page text:\n", fixed = TRUE)[[1]][-1]
  if (length(pages) == 0) {
    cat("    [note] Tavily returned no page text for this query; nothing to cut\n")
  } else {
    page_text <- sub("\n\n\\[\\d+\\].*$", "", pages)
    llt_expect_true(all(nchar(page_text) <= 300 + nchar(" [truncated]")),
                    "page text is longer than .max_chars")
  }
})

llt_test("websearch returns a failed search as text", {
  key <- Sys.getenv("TAVILY_API_KEY")
  Sys.setenv(TAVILY_API_KEY = "tvly-invalid")
  ws  <- tryCatch(websearch_tool(.max_results = 1), finally = Sys.setenv(TAVILY_API_KEY = key))
  out <- ws@func(query = "weather Mannheim")
  llt_expect_true(grepl("^Web search failed \\(HTTP 401\\)", out),
                  paste("unexpected result for a bad key:", out))
})

llt_test("websearch() returns a tibble with dates", {
  r <- websearch("tidyllm R package CRAN", .max_results = 3)
  llt_expect_tibble(r)
  llt_expect_true(identical(names(r), c("query", "title", "url", "published", "snippet", "text")),
                  "unexpected columns")
  llt_expect_true(inherits(r$published, "Date"), "published is not a Date")
  llt_expect_true(all(r$query == "tidyllm R package CRAN"), "query column is wrong")
})

llt_test("websearch() attaches the search service's answer", {
  r <- websearch("R programming language", .max_results = 1, include_answer = TRUE)
  llt_expect_true(is.character(attr(r, "answer")) && nzchar(attr(r, "answer")),
                  "no answer attribute")
})

llt_test("websearch() stops on a failed search", {
  key <- Sys.getenv("TAVILY_API_KEY")
  Sys.setenv(TAVILY_API_KEY = "tvly-invalid")
  err <- tryCatch(websearch("weather Mannheim", .max_results = 1), error = function(e) e,
                  finally = Sys.setenv(TAVILY_API_KEY = key))
  llt_expect_true(inherits(err, "error") && grepl("HTTP 401", conditionMessage(err)),
                  "a bad key did not raise an HTTP 401 error")
})

for (p in list(
  list(name = "claude", provider = claude(.model = "claude-haiku-4-5")),
  list(name = "openai", provider = openai(.model = "gpt-4o")),
  list(name = "gemini", provider = gemini(.model = "gemini-2.5-flash"))
)) {
  llt_test(sprintf("websearch cited answer through %s", p$name), {
    counted <- count_searches(websearch_tool(.max_results = 3))
    result  <- llm_message(question) |> chat(p$provider, .tools = counted$tool)
    llt_expect_reply(result)
    llt_expect_true(length(counted$queries()) >= 1, "the model never called the search tool")
    if (!grepl("https?://", get_reply(result))) cat("    [note] the reply cites no URL (model behaviour)\n")
  })
}

}

searxng_server <- Sys.getenv("SEARXNG_SERVER", "http://127.0.0.1:8888")
searxng_up <- tryCatch(
  httr2::request(searxng_server) |> httr2::req_url_path_append("healthz") |>
    httr2::req_timeout(3) |> httr2::req_perform() |> httr2::resp_status() == 200,
  error = function(e) FALSE
)
if (searxng_up) {
  invisible(tryCatch(websearch("warm up", .backend = "searxng", .server = searxng_server),
                     error = function(e) NULL))
}

if (!searxng_up) {
  cat("  [skip] websearch SearXNG - no server at", searxng_server,
      "(bash local_tests/searxng.sh start)\n")
} else {

llt_test("searxng direct search returns at most .max_results numbered results", {
  out <- websearch_tool("searxng", .server = searxng_server, .max_results = 3)@func(
    query = "tidyllm R package CRAN"
  )
  llt_expect_true(grepl("\n\n[1] ", out, fixed = TRUE), paste("no numbered result:", substr(out, 1, 200)))
  llt_expect_true(!grepl("\n\n[4] ", out, fixed = TRUE), "more results than .max_results")
  llt_expect_true(grepl("URL: https?://", out), "no URL line")
})

llt_test("searxng websearch() returns a tibble with dates", {
  r <- websearch("central bank interest rate decision", .backend = "searxng",
                 .server = searxng_server, .max_results = 5, time_range = "week")
  llt_expect_tibble(r)
  llt_expect_true(nrow(r) >= 1 && nrow(r) <= 5, paste("unexpected row count:", nrow(r)))
  llt_expect_true(inherits(r$published, "Date"), "published is not a Date")
  llt_expect_true(all(is.na(r$text)), "SearXNG should return no page text")
})

llt_test("searxng passes options through", {
  setup <- websearch_setup(websearch_backends$searxng, searxng_server, 5, FALSE, 4000, 30,
                           list(engines = "google cse"))
  body <- search_request(setup$backend, "R programming language", setup$settings) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  used <- c(purrr::map_chr(body$results, "engine"),
            purrr::map_chr(body$unresponsive_engines, ~ as.character(.x[[1]])))
  llt_expect_true(length(used) >= 1 && all(used == "google cse"),
                  paste("engines in the reply:", paste(unique(used), collapse = ", ")))
})

llt_test("searxng returns an unreachable server as text", {
  out <- websearch_tool("searxng", .server = "http://127.0.0.1:9")@func(query = "weather Mannheim")
  llt_expect_true(grepl("^Web search failed:", out), paste("unexpected result:", out))
})

llt_test("searxng cited answer through claude", {
  counted <- count_searches(websearch_tool("searxng", .server = searxng_server, .max_results = 5))
  result  <- llm_message(question) |> chat(claude(.model = "claude-haiku-4-5"), .tools = counted$tool)
  llt_expect_reply(result)
  llt_expect_true(length(counted$queries()) >= 1, "the model never called the search tool")
  if (!grepl("https?://", get_reply(result))) cat("    [note] the reply cites no URL (model behaviour)\n")
})

}

llt_report()

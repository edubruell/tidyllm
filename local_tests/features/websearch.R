# local_tests/features/websearch.R
# Live tests for websearch_tool() with the Tavily backend.
#
# What this tests:
#   - A direct search returns numbered results with URLs and readable dates
#   - Tavily options passed through ... reach the API
#   - .include_content cuts page text to .max_chars
#   - A failing search comes back as text for the model instead of an error
#   - The same tool object drives a cited answer in claude(), openai() and gemini()
#
# Needs TAVILY_API_KEY. A full run costs about 10 Tavily credits of the 1,000
# free monthly credits. Local models are left out on purpose: they are heavy on
# the maintainer's machine, so run them by hand when wanted.

devtools::load_all(quiet = TRUE)
source("local_tests/test_harness.R")
llt_suite("websearch")

if (!nzchar(Sys.getenv("TAVILY_API_KEY"))) {
  cat("  [skip] websearch - TAVILY_API_KEY is not set\n")
} else {

count_searches <- function(.tool) {
  queries <- character(0)
  search  <- .tool@func
  .tool@func <- function(query) {
    queries <<- c(queries, query)
    search(query)
  }
  list(tool = .tool, queries = function() queries)
}

llt_test("websearch direct search returns numbered results", {
  out <- websearch_tool(.max_results = 3)@func(query = "tidyllm R package CRAN")
  llt_expect_true(is.character(out) && length(out) == 1, "result is not one string")
  llt_expect_true(grepl("[1]", out, fixed = TRUE), "no numbered result")
  llt_expect_true(grepl("URL: https?://", out), "no URL line")
  llt_expect_true(grepl(format(Sys.Date(), "%Y-%m-%d"), out, fixed = TRUE),
                  "the header does not carry today's date")
})

llt_test("websearch passes Tavily options through", {
  out <- websearch_tool(.max_results = 3, topic = "news", time_range = "week")@func(
    query = "central bank interest rate decision"
  )
  llt_expect_true(grepl("Published: \\d{4}-\\d{2}-\\d{2}", out),
                  "news results should carry ISO publication dates")
})

llt_test("websearch cuts page text to .max_chars", {
  out <- websearch_tool(.max_results = 2, .include_content = TRUE, .max_chars = 300)@func(
    query = "R programming language"
  )
  pages <- strsplit(out, "Page text:\n", fixed = TRUE)[[1]][-1]
  llt_expect_true(length(pages) > 0, "no page text in the result")
  page_text <- sub("\n\n\\[\\d+\\].*$", "", pages)
  llt_expect_true(all(nchar(page_text) <= 300 + nchar(" [truncated]")),
                  "page text is longer than .max_chars")
})

llt_test("websearch returns a failed search as text", {
  ws  <- websearch_tool(.max_results = 1)
  key <- Sys.getenv("TAVILY_API_KEY")
  Sys.setenv(TAVILY_API_KEY = "tvly-invalid")
  out <- tryCatch(ws@func(query = "weather Mannheim"), finally = Sys.setenv(TAVILY_API_KEY = key))
  llt_expect_true(grepl("^Web search failed \\(HTTP 401\\)", out),
                  paste("unexpected result for a bad key:", out))
})

question <- "What is the current version of the tidyllm R package on CRAN, and when was it published? Cite your source."

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
    llt_expect_true(grepl("https?://", get_reply(result)), "the reply cites no URL")
  })
}

}

llt_report()

# Search the Web

Runs one web search and returns the results as a tibble. Use it to
collect search results as data, for example sources for a list of
companies, or to see exactly what a model would receive from
[`websearch_tool()`](https://edubruell.github.io/tidyllm/reference/websearch_tool.md),
which takes the same arguments.

## Usage

``` r
websearch(
  .query,
  .backend = c("tavily", "searxng"),
  .server = NULL,
  .max_results = 5,
  .include_content = FALSE,
  .max_chars = 4000,
  .timeout = 30,
  ...
)
```

## Arguments

- .query:

  The search query, a single string of at least two characters.

- .backend:

  The search service to use: `"tavily"` (the default) or `"searxng"`.
  See the section on search services below.

- .server:

  The address of your SearXNG server, such as `"http://localhost:8888"`.
  If `NULL`, the `SEARXNG_SERVER` environment variable is used. Only for
  `.backend = "searxng"`.

- .max_results:

  The most results a search returns, between 1 and 20. SearXNG returns
  one page of results, which may hold fewer.

- .include_content:

  If `TRUE`, each result also carries the text of the page, cut to
  `.max_chars` characters. Only Tavily can do this.

- .max_chars:

  The maximum number of characters of page text per result when
  `.include_content = TRUE`. Use `Inf` to keep the whole page.

- .timeout:

  Seconds to wait for each request to the search service. Busy or
  failing services are asked up to three times, for at most a minute in
  total.

- ...:

  Further search options passed to the search service by name. The
  options each service accepts are listed in the section on search
  services below; a misspelled option is an error.

## Value

A tibble with one row per result and the columns `query`, `title`,
`url`, `published` (a date, `NA` where the service does not know it),
`snippet` and `text` (`NA` unless `.include_content = TRUE`). If the
service writes a short summary (Tavily with `include_answer = TRUE`, or
a SearXNG instant answer), it is attached as the attribute `"answer"`.
If some SearXNG engines did not answer, their names and reasons are
attached as the attribute `"unresponsive_engines"`.

## Details

A failed search, for example with a wrong Tavily key, used-up credits or
an unreachable SearXNG server, stops with an error. To search for many
queries, map over them and bind the results; the `query` column keeps
them apart.

## Search services

**Tavily** is a paid search API with a free plan of 1,000 credits per
month, no credit card needed; one basic search costs one credit. It
needs a `TAVILY_API_KEY` environment variable. Options: `search_depth`
(`"basic"`, `"advanced"`, `"fast"` or `"ultra-fast"`; `"advanced"` costs
two credits), `topic` (`"general"`, `"news"` or `"finance"`),
`time_range` (`"day"`, `"week"`, `"month"` or `"year"`, or the short
forms `"d"`, `"w"`, `"m"` and `"y"`), `start_date` and `end_date`
(`"YYYY-MM-DD"`), `include_domains`, `exclude_domains`, `country`,
`include_answer` and others from Tavily's search API.

**SearXNG** is a free search engine you run yourself, which collects
results from Google, Brave and other engines. Use your own server:
public SearXNG servers usually refuse programs. Its `settings.yml` must
list `json` under `search: formats:`, and for a server only you use,
`server: limiter: false` stops it from blocking repeated searches. Set
the address with `.server` or the `SEARXNG_SERVER` environment variable.
SearXNG returns no page text. Options: `categories` (such as `"general"`
or `"news"`), `engines` (such as `c("google", "brave")`), `language`
(such as `"de"`), `time_range` (`"day"`, `"week"`, `"month"` or
`"year"`), `safesearch` (`0`, `1` or `2`) and `pageno` (which page of
results, for more than one page). A search fails if it finds nothing and
at least one engine did not answer, for example because it asked for a
CAPTCHA.

## Examples

``` r
if (FALSE) { # \dontrun{
websearch("tidyllm R package")

websearch("EU AI Act", .max_results = 10, topic = "news", time_range = "month")

websearch("EU AI Act", .backend = "searxng", .server = "http://localhost:8888",
          categories = "news", time_range = "week")

c("ZEW Mannheim", "ifo Institut") |>
  purrr::map(websearch, .max_results = 3) |>
  purrr::list_rbind()
} # }
```

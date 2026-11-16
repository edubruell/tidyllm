# Give a Model Web Search

Creates a tool that lets any model search the web while it answers. Pass
it to
[`chat()`](https://edubruell.github.io/tidyllm/dev/reference/chat.md)
with `.tools`, and the model decides when to search and what to search
for. It works the same way with every provider that supports tools,
including local models through
[`ollama()`](https://edubruell.github.io/tidyllm/dev/reference/ollama.md)
or
[`llamacpp()`](https://edubruell.github.io/tidyllm/dev/reference/llamacpp.md)
that have no web access of their own.

The model only chooses the search query. Everything else, such as how
many results come back and whether full page text is included, is fixed
when you create the tool, so a model cannot run up your search bill by
asking for more. The arguments are the same as for
[`websearch()`](https://edubruell.github.io/tidyllm/dev/reference/websearch.md),
which runs a single search directly and returns the results as a tibble.

## Usage

``` r
websearch_tool(
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

- .backend:

  The search service to use: `"tavily"` (the default) or `"searxng"`.
  See the section on search services below.

- .server:

  The address of your SearXNG server, such as `"http://localhost:8888"`.
  If `NULL`, the `SEARXNG_SERVER` environment variable is used. Only for
  `.backend = "searxng"`.

- .max_results:

  How many results each search returns, between 1 and 20.

- .include_content:

  If `TRUE`, each result also carries the text of the page, cut to
  `.max_chars` characters. This helps with questions a short excerpt
  cannot answer, but it makes every tool result much longer. Only Tavily
  can do this.

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

A tool object to pass to the `.tools` argument of
[`chat()`](https://edubruell.github.io/tidyllm/dev/reference/chat.md).

## Details

Each search returns one block of text to the model: the query, the date
of the search, and a numbered list of results with title, URL,
publication date where the service knows it, and an excerpt. The tool
asks the model to cite the URLs it relies on.

If a search fails, for example because the key is wrong or the monthly
credits are used up, the model receives the error message as the search
result instead of the conversation stopping. It will usually tell you
what went wrong.

The tool is named `tidyllm_web_search`, which is the name you will see
in the tool calls of a conversation. The Tavily key or SearXNG address
is read once, when the tool is created: set it before calling
`websearch_tool()`, and create the tool again after changing it. The key
is stored inside the tool, so do not save the tool object to a file you
share.

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
llm_message("What changed in the latest R release?") |>
  chat(ollama(), .tools = websearch_tool())

news_search <- websearch_tool(.max_results = 8, topic = "news", time_range = "week")
llm_message("Summarise this week's news on EU AI regulation.") |>
  chat(claude(), .tools = news_search)
} # }
```

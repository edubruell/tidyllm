# Using Tools with tidyllm

Language models are trained on static snapshots of the world. Ask a
model “What time is it in Stuttgart?” or “What is our current inventory
level?” and you will get either a refusal or a guess. Tools solve this
problem: they let you expose ordinary R functions to the model, so it
can request real data during a conversation. This article walks through
how tools work in **tidyllm**, from defining a single function to
orchestrating multi-step and parallel tool calls.

## Why Models Need Tools

A model without tools has no access to live data:

``` r

library(tidyllm)

llm_message("What time is it in Stuttgart right now?") |>
  chat(openai())
```

    ## 
    ## Attaching package: 'tidyllm'
    ## The following object is masked from 'package:stats':
    ## 
    ##     embed
    ## Message History:
    ## system:
    ## You are a helpful assistant
    ## --------------------------------------------------------------
    ## user:
    ## What time is it in Stuttgart right now?
    ## --------------------------------------------------------------
    ## assistant:
    ## I don't have access to real-time information, so I cannot
    ## tell you the current time in Stuttgart. Please check a clock
    ## or a time zone website for the current local time.
    ## --------------------------------------------------------------

The model knows that Stuttgart is in the `Europe/Berlin` timezone, but
it cannot call [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html).
When you attach a tool, the workflow becomes:

1.  The model receives your prompt together with tool descriptions.
2.  It identifies that external data is needed.
3.  It sends a **tool call** request back to your R session.
4.  tidyllm runs the R function and returns the result to the model.
5.  The model generates a final response using the tool result.

Steps 3-5 happen automatically inside
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md). You
only define the function and its parameter schema.

## Your First Tool

[`tidyllm_tool()`](https://edubruell.github.io/tidyllm/reference/tidyllm_tool.md)
wraps any R function with a schema that the model can read:

``` r

get_current_time <- function(tz, format = "%Y-%m-%d %H:%M:%S") {
  format(Sys.time(), tz = tz, format = format, usetz = TRUE)
}

time_tool <- tidyllm_tool(
  .f           = get_current_time,
  .description = "Returns the current time in a specified timezone.",
  tz     = field_chr("The timezone identifier, e.g. 'Europe/Berlin'."),
  format = field_chr("strftime format string. Default: '%Y-%m-%d %H:%M:%S'.")
)

llm_message("What time is it in Stuttgart right now?") |>
  chat(openai(), .tools = time_tool)
```

    ## Message History:
    ## system:
    ## You are a helpful assistant
    ## --------------------------------------------------------------
    ## user:
    ## What time is it in Stuttgart right now?
    ## --------------------------------------------------------------
    ## assistant:
    ## The current time in Stuttgart (Europe/Berlin timezone) is
    ## 2025-03-03 09:51:22 CET.
    ## --------------------------------------------------------------

Pass a single tool as `.tools = time_tool` or a list of tools as
`.tools = list(tool_a, tool_b)`.

## Defining Parameters

Each parameter in your function needs a matching schema entry. The field
helpers correspond to R types:

| Helper | Type sent to model | Use for |
|----|----|----|
| [`field_chr()`](https://edubruell.github.io/tidyllm/reference/field_chr.md) | string | text, identifiers, timezone names |
| [`field_dbl()`](https://edubruell.github.io/tidyllm/reference/field_chr.md) | number | numeric values, prices, quantities |
| [`field_lgl()`](https://edubruell.github.io/tidyllm/reference/field_chr.md) | boolean | flags, yes/no switches |
| [`field_fct()`](https://edubruell.github.io/tidyllm/reference/field_chr.md) | string with enum | parameters with a fixed set of valid values |

Here is a function with several parameters of different types:

``` r

query_products <- function(category, max_price, in_stock_only = TRUE) {
  products |>
    filter(
      category == category,
      price <= max_price,
      if (in_stock_only) stock > 0 else TRUE
    )
}

product_tool <- tidyllm_tool(
  .f           = query_products,
  .description = "Query the product catalogue by category and price.",
  category     = field_fct(
    "Product category to filter on.",
    .levels = c("electronics", "clothing", "books", "home")
  ),
  max_price    = field_dbl("Maximum price in USD."),
  in_stock_only = field_lgl("If TRUE, only return products currently in stock.")
)
```

[`field_fct()`](https://edubruell.github.io/tidyllm/reference/field_chr.md)
is particularly useful: by passing `.levels`, you tell the model exactly
which values are valid, which prevents it from inventing category names.

## Looking Up Data from R

Because tools are plain R functions, they have access to everything in
your session: data frames, database connections, API wrappers, or file
system paths. The following example lets a model answer questions about
employee records stored in a tibble:

``` r

library(tidyverse)

employees <- tribble(
  ~id, ~name,           ~department,   ~salary,
  1L,  "Alice Müller",  "Engineering", 82000,
  2L,  "Bob Schmidt",   "Marketing",   65000,
  3L,  "Clara Fischer", "Engineering", 91000,
  4L,  "David Braun",   "HR",          58000
)

get_employee <- function(department) {
  employees |>
    filter(department == department) |>
    select(name, salary) |>
    purrr::pmap_chr(\(name, salary) glue::glue("{name}: ${salary}")) |>
    paste(collapse = "\n")
}

employee_tool <- tidyllm_tool(
  .f           = get_employee,
  .description = "Return name and salary for all employees in a department.",
  department   = field_fct(
    "The department to query.",
    .levels = c("Engineering", "Marketing", "HR")
  )
)

llm_message("Who works in Engineering and what do they earn?") |>
  chat(claude(), .tools = employee_tool)
```

    ## Message History:
    ## system:
    ## You are a helpful assistant
    ## --------------------------------------------------------------
    ## user:
    ## Who works in Engineering and what do they earn?
    ## --------------------------------------------------------------
    ## assistant:
    ## The Engineering department has two employees:
    ## 
    ## - **Alice Müller** earns $82,000
    ## - **Clara Fischer** earns $91,000
    ## --------------------------------------------------------------

The model formulates the query, receives the returned text, and composes
a natural-language answer.

## Using Tools from Packages

R packages can expose ready-made tool definitions. If those definitions
were written for **ellmer**, convert them to tidyllm with
[`ellmer_tool()`](https://edubruell.github.io/tidyllm/reference/ellmer_tool.md):

``` r

library(ellmer)

btw_tool <- ellmer_tool(btw::btw_tool_files_list_files)

llm_message("List the files in the R/ folder.") |>
  chat(claude(), .tools = btw_tool)
```

    ## Message History:
    ## system:
    ## You are a helpful assistant
    ## --------------------------------------------------------------
    ## user:
    ## List the files in the R/ folder.
    ## --------------------------------------------------------------
    ## assistant:
    ## The `R/` folder contains the following files:
    ## 
    ## - `LLMMessage.R`
    ## - `APIProvider.R`
    ## - `llm_verbs.R`
    ## - `llm_message.R`
    ## - `tools.R`
    ## - `tidyllm_schema.R`
    ## - `api_openai.R`
    ## - `api_claude.R`
    ## - `api_gemini.R`
    ## - `api_ollama.R`
    ## - `zzz.R`
    ## --------------------------------------------------------------

[`ellmer_tool()`](https://edubruell.github.io/tidyllm/reference/ellmer_tool.md)
also supports provider-native builtin tools. For example, Claude’s
built-in web search is exposed by ellmer and can be passed directly to
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md):

``` r

web_search <- ellmer_tool(ellmer::claude_tool_web_search())

llm_message("What are the latest developments in R package tooling?") |>
  chat(claude(), .tools = web_search)
```

## Multi-Step Tool Chains

When solving a problem requires more than one tool call, the model will
make them in sequence. Each round, tidyllm executes the requested
function, returns the result, and the model decides whether another call
is needed before it can answer.

The `.max_tool_rounds` argument (default `10`) caps the number of rounds
to prevent runaway loops:

``` r

get_country_capital <- function(country) {
  capitals <- c(
    France  = "Paris",
    Germany = "Berlin",
    Japan   = "Tokyo",
    Brazil  = "Brasilia"
  )
  unname(capitals[country]) %||% "Unknown"
}

get_city_population <- function(city) {
  populations <- c(
    Paris    = "2.1 million (city), 12 million (metro)",
    Berlin   = "3.7 million (city), 6 million (metro)",
    Tokyo    = "14 million (city), 37 million (metro)",
    Brasilia = "3.1 million (city)"
  )
  unname(populations[city]) %||% "Unknown"
}

capital_tool <- tidyllm_tool(
  .f           = get_country_capital,
  .description = "Return the capital city of a country.",
  country      = field_chr("Country name, e.g. 'France'.")
)

population_tool <- tidyllm_tool(
  .f           = get_city_population,
  .description = "Return the population of a city.",
  city         = field_chr("City name, e.g. 'Paris'.")
)

llm_message("What is the population of the capital of Japan?") |>
  chat(openai(), .tools = list(capital_tool, population_tool), .max_tool_rounds = 5)
```

    ## Message History:
    ## system:
    ## You are a helpful assistant
    ## --------------------------------------------------------------
    ## user:
    ## What is the population of the capital of Japan?
    ## --------------------------------------------------------------
    ## assistant:
    ## The capital of Japan is **Tokyo**. Its population is
    ## approximately 14 million people in the city proper and
    ## around 37 million in the greater metropolitan area, making
    ## it the most populous metropolitan area in the world.
    ## --------------------------------------------------------------

The model first called `get_country_capital("Japan")` to get Tokyo, then
called `get_city_population("Tokyo")` to get the figure, and finally
composed the answer. Passing a list of tools lets the model choose which
one to call at each step.

## Parallel Tool Calls

Some models can issue multiple tool calls in a single round, before
waiting for any results. tidyllm executes all of them and returns all
results together, which reduces the number of API round-trips for
independent lookups:

``` r

get_weather <- function(city) {
  weather <- c(
    Paris  = "15°C, partly cloudy",
    London = "9°C, overcast with light rain",
    Tokyo  = "22°C, clear and sunny"
  )
  unname(weather[city]) %||% "No data available"
}

weather_tool <- tidyllm_tool(
  .f           = get_weather,
  .description = "Return the current weather for a city.",
  city         = field_chr("City name.")
)

llm_message("What is the weather like in Paris, London, and Tokyo right now?") |>
  chat(openai(), .tools = weather_tool)
```

    ## Message History:
    ## system:
    ## You are a helpful assistant
    ## --------------------------------------------------------------
    ## user:
    ## What is the weather like in Paris, London, and Tokyo right
    ## now?
    ## --------------------------------------------------------------
    ## assistant:
    ## Here is the current weather for each city:
    ## 
    ## - **Paris**: 15°C, partly cloudy
    ## - **London**: 9°C, overcast with light rain
    ## - **Tokyo**: 22°C, clear and sunny
    ## 
    ## Tokyo has the warmest and sunniest conditions, while London
    ## is the coolest with some rain.
    ## --------------------------------------------------------------

Instead of three sequential rounds (call, return, call, return, call,
return), the model issued all three calls at once, and tidyllm executed
them in a single batch before continuing.

## Conclusion

- [`tidyllm_tool()`](https://edubruell.github.io/tidyllm/reference/tidyllm_tool.md)
  wraps any R function into a tool definition the model can discover and
  call.
- [`field_chr()`](https://edubruell.github.io/tidyllm/reference/field_chr.md),
  [`field_dbl()`](https://edubruell.github.io/tidyllm/reference/field_chr.md),
  [`field_lgl()`](https://edubruell.github.io/tidyllm/reference/field_chr.md),
  and
  [`field_fct()`](https://edubruell.github.io/tidyllm/reference/field_chr.md)
  describe parameter types;
  [`field_fct()`](https://edubruell.github.io/tidyllm/reference/field_chr.md)
  with `.levels` constrains the model to valid values.
- Tools can access anything in your R session: in-memory data,
  databases, APIs, or the file system.
- [`ellmer_tool()`](https://edubruell.github.io/tidyllm/reference/ellmer_tool.md)
  converts ellmer `ToolDef` objects and provider-native builtin tools to
  tidyllm format, so tools from packages like `btw` work without
  modification.
- Multi-step chains are handled automatically; `.max_tool_rounds`
  prevents runaway loops.
- Models that support parallel tool calls will batch independent lookups
  into a single round; tidyllm handles the execution transparently.

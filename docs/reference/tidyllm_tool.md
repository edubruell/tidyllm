# Create a Tool Definition for tidyllm

Creates a tool definition for use with Language Model API calls that
support function calling. This function wraps an existing R function
with schema information for LLM interaction.

## Usage

``` r
tidyllm_tool(.f, .description = character(0), ...)
```

## Arguments

- .f:

  The function to wrap as a tool

- .description:

  Character string describing what the tool does

- ...:

  Named arguments providing schema definitions for each function
  parameter using tidyllm_fields

## Value

A `TOOL` class object that can be used with tidyllm
[`chat()`](https://edubruell.github.io/tidyllm/reference/chat.md)
functions

## Details

Each parameter schema in `...` should correspond to a parameter in the
wrapped function. All required function parameters must have
corresponding schema definitions.

## Examples

``` r
get_weather <- function(location){}
weather_tool <- tidyllm_tool(
  get_weather,
  "Get the current weather in a given location",
  location = field_chr("The city and state, e.g., San Francisco, CA")
)
```

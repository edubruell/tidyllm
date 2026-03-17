# Convert an ellmer Tool to a tidyllm TOOL

Converts an ellmer `ToolDef` or `ToolBuiltIn` object to a tidyllm `TOOL`
object, allowing seamless integration of ellmer-defined tools and
builtin provider tools with tidyllm workflows.

## Usage

``` r
ellmer_tool(.ellmer_tool)
```

## Arguments

- .ellmer_tool:

  An ellmer `ToolDef` object created via
  [`ellmer::tool()`](https://ellmer.tidyverse.org/reference/tool.html),
  or a builtin tool like
  [`ellmer::claude_tool_web_search()`](https://ellmer.tidyverse.org/reference/claude_tool_web_search.html)

## Value

A `TOOL` class object that can be used with tidyllm
[`chat()`](https://edubruell.github.io/tidyllm/dev/reference/chat.md)
functions

## Details

This function supports two types of ellmer tools:

**Custom ToolDef objects**: Extracts the function, description, and
argument schemas from an ellmer tool and converts them to tidyllm's
internal representation. Ellmer type objects are automatically converted
to tidyllm field descriptors.

**Builtin ToolBuiltIn objects**: Converts provider-native builtin tools
(like Claude's web search) to tidyllm format. For builtin tools, the
tool definition is passed through as-is to the provider's API, which
handles the tool execution natively.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ellmer)

# Example 1: Custom tool
tool_rnorm <- ellmer::tool(
  rnorm,
  description = "Draw numbers from a random normal distribution",
  arguments = list(
    n = ellmer::type_integer("The number of observations"),
    mean = ellmer::type_number("The mean value"),
    sd = ellmer::type_number("The standard deviation")
  )
)

tidyllm_tool_rnorm <- ellmer_tool(tool_rnorm)

llm_message("Generate 100 random numbers") |> 
  chat(openai(), .tools = tidyllm_tool_rnorm)

# Example 2: Builtin tool
web_search <- ellmer_tool(ellmer::claude_tool_web_search())
llm_message("What are the latest AI developments?") |>
  chat(claude(), .tools = web_search)
} # }
```

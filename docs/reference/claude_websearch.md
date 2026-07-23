# Builtin Claude Web Search Tool

Returns a TOOL object for Claude's builtin web_search tool. The default
tool version `web_search_20260209` adds dynamic result filtering and
requires Claude Sonnet 4.6, Opus 4.6 or newer; pass
`.version = "web_search_20250305"` for older models.

## Usage

``` r
claude_websearch(
  .max_uses = NULL,
  .allowed_domains = NULL,
  .blocked_domains = NULL,
  .version = "web_search_20260209"
)
```

## Arguments

- .max_uses:

  Integer; maximum number of searches the model may run per request.

- .allowed_domains:

  Character vector of domains to restrict search results to.

- .blocked_domains:

  Character vector of domains to exclude from search results.

- .version:

  Character; the Anthropic web search tool version (default:
  "web_search_20260209").

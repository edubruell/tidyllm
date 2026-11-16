# Builtin Claude Web Search Tool

Returns a TOOL object for Claude's builtin web_search tool. The default
tool version is `web_search_20260318`, which adds the
`response_inclusion` control on top of the dynamic result filtering
introduced in `web_search_20260209`.

## Usage

``` r
claude_websearch(
  .max_uses = NULL,
  .allowed_domains = NULL,
  .blocked_domains = NULL,
  .allowed_callers = "direct",
  .response_inclusion = NULL,
  .version = "web_search_20260318"
)
```

## Arguments

- .max_uses:

  Integer; maximum number of searches the model may run per request.

- .allowed_domains:

  Character vector of domains to restrict search results to.

- .blocked_domains:

  Character vector of domains to exclude from search results.

- .allowed_callers:

  Character vector of callers allowed to invoke the tool; `"direct"`
  (default) or `"code_execution_20260120"`. Pass `NULL` to let the API
  apply its own default. Ignored for `"web_search_20250305"`, which has
  no such field.

- .response_inclusion:

  Character; `"full"` or `"excluded"`, controlling whether search
  results enter the model context. Requires `web_search_20260318` or
  newer.

- .version:

  Character; the Anthropic web search tool version (default:
  "web_search_20260318").

## Details

From `web_search_20260209` onwards the API defaults `allowed_callers` to
the code execution tool, which only models with programmatic tool
calling support. tidyllm therefore sends `allowed_callers = "direct"` by
default, so web search works on every model. Pass
`.allowed_callers = "code_execution_20260120"` to opt into the dynamic
filtering path on Sonnet 4.6, Opus 4.6 or newer.

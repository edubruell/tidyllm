tidyllm 0.7.0 adds `claude_cli()`, a provider that runs a locally installed Claude command line tool, and `websearch_tool()` and `websearch()`, which give any provider web search through Tavily or SearXNG. `provider_capabilities()` returns a tibble of what each provider accepts. `perplexity()` is deprecated.

Default models are updated, because several providers retired models this summer. Bug fixes: empty tool schemas are sent as `{}`, Mistral reasoning replies and batch results are parsed correctly, OpenAI stateful mode sends all images, and `.capture_plot` no longer warns. The classifier article is rewritten for models without a temperature setting.

No new Imports. `withr`, used in tests, is added to `Suggests`.

## Test environments

* local macOS (aarch64-apple-darwin20), R 4.5.3

## R CMD check results

0 errors | 0 warnings | 3 notes

The notes are environmental: "unable to verify current time", an outdated local HTML Tidy, and 403 responses to scripted requests from `openai.com`, `platform.openai.com` and `portal.azure.com`, which load in a browser.

There are no reverse dependencies.

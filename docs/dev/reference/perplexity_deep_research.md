# Submit a Deep Research Request to Perplexity

Submit a Deep Research Request to Perplexity

## Usage

``` r
perplexity_deep_research(
  .llm,
  .background = FALSE,
  .reasoning_effort = "medium",
  .search_context_size = "medium",
  .search_domain_filter = NULL,
  .search_language_filter = NULL,
  .language_preference = NULL,
  .search_recency_filter = NULL,
  .search_mode = NULL,
  .search_after_date_filter = NULL,
  .search_before_date_filter = NULL,
  .last_updated_after_filter = NULL,
  .last_updated_before_filter = NULL,
  .user_location = NULL,
  .json_schema = NULL,
  .idempotency_key = NULL,
  .api_key = Sys.getenv("PERPLEXITY_API_KEY"),
  .timeout = 300,
  .max_tries = 3
)
```

## Arguments

- .llm:

  An `LLMMessage` object containing the research question.

- .background:

  Logical; if TRUE, returns a `tidyllm_research_job` immediately without
  waiting (default: FALSE).

- .reasoning_effort:

  Reasoning level: "low", "medium" (default), or "high".

- .search_context_size:

  Amount of search context: "low", "medium" (default), or "high".

- .search_domain_filter:

  Character vector of domains to allowlist/denylist (max 10; prefix with
  "-" to denylist).

- .search_language_filter:

  ISO 639-1 language code to restrict search results (e.g. "en", "de").

- .language_preference:

  ISO 639-1 code for preferred response language.

- .search_recency_filter:

  Restrict search to recent results: "hour", "day", "week", "month", or
  "year".

- .search_mode:

  Search index to use: "web" (default), "academic", or "sec".

- .search_after_date_filter:

  Only include content published after this date (MM/DD/YYYY).

- .search_before_date_filter:

  Only include content published before this date (MM/DD/YYYY).

- .last_updated_after_filter:

  Only include content last updated after this date (MM/DD/YYYY).

- .last_updated_before_filter:

  Only include content last updated before this date (MM/DD/YYYY).

- .user_location:

  Named list for geographic search personalisation (fields: country,
  city, region, latitude, longitude).

- .json_schema:

  A tidyllm schema created with
  [`tidyllm_schema()`](https://edubruell.github.io/tidyllm/dev/reference/tidyllm_schema.md)
  for structured JSON output (optional).

- .idempotency_key:

  Optional string; unique key to prevent duplicate submissions.

- .api_key:

  Character; Perplexity API key (default: from environment variable).

- .timeout:

  Integer; request timeout in seconds for blocking polling (default:
  300).

- .max_tries:

  Integer; maximum retries (default: 3).

## Value

If `.background = FALSE`, an updated `LLMMessage` with the research
reply. If `.background = TRUE`, a `tidyllm_research_job` object.

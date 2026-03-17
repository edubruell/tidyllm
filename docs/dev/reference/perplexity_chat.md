# Send LLM Messages to the Perplexity Chat API

Sends a chat message history to the Perplexity Chat API, supporting all
documented API parameters.

## Usage

``` r
perplexity_chat(
  .llm,
  .model = "sonar",
  .max_tokens = 1024,
  .temperature = NULL,
  .top_p = NULL,
  .frequency_penalty = NULL,
  .presence_penalty = NULL,
  .stop = NULL,
  .search_domain_filter = NULL,
  .search_language_filter = NULL,
  .language_preference = NULL,
  .return_images = FALSE,
  .image_domain_filter = NULL,
  .image_format_filter = NULL,
  .search_recency_filter = NULL,
  .search_mode = "web",
  .search_after_date_filter = NULL,
  .search_before_date_filter = NULL,
  .last_updated_after_filter = NULL,
  .last_updated_before_filter = NULL,
  .disable_search = FALSE,
  .enable_search_classifier = FALSE,
  .reasoning_effort = NULL,
  .return_related_questions = FALSE,
  .user_location = NULL,
  .search_context_size = NULL,
  .search_type = NULL,
  .stream_mode = NULL,
  .json_schema = NULL,
  .top_k = NULL,
  .web_search_options = NULL,
  .api_url = "https://api.perplexity.ai/",
  .timeout = 60,
  .stream = FALSE,
  .verbose = FALSE,
  .max_tries = 3,
  .dry_run = FALSE
)
```

## Arguments

- .llm:

  An `LLMMessage` object containing the conversation history.

- .model:

  Model name to use (default: "sonar").

- .max_tokens:

  Max completion tokens (default: 1024).

- .temperature:

  Controls response randomness (0 \< x \< 2).

- .top_p:

  Nucleus sampling threshold (0 \< x \< 1).

- .frequency_penalty:

  Number \> 0. Penalizes frequent tokens.

- .presence_penalty:

  Numeric between -2 and 2. Penalizes present tokens.

- .stop:

  Stop sequence(s), string or character vector/list.

- .search_domain_filter:

  Character vector of domains to allowlist/denylist (max 10; prefix with
  "-" to denylist).

- .search_language_filter:

  ISO 639-1 language code to restrict search results (e.g. "en", "de",
  "fr").

- .language_preference:

  ISO 639-1 code for preferred response language (e.g. "en").

- .return_images:

  Logical; if TRUE, returns images from search.

- .image_domain_filter:

  Character vector of domains to restrict image results to.

- .image_format_filter:

  Character vector of image formats to include (e.g. c("png", "jpg")).

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

- .disable_search:

  Logical; if TRUE, disables web search entirely (default: FALSE).

- .enable_search_classifier:

  Logical; if TRUE, lets the model decide whether to search (default:
  FALSE).

- .reasoning_effort:

  Reasoning level: "low", "medium", or "high".

- .return_related_questions:

  Logical; if TRUE, returns related questions.

- .user_location:

  Named list for geographic search personalisation. Accepted fields:
  `country` (ISO 3166-1 alpha-2), `city`, `region`, `latitude`,
  `longitude`. Example: `list(country = "DE", city = "Berlin")`.

- .search_context_size:

  Amount of search context to include: "low", "medium" (default), or
  "high".

- .search_type:

  Search quality preference inside `web_search_options`: "fast", "pro",
  or "auto".

- .stream_mode:

  Response format: "full" (default) or "concise".

- .json_schema:

  A tidyllm schema created with
  [`tidyllm_schema()`](https://edubruell.github.io/tidyllm/dev/reference/tidyllm_schema.md)
  for structured JSON output (optional).

- .top_k:

  Top-k token sampling (integer, 0 disables).

- .web_search_options:

  Named list with raw web_search_options overrides. Values set here take
  precedence over the dedicated parameters above.

- .api_url:

  API endpoint (default: "https://api.perplexity.ai/").

- .timeout:

  Timeout in seconds (default: 60).

- .stream:

  If TRUE, streams response.

- .verbose:

  If TRUE, prints additional info.

- .max_tries:

  Max request retries (default: 3).

- .dry_run:

  If TRUE, returns constructed request instead of sending.

## Value

An updated `LLMMessage` object with the assistant's reply and metadata,
including citations and search_results.

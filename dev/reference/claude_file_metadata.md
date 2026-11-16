# Retrieve Metadata for a File from Claude API

Retrieves metadata for a specific file uploaded to the Claude API.

## Usage

``` r
claude_file_metadata(
  .file_id,
  .api_url = "https://api.anthropic.com/",
  .timeout = 60,
  .max_tries = 3,
  .dry_run = FALSE
)
```

## Arguments

- .file_id:

  The file ID to retrieve metadata for.

- .api_url:

  Base URL for the Claude API (default: "https://api.anthropic.com/").

- .timeout:

  Request timeout in seconds (default: 60).

- .max_tries:

  Maximum retry attempts for requests (default: 3).

- .dry_run:

  Logical; if TRUE, returns the prepared request object without
  executing it.

## Value

A tibble containing metadata fields such as file_id, filename, size, and
MIME type.

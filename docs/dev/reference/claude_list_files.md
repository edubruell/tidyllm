# List Files in Claude API

Lists metadata for files uploaded to the Claude API, supporting
pagination.

## Usage

``` r
claude_list_files(
  .limit = 20,
  .order = "desc",
  .api_url = "https://api.anthropic.com/",
  .timeout = 60,
  .max_tries = 3,
  .dry_run = FALSE
)
```

## Arguments

- .limit:

  The maximum number of files to return (default: 20).

- .order:

  Order of results, either "asc" or "desc" (default: "desc").

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

A tibble containing metadata for each file, including file_id, filename,
size, and MIME type.

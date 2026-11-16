# Upload a File to Claude API

Uploads a file to the Claude API and returns its metadata as a tibble.

## Usage

``` r
claude_upload_file(
  .file_path,
  .api_url = "https://api.anthropic.com/",
  .timeout = 60,
  .max_tries = 3,
  .dry_run = FALSE
)
```

## Arguments

- .file_path:

  The local file path of the file to upload.

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

A tibble containing metadata about the uploaded file, including its
file_id, name, and size.

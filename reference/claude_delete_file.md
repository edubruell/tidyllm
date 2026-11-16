# Delete a File from Claude API

Deletes a specific file from the Claude API using its file ID.

## Usage

``` r
claude_delete_file(
  .file_id,
  .api_url = "https://api.anthropic.com/",
  .timeout = 60,
  .max_tries = 3,
  .dry_run = FALSE
)
```

## Arguments

- .file_id:

  The file ID to delete.

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

Invisibly returns NULL. Prints a confirmation message upon successful
deletion.

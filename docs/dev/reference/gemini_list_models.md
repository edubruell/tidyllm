# List Available Models from the Google Gemini API

List Available Models from the Google Gemini API

## Usage

``` r
gemini_list_models(.timeout = 60, .max_tries = 3, .dry_run = FALSE)
```

## Arguments

- .timeout:

  Request timeout in seconds (default: 60).

- .max_tries:

  Maximum number of retries for the API request (default: 3).

- .dry_run:

  Logical; if TRUE, returns the prepared request object without
  executing it.

## Value

A tibble containing model information with columns including `name`,
`base_model_id`, `version`, `display_name`, `description`,
`input_token_limit`, `output_token_limit`,
`supported_generation_methods`, `thinking`, `temperature`,
`max_temperature`, `top_p`, and `top_k`, or NULL if no models are found.

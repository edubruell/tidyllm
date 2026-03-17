# Get the current rate limit information for all or a specific API

This function retrieves the rate limit details for the specified API, or
for all APIs stored in the .tidyllm_rate_limit_env if no API is
specified.

## Usage

``` r
rate_limit_info(.api_name = NULL)
```

## Arguments

- .api_name:

  (Optional) The name of the API whose rate limit info you want to get
  If not provided, the rate limit info for all APIs in the environment
  will be returned

## Value

A tibble containing the rate limit information.

# Jev and System One compatible servers

Provider for
[`decide()`](https://edubruell.github.io/tidyllm/reference/decide.md).
The default server is TypeSafe AI's hosted Jev, read with the key in
`TYPESAFE_API_KEY`. Pass `.server` to reach any server that speaks the
same API, such as a Clef or CLM deployment.

## Usage

``` r
jev(..., .called_from = NULL)
```

## Arguments

- ...:

  Provider settings: `.server`, the base URL of the server without a
  path (`NULL`, the default, is `https://api.typesafe.ai`), and
  `.model`, the model name (default `"jev-latest"`).

- .called_from:

  Internal; the verb that dispatched here.

## Value

A provider call to pass as `.provider` to
[`decide()`](https://edubruell.github.io/tidyllm/reference/decide.md).

## Examples

``` r
if (FALSE) { # \dontrun{
decide("The invoice was paid late.",
       tidyllm_schema(late = field_lgl("Was the payment late?")), jev())
} # }
```

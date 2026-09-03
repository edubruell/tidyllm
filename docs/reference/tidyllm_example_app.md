# Run a tidyllm example Shiny app

The apps ship with the installed package, so they always match the
version of tidyllm you have. Each one defaults to a local
[`ollama()`](https://edubruell.github.io/tidyllm/reference/ollama.md)
model and therefore runs with no API key and no spend; a dropdown
switches to the cloud providers.

## Usage

``` r
tidyllm_example_app(.app = NULL, ...)
```

## Arguments

- .app:

  Name of the app to run. With no argument the available apps are
  listed.

- ...:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

Invisibly, the path to the app that was run.

## Details

Available apps:

`"model_explainer"`: fits a linear model to a small public dataset and
streams two explanations of the coefficients side by side, one
plain-English and one adversarial. Every number is computed in R; the
model only narrates the coefficient table it is shown.

## Examples

``` r
if (FALSE) { # \dontrun{
tidyllm_example_app()
tidyllm_example_app("model_explainer")
} # }
```

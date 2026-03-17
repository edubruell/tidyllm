# Define Field Descriptors for JSON Schema

These functions create field descriptors used in
[`tidyllm_schema()`](https://edubruell.github.io/tidyllm/dev/reference/tidyllm_schema.md)
or
[`field_object()`](https://edubruell.github.io/tidyllm/dev/reference/field_object.md)
to define JSON schema fields. They support character, factor, numeric,
and logical types.

## Usage

``` r
field_chr(.description = character(0), .vector = FALSE)

field_fct(.description = character(0), .levels, .vector = FALSE)

field_dbl(.description = character(0), .vector = FALSE)

field_lgl(.description = character(0), .vector = FALSE)
```

## Arguments

- .description:

  A character string describing the field (optional).

- .vector:

  A logical value indicating if the field is a vector (default: FALSE).

- .levels:

  A character vector specifying allowable values (for `field_fct()`
  only).

## Value

An S7 `tidyllm_field` object representing the field descriptor.

## Examples

``` r
field_chr("A common street name")
#> <tidyllm::tidyllm_field>
#>  @ type       : chr "string"
#>  @ description: chr "A common street name"
#>  @ enum       : chr(0) 
#>  @ vector     : logi FALSE
#>  @ schema     : list()
field_fct("State abbreviation", .levels = c("CA", "TX", "Other"))
#> <tidyllm::tidyllm_field>
#>  @ type       : chr "string"
#>  @ description: chr "State abbreviation"
#>  @ enum       : chr [1:3] "CA" "TX" "Other"
#>  @ vector     : logi FALSE
#>  @ schema     : list()
field_dbl("House number")
#> <tidyllm::tidyllm_field>
#>  @ type       : chr "number"
#>  @ description: chr "House number"
#>  @ enum       : chr(0) 
#>  @ vector     : logi FALSE
#>  @ schema     : list()
field_lgl("Is residential")
#> <tidyllm::tidyllm_field>
#>  @ type       : chr "boolean"
#>  @ description: chr "Is residential"
#>  @ enum       : chr(0) 
#>  @ vector     : logi FALSE
#>  @ schema     : list()
field_dbl("A list of appartment numbers at the address",.vector=TRUE )
#> <tidyllm::tidyllm_field>
#>  @ type       : chr "number"
#>  @ description: chr "A list of appartment numbers at the address"
#>  @ enum       : chr(0) 
#>  @ vector     : logi TRUE
#>  @ schema     : list()
```

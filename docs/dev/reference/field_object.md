# Define a nested object field

Define a nested object field

## Usage

``` r
field_object(.description = character(0), ..., .vector = FALSE)
```

## Arguments

- .description:

  A character string describing the field (optional).

- ...:

  Named fields to include in the object definition (required).

- .vector:

  A logical value indicating if the field is a list of objects (default:
  FALSE).

## Value

An S7 `tidyllm_field` object of type "object" containing nested fields.

## Examples

``` r
# Define an address object with nested fields
address <- field_object("A mailing address",
  street = field_chr("Street name"),
  city = field_chr("City name"),
  zipcode = field_chr("Postal code")
)

# Create a vector of objects
addresses <- field_object("List of addresses", 
  street = field_chr("Street name"),
  city = field_chr("City name"),
  .vector = TRUE
)
```

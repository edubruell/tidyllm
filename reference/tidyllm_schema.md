# Create a JSON Schema for Structured Outputs

This function creates a JSON schema for structured outputs, supporting
both character-based shorthand and S7 `tidyllm_field` objects. It also
integrates with `ellmer` types like
[`ellmer::type_string()`](https://ellmer.tidyverse.org/reference/type_boolean.html)
if ellmer is in your namespace

## Usage

``` r
tidyllm_schema(name = "tidyllm_schema", ...)
```

## Arguments

- name:

  A character string specifying the schema name (default:
  "tidyllm_schema").

- ...:

  Named arguments where each name represents a field, and each value is
  either a character string, a `tidyllm_field`, or an `ellmer` type.

  Supported character shorthand types:

  - "character" or "string" for character fields

  - "logical" for boolean fields

  - "numeric" for number fields

  - "factor(...)" for enumerations

  - Use `[]` to indicate vectors, e.g., "character\[\]"

## Value

A list representing the JSON schema, suitable for use with
`.json_schema` in LLM API calls.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example using different field types
address_schema <- tidyllm_schema(
  name = "AddressSchema",
  Street = field_chr("A common street name"),
  house_number = field_dbl(),
  City = field_chr("Name of a city"),
  State = field_fct("State abbreviation", .levels = c("CA", "TX", "Other")),
  Country = "string",
  PostalCode = "string"
)

llm_message("Imagine an address") |> chat(openai, .json_schema = address_schema)

# Example with vector field
tidyllm_schema(
  plz = field_dbl(.vector = TRUE)
)
} # }
```

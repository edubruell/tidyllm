% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidyllm_schema.R
\name{tidyllm_schema}
\alias{tidyllm_schema}
\title{Create a JSON Schema for Structured Outputs}
\usage{
tidyllm_schema(name = "tidyllm_schema", ...)
}
\arguments{
\item{name}{A character string specifying the schema name (default: "tidyllm_schema").}

\item{...}{Named arguments where each name represents a field, and each value is either a character string, a \code{tidyllm_field}, or an \code{ellmer} type.

Supported character shorthand types:
\itemize{
\item "character" or "string" for character fields
\item "logical" for boolean fields
\item "numeric" for number fields
\item "factor(...)" for enumerations
\item Use \verb{[]} to indicate vectors, e.g., "character[]"
}}
}
\value{
A list representing the JSON schema, suitable for use with \code{.json_schema} in LLM API calls.
}
\description{
This function creates a JSON schema for structured outputs, supporting both character-based shorthand and S7 \code{tidyllm_field} objects.
It also integrates with \code{ellmer} types like \code{ellmer::type_string()} if ellmer is in your namespace
}
\examples{
\dontrun{
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
}
}

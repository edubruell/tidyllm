#' Create a JSON schema for structured outputs
#'
#' This function creates a JSON schema suitable for use with the API functions in tidyllm. 
#'
#' @param name A character vector specifying the schema name. This serves as an identifier for the schema.
#' @param ... Named arguments where each name represents a field in the schema and each value specifies the type. Supported types include R data types:
#'   - "character": Represents a charcater type
#'   - "string": Allowed shorthand for charachter type
#'   - "factor(...)": A string with specific allowable values, represented as enum in JSON. Specify options as factor(option1, option2).
#'   - "logical": Represents a boolean.
#'   - "numeric": Represents a number.
#'   - "type[]": Appending [] allows for vector of a given type, e.g., "character[]".
#'
#' @return A list representing the JSON schema with the specified fields and types, suitable for passing to openai()'s .json_schema parameter.
#'
#' @examples
#' \dontrun{
#' # Define a schema with tidy data principles
#' json_schema <- tidyllm_schema(
#'   name = "DocumentAnalysisSchema",
#'   Title = "character",
#'   Authors = "character[]",
#'   SuggestedFilename = "character",
#'   Type = "factor(Policy, Research)",
#'   Answer_Q1 = "character",
#'   Answer_Q2 = "character",
#'   Answer_Q3 = "character",
#'   Answer_Q4 = "character",
#'   KeyCitations = "character[]"
#' )
#'
#' # Pass the schema to openai()
#' result <- openai(
#'   .llm = msg,
#'   .json_schema = json_schema
#' )
#'}
#' @details
#' The tidyllm_schema() function is designed to make defining JSON schemas for tidyllm more concise and user-friendly. It maps R-like types to JSON schema types and validates inputs to enforce tidy data principles. Nested structures are not allowed to maintain compatibility with tidy data conventions.
#'
#' @note Factor types (factor(...)) are treated as enumerations in JSON and are limited to a set of allowable string values. Arrays of a given type can be specified by appending [] to the type.
#'
#' @export
tidyllm_schema <- function(name, ...) {
  fields <- list(...)
  
  # Define mapping from R-like types to JSON schema types
  type_map <- list(
    string    = "string",
    character = "string",
    factor = "string",     # factors will be string in JSON, possibly with enum values
    logical = "boolean",
    numeric = "number"
  )
  
  # Input validation using validate_inputs
  validate_inputs(c(
    "Schema name must be a non-empty character string" = is.character(name) && nzchar(name),
    "Field names must be non-empty character strings" = all(nzchar(names(fields))),
    "Field types must be one of: character, factor(...), logical, numeric, or arrays thereof" = all(
      sapply(fields, function(field) {
        is.character(field) && 
          grepl("^(string|character|logical|numeric|factor\\(.*\\))(\\[\\])?$", field)
      })
    )
  ))
  
  parse_field <- function(field) {
    # Check for array notation
    is_array <- grepl("\\[\\]$", field)
    base_field <- sub("\\[\\]$", "", field)
    
    if (grepl("^factor\\(.*\\)$", base_field)) {
      # Enum type for factors
      enums <- base_field |>
        stringr::str_replace("^factor\\((.*)\\)$", "\\1") |>
        stringr::str_split(",") |>
        (\(x) x[[1]])()
      enums <- stringr::str_trim(enums)
      json_type <- list(type = "string", enum = enums)
    } else if (base_field %in% names(type_map)) {
      # Simple type
      json_type <- list(type = type_map[[base_field]])
    } else {
      stop("Unsupported field type: ", field)
    }
    
    if (is_array) {
      json_type <- list(type = "array", items = json_type)
    }
    
    return(json_type)
  }
  
  properties <- lapply(fields, parse_field)
  
  schema <- list(
    name = name,  # Schema name
    schema = list(
      type = "object",
      properties = properties,
      required = names(fields)
    )
  )
  
  return(schema)
}
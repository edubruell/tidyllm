tidyllm_field <- new_class("tidyllm_field", properties = list(
  type = class_character,
  description = class_character,
  enum = class_character,   # For factor (enum) fields only.
  vector = class_logical
))

#' Define Field Descriptors for JSON Schema
#'
#' These functions create field descriptors used in `tidyllm_schema()` to define JSON schema fields. They support character, factor, numeric, and logical types.
#'
#' @param .description A character string describing the field (optional).
#' @param .vector A logical value indicating if the field is a vector (default: FALSE).
#' @param .levels A character vector specifying allowable values (for `field_fct()` only).
#'
#' @return An S7 `tidyllm_field` object representing the field descriptor.
#'
#' @examples
#' field_chr("A common street name")
#' field_fct("State abbreviation", .levels = c("CA", "TX", "Other"))
#' field_dbl("House number")
#' field_lgl("Is residential")
#' field_dbl("A list of appartment numbers at the address",.vector=TRUE )
#' @export
field_chr <- function(.description = character(0), .vector = FALSE) {
  tidyllm_field(
    type = "string",
    description = .description,
    enum = character(0),
    vector = .vector
  )
}

#' @rdname field_chr
#' @export
field_fct <- function(.description = character(0), .levels, .vector = FALSE) {
  tidyllm_field(
    type = "string",
    description = .description,
    enum = .levels,
    vector = .vector
  )
}

#' @rdname field_chr
#' @export
field_dbl <- function(.description = character(0), .vector = FALSE) {
  tidyllm_field(
    type = "number",
    description = .description,
    enum = character(0),
    vector = .vector
  )
}

#' @rdname field_chr
#' @export
field_lgl <- function(.description = character(0), .vector = FALSE) {
  tidyllm_field(
    type = "boolean",
    description = .description,
    enum = character(0),
    vector = .vector
  )
}


#' Create a JSON Schema for Structured Outputs
#'
#' This function creates a JSON schema for structured outputs, supporting both character-based shorthand and S7 `tidyllm_field` objects.
#' It also integrates with `ellmer` types like `ellmer::type_string()` if ellmer is in your namespace
#'
#' @param name A character string specifying the schema name (default: "tidyllm_schema").
#' @param ... Named arguments where each name represents a field, and each value is either a character string, a `tidyllm_field`, or an `ellmer` type.
#'
#' Supported character shorthand types:
#' - "character" or "string" for character fields
#' - "logical" for boolean fields
#' - "numeric" for number fields
#' - "factor(...)" for enumerations
#' - Use `[]` to indicate vectors, e.g., "character[]"
#'
#' @return A list representing the JSON schema, suitable for use with `.json_schema` in LLM API calls.
#'
#' @examples
#' \dontrun{
#' # Example using different field types
#' address_schema <- tidyllm_schema(
#'   name = "AddressSchema",
#'   Street = field_chr("A common street name"),
#'   house_number = field_dbl(),
#'   City = field_chr("Name of a city"),
#'   State = field_fct("State abbreviation", .levels = c("CA", "TX", "Other")),
#'   Country = "string",
#'   PostalCode = "string"
#' )
#'
#' llm_message("Imagine an address") |> chat(openai, .json_schema = address_schema)
#'
#' # Example with vector field
#' tidyllm_schema(
#'   plz = field_dbl(.vector = TRUE)
#' )
#'}
#' @export
tidyllm_schema <- function(name = "tidyllm_schema", ...) {
  fields <- list(...)
  # Validate inputs: each field must be either a supported character string,
  # an S7 tidyllm_field, or (if ellmer is available) an ellmer type.
  validate_inputs(c(
    "Schema name must be a non-empty character string" = is.character(name) && nzchar(name),
    "Field names must be non-empty character strings" = all(nzchar(names(fields))),
    "Field types must be either a supported character string, an S7 tidyllm_field, or an ellmer type" =
      all(sapply(fields, function(field) {
        (is.character(field) && grepl("^(string|character|logical|numeric|factor\\(.*\\))(\\[\\])?$", field)) ||
          S7_inherits(field, tidyllm_field)  ||
          is_ellmer_type(field)
      }))
  ))
  
  
  # Mapping for simple character-based type specifications.
  type_map <- list(
    string    = "string",
    character = "string",
    factor    = "string",  # factors become strings (with enum specification below)
    logical   = "boolean",
    numeric   = "number"
  )
  
  # Convert each field to its JSON schema fragment.
  parse_field <- function(field) {
    # If ellmer is available and the field is an ellmer type, use ellmer's to_schema().
    if (requireNamespace("ellmer", quietly = TRUE) &&
        any(class(field) %in% c("ellmer::TypeBasic", "ellmer::TypeEnum", "ellmer::TypeObject", "ellmer::TypeArray"))) {
      json_type <- to_schema(field)
      return(json_type)
    }
    
    # If field is an S7 tidyllm_field object, extract its properties.
    if (S7_inherits(field, tidyllm_field)) {
      json_type <- list(type = field@type)
      if (length(field@description) > 0) {
        json_type$description <- field@description
      }
      if (length(field@enum) > 0) {
        json_type$enum <- field@enum
      }
      if (isTRUE(field@vector)) {
        json_type <- list(
          type = "array",
          items = json_type
        )
      }
      return(json_type)
    }
    
    # Otherwise, assume a character string specification.
    is_array <- grepl("\\[\\]$", field)
    base_field <- sub("\\[\\]$", "", field)
    
    if (grepl("^factor\\(.*\\)$", base_field)) {
      enums <- base_field |>
        stringr::str_replace("^factor\\((.*)\\)$", "\\1") |>
        stringr::str_split(",") |>
        (\(x) x[[1]])()
      enums <- stringr::str_trim(enums)
      json_type <- list(type = "string", enum = enums)
    } else if (base_field %in% names(type_map)) {
      json_type <- list(type = type_map[[base_field]])
    } else {
      stop("Unsupported field type: ", field)
    }
    
    if (is_array) {
      json_type <- list(
        type = "array",
        items = json_type
      )
    }
    return(json_type)
  }
  
  properties <- lapply(fields, parse_field)
  
  schema <- list(
    type = "object",
    properties = properties,
    required = I(names(fields))
  )
  
  attr(schema, "name") <- name
  
  return(schema)
}

to_schema <- new_generic("to_schema","x")
if (requireNamespace("ellmer", quietly = TRUE)) {
  method(to_schema, ellmer::TypeBasic) <- function(x) {
    list(type = x@type, description = x@description %||% "")
  }
  
  method(to_schema, ellmer::TypeEnum) <- function(x) {
    list(
      type = "string",
      description = x@description %||% "",
      enum = as.list(x@values)
    )
  }
  
  method(to_schema, ellmer::TypeObject) <- function(x) {
    names <- rlang::names2(x@properties)
    required <- purrr::map_lgl(x@properties, function(prop) prop@required)
    
    properties <- to_schema(x@properties)
    names(properties) <- names
    
    list(
      type = "object",
      description = x@description %||% "",
      properties = properties,
      required = as.list(names[required]),
      additionalProperties = x@additional_properties
    )
  }
  
  method(to_schema, ellmer::TypeArray) <- function(x) {
    list(
      type = "array",
      description = x@description %||% "",
      items = to_schema(x@items)
    )
  }  
  
  method(to_schema, class_list) <- function(x) {
      lapply(x, to_schema)
  }
}
tidyllm_field <- new_class("tidyllm_field", properties = list(
  type = class_character,
  description = class_character,
  enum = class_character,   # For factor (enum) fields only.
  vector = class_logical,
  schema = class_list,
  ordered = class_logical,
  level_descriptions = class_character,
  true_description = class_character,
  false_description = class_character
))

#' Define Field Descriptors for JSON Schema
#'
#' These functions create field descriptors used in `tidyllm_schema()` or `field_object()` to define JSON schema fields. They support character, factor, numeric, and logical types.
#'
#' @param .description A character string describing the field (optional).
#' @param .vector A logical value indicating if the field is a vector (default: FALSE).
#' @param .levels A character vector specifying allowable values (for `field_fct()` only).
#' @param .ordered For `field_fct()`: `TRUE` marks the levels as ordered, which
#'   `decide()` reads as a score question. Structured-output chat ignores it.
#' @param .level_descriptions For `field_fct()`: `NULL` (default) or a character
#'   vector with one description per level, in the order of `.levels`. Only
#'   `decide()` sends it; structured-output chat ignores it.
#' @param .true,.false For `field_lgl()`: `NULL` (default) or a string saying what
#'   a yes and a no mean. Give both or neither. Only `decide()` sends them;
#'   structured-output chat ignores them.
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
    vector = .vector,
    schema = list()
  )
}

#' @rdname field_chr
#' @export
field_fct <- function(.description = character(0), .levels, .vector = FALSE,
                      .ordered = FALSE, .level_descriptions = NULL) {
  c(".ordered must be TRUE or FALSE" = is.logical(.ordered) && length(.ordered) == 1 && !is.na(.ordered),
    ".level_descriptions must be NULL or a character vector with one entry per level" =
      is.null(.level_descriptions) ||
      (is.character(.level_descriptions) && length(.level_descriptions) == length(.levels))) |>
    validate_inputs()
  tidyllm_field(
    type = "string",
    description = .description,
    enum = .levels,
    vector = .vector,
    schema = list(),
    ordered = .ordered,
    level_descriptions = .level_descriptions %||% character(0)
  )
}

#' @rdname field_chr
#' @export
field_dbl <- function(.description = character(0), .vector = FALSE) {
  tidyllm_field(
    type = "number",
    description = .description,
    enum = character(0),
    vector = .vector,
    schema = list()
  )
}

#' @rdname field_chr
#' @export
field_lgl <- function(.description = character(0), .vector = FALSE,
                      .true = NULL, .false = NULL) {
  is_text <- \(x) is.null(x) || (is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x))
  c(".true and .false must each be NULL or one non-empty string" = is_text(.true) && is_text(.false),
    "Give both .true and .false, or neither" = is.null(.true) == is.null(.false)) |>
    validate_inputs()
  tidyllm_field(
    type = "boolean",
    description = .description,
    enum = character(0),
    vector = .vector,
    schema = list(),
    true_description = .true %||% character(0),
    false_description = .false %||% character(0)
  )
}

#' Define a nested object field
#' @param .description A character string describing the field (optional).
#' @param ... Named fields to include in the object definition (required).
#' @param .vector A logical value indicating if the field is a list of objects (default: FALSE).
#' @return An S7 `tidyllm_field` object of type "object" containing nested fields.
#' @examples
#' # Define an address object with nested fields
#' address <- field_object("A mailing address",
#'   street = field_chr("Street name"),
#'   city = field_chr("City name"),
#'   zipcode = field_chr("Postal code")
#' )
#' 
#' # Create a vector of objects
#' addresses <- field_object("List of addresses", 
#'   street = field_chr("Street name"),
#'   city = field_chr("City name"),
#'   .vector = TRUE
#' )
#' @export
field_object <- function(.description = character(0), ..., .vector = FALSE) {
  nested_fields <- list(...)
  if (length(nested_fields) == 0) {
    stop("field_object must have at least one nested field.")
  }
  nested_schema <- build_schema(nested_fields)
  
  tidyllm_field(
    type = "object",
    description = .description,
    enum = character(0),
    vector = .vector,
    schema = nested_schema
  )
}


#'parse_field converts a tidyllm_field (or a shorthand string) to its JSON schema fragment.
#'  
#'@noRd
parse_field <- function(field) {
  # If ellmer is available and the field is an ellmer type, use ellmer's to_schema().
  if (requireNamespace("ellmer", quietly = TRUE) &&
      any(class(field) %in% c("ellmer::TypeBasic", "ellmer::TypeEnum", "ellmer::TypeObject", "ellmer::TypeArray"))) {
    json_type <- to_schema(field)
    return(json_type)
  }
  
  # If field is an S7 tidyllm_field, process it.
  if (S7_inherits(field, tidyllm_field)) {
    # If the field is an object with a nested schema.
    if (field@type == "object" && length(field@schema)>=1) {
      json_type <- list(
        type = "object",
        properties = field@schema$properties,
        required = field@schema$required
      )
      if (length(field@description) > 0) {
        json_type$description <- field@description
      }
      if (isTRUE(field@vector)) {
        json_type <- list(
          type = "array",
          items = json_type
        )
      }
      return(json_type)
    }
    
    # For non-object fields.
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
  
  # Fallback for character shorthand specifications.
  is_array <- grepl("\\[\\]$", field)
  base_field <- sub("\\[\\]$", "", field)
  
  if (grepl("^factor\\(.*\\)$", base_field)) {
    enums <- base_field |>
      stringr::str_replace("^factor\\((.*)\\)$", "\\1") |>
      stringr::str_split(",") |>
      (\(x) x[[1]])()
    enums <- stringr::str_trim(enums)
    json_type <- list(type = "string", enum = enums)
  } else if (base_field %in% c("string", "character", "logical", "numeric", "factor")) {
    type_map <- list(
      string    = "string",
      character = "string",
      factor    = "string",
      logical   = "boolean",
      numeric   = "number"
    )
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

#' build_schema constructs a JSON schema list from a list of named field definitions.
#'  
#' @noRd
build_schema <- function(fields, name = NULL) {
  if (!all(nzchar(names(fields)))) {
    stop("Field names must be non-empty character strings")
  }
  properties <- lapply(fields, parse_field)
  schema <- list(
    type = "object",
    properties = properties,
    required = I(names(fields))
  )
  if (!is.null(name)) {
    attr(schema, "name") <- name
  }
  decide_meta <- schema_decide_meta(fields)
  if (length(decide_meta) > 0) {
    attr(schema, "decide_meta") <- decide_meta
  }
  schema
}

#' Question hints that only `decide()` reads, kept off the JSON itself
#'
#' Returns a named list with one entry per field that is ordered or carries level
#' or yes/no descriptions, and an empty list otherwise, so ordinary schemas gain no attribute.
#' @noRd
schema_decide_meta <- function(fields) {
  hints <- purrr::map(fields, \(f) {
    if (!S7_inherits(f, tidyllm_field)) return(NULL)
    if (!isTRUE(f@ordered) && length(f@level_descriptions) == 0 && length(f@true_description) == 0) return(NULL)
    list(ordered = isTRUE(f@ordered), level_descriptions = f@level_descriptions,
         true = f@true_description, false = f@false_description)
  })
  purrr::compact(hints)
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
  schema <- build_schema(fields, name = name)
  add_no_extra_fields(schema)
}


to_schema <- new_generic("to_schema", "x")
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


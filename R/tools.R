#' @title TOOL Class
#' @description A class representing a tool for Language Model function calling
#' 
#' @slot description Character string describing what the tool does
#' @slot input_schema List of parameter schemas for the tool (empty for builtin tools)
#' @slot func Function to be called by the LLM (dummy function for builtin tools that raises an error)
#' @slot name Character string name of the tool
#' @slot builtin List containing provider-native builtin tool definitions (empty for custom tools)
#'
#' @noRd 
TOOL <- new_class("TOOL", properties = list(
  description  = class_character,
  input_schema = class_list,
  func         = class_function,
  name         = class_character,
  builtin      = class_list
))


#' Create a Tool Definition for tidyllm
#'
#' @description
#' Creates a tool definition for use with Language Model API calls that support function calling.
#' This function wraps an existing R function with schema information for LLM interaction.
#'
#' @param .f The function to wrap as a tool 
#' @param .description Character string describing what the tool does
#' @param ... Named arguments providing schema definitions for each function parameter using tidyllm_fields
#'
#' @return A `TOOL` class object that can be used with tidyllm `chat()` functions
#'
#' @details
#' Each parameter schema in `...` should correspond to a parameter in the wrapped function.
#' All required function parameters must have corresponding schema definitions.
#'
#' @examples
#' get_weather <- function(location){}
#' weather_tool <- tidyllm_tool(
#'   get_weather,
#'   "Get the current weather in a given location",
#'   location = field_chr("The city and state, e.g., San Francisco, CA")
#' )
#'
#' @export
tidyllm_tool <- function(.f, .description = character(0), ...) {
  # Convert formula to function if needed
  formula_flag <- FALSE
  if (rlang::is_formula(.f)) {
    .f <- rlang::as_function(.f)
    formula_flag <- TRUE
  }
  
  # Check if function is anonymous
  fn_name <- tryCatch(
    rlang::as_name(rlang::ensym(.f)), 
    error = function(e) NULL
  )
  
  # Assign default name if function is anonymous
  if (is.null(fn_name) || fn_name == "") {
    fn_hash <- substr(rlang::hash(.f), 1, 8)  # Shorten hash
    .name <- paste0("anonymous_", fn_hash)
  } else {
    .name <- fn_name
  }
  
  # Extract function arguments and required ones
  fn_args <- rlang::fn_fmls(.f)
  required_args <- names(purrr::keep(fn_args, ~ identical(., rlang::missing_arg())))
  if (formula_flag==TRUE) {
    required_args <- ".x"
  }
  # Capture provided schema definitions
  schema_args <- list(...)
  
  # Ensure all required function args are in schema
  missing_args <- setdiff(required_args, names(schema_args))
  if (length(missing_args) > 0) {
    stop("Missing schema definitions for required arguments: ", paste(missing_args, collapse = ", "))
  }
  
  # Ensure all schema arguments are valid function arguments
  invalid_args <- setdiff(names(schema_args), names(fn_args))
  if (length(invalid_args) > 0) {
    stop("Schema defines arguments not present in function: ", paste(invalid_args, collapse = ", "))
  }
  
  obj <- TOOL(
    description = .description,
    input_schema = schema_args,
    func = .f,
    name = .name,
    builtin = list()
  )
  
  obj
}



#' Convert an ellmer Tool to a tidyllm TOOL
#'
#' @description
#' Converts an ellmer `ToolDef` or `ToolBuiltIn` object to a tidyllm `TOOL` object,
#' allowing seamless integration of ellmer-defined tools and builtin provider tools
#' with tidyllm workflows.
#'
#' @param .ellmer_tool An ellmer `ToolDef` object created via `ellmer::tool()`,
#'   or a builtin tool like `ellmer::claude_tool_web_search()`
#'
#' @return A `TOOL` class object that can be used with tidyllm `chat()` functions
#'
#' @details
#' This function supports two types of ellmer tools:
#'
#' **Custom ToolDef objects**: Extracts the function, description, and argument
#' schemas from an ellmer tool and converts them to tidyllm's internal representation.
#' Ellmer type objects are automatically converted to tidyllm field descriptors.
#'
#' **Builtin ToolBuiltIn objects**: Converts provider-native builtin tools (like
#' Claude's web search) to tidyllm format. For builtin tools, the tool definition
#' is passed through as-is to the provider's API, which handles the tool execution
#' natively.
#'
#' @examples
#' \dontrun{
#' library(ellmer)
#'
#' # Example 1: Custom tool
#' tool_rnorm <- ellmer::tool(
#'   rnorm,
#'   description = "Draw numbers from a random normal distribution",
#'   arguments = list(
#'     n = ellmer::type_integer("The number of observations"),
#'     mean = ellmer::type_number("The mean value"),
#'     sd = ellmer::type_number("The standard deviation")
#'   )
#' )
#'
#' tidyllm_tool_rnorm <- ellmer_tool(tool_rnorm)
#'
#' llm_message("Generate 100 random numbers") |> 
#'   chat(openai(), .tools = tidyllm_tool_rnorm)
#'
#' # Example 2: Builtin tool
#' web_search <- ellmer_tool(ellmer::claude_tool_web_search())
#' llm_message("What are the latest AI developments?") |>
#'   chat(claude(), .tools = web_search)
#' }
#'
#' @export
ellmer_tool <- function(.ellmer_tool) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("ellmer package required for ellmer_tool(). Install with: install.packages('ellmer')")
  }
  
  tool_classes <- class(.ellmer_tool)
  is_tool_def <- any(tool_classes %in% c("ellmer::ToolDef", "ellmer_ToolDef"))
  is_builtin <- any(tool_classes %in% c("ellmer::ToolBuiltIn", "ellmer_ToolBuiltIn"))
  
  if (!is_tool_def && !is_builtin) {
    stop("Input must be an ellmer ToolDef or ToolBuiltIn object created via ellmer::tool() or ellmer::*_tool_*()")
  }
  
  fn_name <- .ellmer_tool@name %||% "unknown"
  
  if (is_builtin) {
    dummy_fn <- function() {
      stop(sprintf("Builtin tool '%s' is executed by the provider, not locally", fn_name))
    }
    
    TOOL(
      description = sprintf("Builtin tool: %s", fn_name),
      input_schema = list(),
      func = dummy_fn,
      name = fn_name,
      builtin = list(.ellmer_tool@json)
    )
  } else {
    fn_desc <- .ellmer_tool@description %||% ""
    arguments_obj <- .ellmer_tool@arguments
    
    if (!any(class(arguments_obj) %in% c("ellmer::TypeObject", "ellmer_TypeObject"))) {
      stop("ellmer tool @arguments must be a TypeObject")
    }
    
    properties <- arguments_obj@properties
    
    if (length(properties) == 0) {
      input_schema <- list()
    } else {
      input_schema <- purrr::map(properties, function(arg_type) {
        convert_ellmer_type_to_field(arg_type)
      })
    }
    
    TOOL(
      description = fn_desc,
      input_schema = input_schema,
      func = .ellmer_tool,
      name = fn_name,
      builtin = list()
    )
  }
}


#' Convert an ellmer Type to a tidyllm Field
#'
#' @description Internal helper to convert ellmer type objects to tidyllm field descriptors
#'
#' @param .ellmer_type An ellmer type object (TypeBasic, TypeEnum, TypeArray, TypeObject)
#'
#' @return A `tidyllm_field` object
#'
#' @noRd
convert_ellmer_type_to_field <- function(.ellmer_type) {
  type_class <- class(.ellmer_type)
  
  if (any(type_class %in% c("ellmer::TypeBasic", "ellmer_TypeBasic"))) {
    ellmer_type_name <- .ellmer_type@type
    
    type_map <- c(
      "string" = "string",
      "integer" = "number",
      "number" = "number",
      "boolean" = "boolean"
    )
    
    tidyllm_type <- type_map[ellmer_type_name] %||% "string"
    
    tidyllm_field(
      type = tidyllm_type,
      description = .ellmer_type@description %||% "",
      enum = character(0),
      vector = FALSE,
      schema = list()
    )
  } else if (any(type_class %in% c("ellmer::TypeEnum", "ellmer_TypeEnum"))) {
    tidyllm_field(
      type = "string",
      description = .ellmer_type@description %||% "",
      enum = as.character(.ellmer_type@values),
      vector = FALSE,
      schema = list()
    )
  } else if (any(type_class %in% c("ellmer::TypeArray", "ellmer_TypeArray"))) {
    inner_field <- convert_ellmer_type_to_field(.ellmer_type@items)
    inner_field@vector <- TRUE
    inner_field
  } else if (any(type_class %in% c("ellmer::TypeObject", "ellmer_TypeObject"))) {
    properties <- .ellmer_type@properties
    nested_fields <- purrr::map(properties, convert_ellmer_type_to_field)
    nested_schema <- build_schema(nested_fields)
    
    tidyllm_field(
      type = "object",
      description = .ellmer_type@description %||% "",
      enum = character(0),
      vector = FALSE,
      schema = nested_schema
    )
  } else {
    stop("Unsupported ellmer type class: ", paste(type_class, collapse = ", "))
  }
}

#Generics for tools
tools_to_api <- new_generic("tools_to_api", c(".api", ".tools"))
run_tool_calls <- new_generic("run_tool_calls", c(".api",".tool_calls",".tools"))
send_tool_results <- new_generic("send_tool_results", c(".api",".request",".request_body"))

print.TOOL  <- new_external_generic("base", "print", "x")

#' Print method for a Tool Definition for tidyllm
#' @noRd
method(print.TOOL,TOOL) <- function(x, ...){
  cat("<tidyllm_tool> ", x@name, "\n", sep = "")
  cat("  Description: ", x@description, "\n", sep = "")
  cat("  Arguments: \n")
  purrr::iwalk(x@input_schema, ~ cat("    -", .y, ": ", .x@type, "\n"))
}

#' Generic method to convert a tidyllm TOOL definition for a generic API
#' @noRd
method(tools_to_api, list(APIProvider, class_list)) <- function(.api, .tools) {
  purrr::map(.tools, function(tool) {
    if (length(tool@builtin) > 0) {
      tool@builtin[[1]]
    } else {
      list(
        type = "function",
        `function` = list(
          name = tool@name,
          description = tool@description,
          parameters = list(
            type = "object",
            properties = purrr::map(tool@input_schema, function(param) {
              list(
                type = param@type,
                description = param@description
              )
            }),
            required = as.list(names(tool@input_schema)) # Assume all are required
          )
        )
      )
    }
  })
}

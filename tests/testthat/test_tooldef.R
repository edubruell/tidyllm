
test_that("tidyllm_tool works with an explicit function", {
  explicit_tool <- tidyllm_tool(
    .f = runif,
    .description = "Generates n uniformly distributed random numbers on a range.",
    n = field_dbl("An integer specifying how many random numbers to generate"),
    min = field_dbl("The smallest floating point number on the range"),
    max = field_dbl("The largest floating point number on the range")
  )
  
  expect_true(S7::S7_inherits(explicit_tool, TOOL))  # Check S7 class
  expect_equal(explicit_tool@description, "Generates n uniformly distributed random numbers on a range.")
  expect_named(explicit_tool@input_schema, c("n", "min", "max"))
  expect_true(is.function(explicit_tool@func))
  expect_type(explicit_tool@func(n = 1, min = 0, max = 1), "double")
})

test_that("tidyllm_tool works with an anonymous function", {
  anon_tool <- tidyllm_tool(
    .f = function(tz = "UTC", format = "%Y-%m-%d %H:%M:%S") {
      format(Sys.time(), format, tz = tz)
    },
    .description = "Returns the current time in a specified timezone. Use this to determine the current time in any location.",
    tz = field_chr("The time zone identifier (e.g., 'Europe/Berlin', 'America/New_York', 'Asia/Tokyo', 'UTC'). Required."),
    format = field_chr("Format string for the time output. Default is '%Y-%m-%d %H:%M:%S'.")
  )
  
  expect_true(S7::S7_inherits(anon_tool, TOOL))  # Check S7 class
  expect_equal(anon_tool@description, "Returns the current time in a specified timezone. Use this to determine the current time in any location.")
  expect_named(anon_tool@input_schema, c("tz", "format"))
  expect_true(is.function(anon_tool@func))
  expect_type(anon_tool@func(), "character")
})

test_that("tidyllm_tool works with formula-based anonymous functions", {
  fml_tool <- tidyllm_tool(
    ~{.x * 12345.6789},
    .description = "Multiplies a number by 12345.6789",
    .x = field_dbl("A number")
  )
  
  expect_true(S7::S7_inherits(fml_tool, TOOL))  # Check S7 class
  expect_equal(fml_tool@description, "Multiplies a number by 12345.6789")
  expect_named(fml_tool@input_schema, c(".x"))
  expect_true(is.function(fml_tool@func))
  expect_equal(fml_tool@func(2), 2 * 12345.6789)
})

test_that("tidyllm_tool assigns unique names to anonymous functions", {
  tool1 <- tidyllm_tool(~ .x * 2, "Doubles a number", .x = field_dbl("A number"))
  tool2 <- tidyllm_tool(~ .x + 3, "Adds 3", .x = field_dbl("A number"))
  
  expect_match(tool1@name, "anonymous_")
  expect_match(tool2@name, "anonymous_")
  expect_false(tool1@name == tool2@name)  # Should be unique
})

test_that("tidyllm_tool validates missing schema definitions", {
  expect_error(
    tidyllm_tool(
      function(x, y) x + y,
      "Adds two numbers",
      x = field_dbl("First number") # Missing `y`
    ),
    "Missing schema definitions for required arguments: y"
  )
})

test_that("tidyllm_tool does not allow invalid schema arguments", {
  expect_error(
    tidyllm_tool(
      function(x) x + 1,
      "Adds 1 to a number",
      x = field_dbl("A number"),
      y = field_dbl("A number")  # `y` does not exist in the function
    ),
    "Schema defines arguments not present in function: y"
  )
})

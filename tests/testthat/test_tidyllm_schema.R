
test_that("tidyllm_schema handles basic field types correctly", {
  schema <- tidyllm_schema(
    name = "TestSchema",
    field1 = field_chr("A test character field"),
    field2 = field_dbl("A test numeric field"),
    field3 = field_lgl("A test logical field"),
    field4 = field_fct("A test factor field", .levels = c("A", "B", "C"))
  )
  
  expect_equal(schema$type, "object")
  expect_named(schema$properties, c("field1", "field2", "field3", "field4"))
  
  expect_equal(schema$properties$field1$type, "string")
  expect_equal(schema$properties$field1$description, "A test character field")
  
  expect_equal(schema$properties$field2$type, "number")
  expect_equal(schema$properties$field2$description, "A test numeric field")
  
  expect_equal(schema$properties$field3$type, "boolean")
  expect_equal(schema$properties$field3$description, "A test logical field")
  
  expect_equal(schema$properties$field4$type, "string")
  expect_equal(schema$properties$field4$enum, c("A", "B", "C"))
  expect_equal(schema$properties$field4$description, "A test factor field")
})


test_that("tidyllm_schema correctly handles vector fields", {
  schema <- tidyllm_schema(
    name = "TestVectorSchema",
    field_vec = field_dbl("A numeric vector field", .vector = TRUE)
  )
  
  expect_named(schema$properties, "field_vec")
  expect_equal(schema$properties$field_vec$type, "array")
  expect_equal(schema$properties$field_vec$items$type, "number")
  expect_equal(schema$properties$field_vec$items$description, "A numeric vector field")
})


test_that("tidyllm_schema handles character-based type specifications", {
  schema <- tidyllm_schema(
    name = "TestShorthandSchema",
    field1 = "string",
    field2 = "logical",
    field3 = "numeric",
    field4 = "factor(A, B, C)",
    field5 = "character[]"
  )
  
  expect_named(schema$properties, c("field1", "field2", "field3", "field4", "field5"))
  
  expect_equal(schema$properties$field1$type, "string")
  expect_equal(schema$properties$field2$type, "boolean")
  expect_equal(schema$properties$field3$type, "number")
  expect_equal(schema$properties$field4$type, "string")
  expect_equal(schema$properties$field4$enum, c("A", "B", "C"))
  expect_equal(schema$properties$field5$type, "array")
  expect_equal(schema$properties$field5$items$type, "string")
})


test_that("tidyllm_schema validates incorrect field types", {
  expect_error(
    tidyllm_schema(
      name = "InvalidSchema",
      field1 = 42  # Invalid type
    ),
    "Field types must be either a supported character string, an S7 tidyllm_field, or an ellmer type"
  )
})

#Uncommented (causes issues in devtools::check)
#test_that("tidyllm_schema integrates with ellmer types if available", {
#  skip_if_not(requireNamespace("ellmer", quietly = TRUE))
#  library(ellmer)
#  
#  field_ellmer <- ellmer::type_string("An ellmer string field")
#  schema <- tidyllm_schema(
#    name = "EllmerSchema",
#    field1 = field_ellmer
#  )
#  
#  expect_named(schema$properties, "field1")
#  expect_equal(schema$properties$field1$type, "string")
#  expect_equal(schema$properties$field1$description, "An ellmer string field")
#})
test_that("tidyllm_schema sets additionalProperties on every object node", {
  schema <- tidyllm_schema(
    name = "NestedSchema",
    classifications = field_object(
      "One entry per title",
      title    = field_chr("Title"),
      relevant = field_lgl("Is it research"),
      .vector  = TRUE
    ),
    summary = field_object(
      "Aggregate counts",
      n_total = field_dbl("Number of titles")
    )
  )

  expect_false(schema$additionalProperties)
  expect_false(schema$properties$classifications$items$additionalProperties)
  expect_false(schema$properties$summary$additionalProperties)
  expect_equal(attr(schema, "name"), "NestedSchema")
})

test_that("the Gemini boundary strips additionalProperties recursively", {
  schema <- tidyllm_schema(
    name = "NestedSchema",
    classifications = field_object(
      "One entry per title",
      title   = field_chr("Title"),
      .vector = TRUE
    )
  )

  stripped <- tidyllm:::remove_extra_fields_key(schema)
  has_key <- function(node) {
    if (!is.list(node)) return(FALSE)
    if ("additionalProperties" %in% names(node)) return(TRUE)
    any(vapply(node, has_key, logical(1)))
  }

  expect_false(has_key(stripped))
})

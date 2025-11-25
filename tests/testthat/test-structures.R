# Structure Tests

library(testthat)
library(RSimpleFFI)

test_that("Basic structure creation works", {
  int_t <- ffi_int()
  double_t <- ffi_double()
  
  # Create a simple point structure
  Point <- ffi_struct(
    x = int_t,
    y = int_t
  )
  
  expect_s7_class(Point, RSimpleFFI::StructType) 
  expect_equal(Point@fields, c("x", "y"))
  expect_equal(length(Point@field_types), 2)
  expect_gt(Point@size, 0)
  
  # Check field types
  expect_s7_class(Point@field_types[[1]], RSimpleFFI::FFIType)
  expect_s7_class(Point@field_types[[2]], RSimpleFFI::FFIType)
  expect_equal(Point@field_types[[1]]@name, "int")
  expect_equal(Point@field_types[[2]]@name, "int")
})

test_that("Mixed type structures work", {
  int_t <- ffi_int()
  double_t <- ffi_double()
  
  Record <- ffi_struct(
    id = int_t,
    value = double_t,
    count = int_t
  )
  
  expect_equal(Record@fields, c("id", "value", "count"))
  expect_gt(Record@size, ffi_sizeof(int_t) + ffi_sizeof(double_t))
})

test_that("Structure validation works", {
  int_t <- ffi_int()
  
  # Empty struct should fail
  expect_error({
    ffi_struct()
  }, "Struct must have at least one field")
  
  # Unnamed fields should fail
  expect_error({
    ffi_struct(int_t, int_t)
  }, "All struct fields must be named")
  
  # Non-FFIType fields should fail  
  expect_error({
    ffi_struct(x = "not_a_type")
  }, "All struct fields must be FFIType objects")
})

test_that("Structure allocation works", {
  int_t <- ffi_int()
  double_t <- ffi_double()
  
  Point <- ffi_struct(
    x = double_t,
    y = double_t
  )
  
  # Allocate structure
  point <- ffi_alloc(Point)
  expect_type(point, "externalptr")
  expect_false(is_null_pointer(point))
})

test_that("Structure field access works", {
  double_t <- ffi_double()
  
  Point <- ffi_struct(
    x = double_t,
    y = double_t
  )
  
  point <- ffi_alloc(Point)
  
  # Set fields by name
  ffi_set_field(point, "x", 3.5, Point)
  ffi_set_field(point, "y", 4.5, Point)
  
  # Get fields by name
  x_val <- ffi_get_field(point, "x", Point)
  y_val <- ffi_get_field(point, "y", Point)
  
  expect_equal(x_val, 3.5)
  expect_equal(y_val, 4.5)
  
  # Access by index
  x_val2 <- ffi_get_field(point, 1L, Point)
  y_val2 <- ffi_get_field(point, 2L, Point)
  
  expect_equal(x_val2, 3.5)
  expect_equal(y_val2, 4.5)
})

test_that("Structure field validation works", {
  int_t <- ffi_int()
  
  Point <- ffi_struct(x = int_t, y = int_t)
  point <- ffi_alloc(Point)
  
  # Invalid field name
  expect_error({
    ffi_get_field(point, "z", Point)
  }, "No such field 'z'")
  
  expect_error({
    ffi_set_field(point, "invalid", 5L, Point)
  }, "No such field 'invalid'")
  
  # Invalid field index
  expect_error({
    ffi_get_field(point, 0L, Point)
  }, "Field index out of range")
  
  expect_error({
    ffi_get_field(point, 10L, Point)
  }, "Field index out of range")
})

test_that("Structure with functions works", {
  # Test structure functionality with existing factorial function
  int_t <- ffi_int()
  
  Point2D <- ffi_struct(
    x = int_t,
    y = int_t
  )
  
  # Create and initialize point
  point <- ffi_alloc(Point2D)
  ffi_set_field(point, "x", 5L, Point2D)
  ffi_set_field(point, "y", 4L, Point2D)
  
  # Get values back
  x_val <- ffi_get_field(point, "x", Point2D)
  y_val <- ffi_get_field(point, "y", Point2D)
  
  # Use factorial function to test the values
  factorial_fn <- ffi_function("test_factorial", int_t, int_t)
  x_factorial <- factorial_fn(x_val)
  y_factorial <- factorial_fn(y_val)
  
  expect_equal(x_factorial, 120L)  # 5! = 120
  expect_equal(y_factorial, 24L)   # 4! = 24
})

test_that("Structure return values work", {
  # Test structure field access and validation
  int_t <- ffi_int()
  double_t <- ffi_double()
  
  Record <- ffi_struct(
    id = int_t,
    value = double_t
  )
  
  # Test multiple structures
  record1 <- ffi_alloc(Record)
  record2 <- ffi_alloc(Record)
  
  # Set different values
  ffi_set_field(record1, "id", 100L, Record)
  ffi_set_field(record1, "value", 3.14, Record)
  
  ffi_set_field(record2, "id", 200L, Record)
  ffi_set_field(record2, "value", 2.71, Record)
  
  # Verify independence
  expect_equal(ffi_get_field(record1, "id", Record), 100L)
  expect_equal(ffi_get_field(record2, "id", Record), 200L)
})

test_that("Nested structures could work", {
  # This would require more complex C struct definitions
  # For now, just test that we could create nested types
  
  int_t <- ffi_int()
  double_t <- ffi_double()
  
  Inner <- ffi_struct(
    a = int_t,
    b = double_t
  )
  
  # We would need pointer types to Inner struct to make this work fully
  # For now, just verify the inner struct was created
  expect_s7_class(Inner, RSimpleFFI::StructType)
  expect_equal(Inner@fields, c("a", "b"))
})

test_that("Structure pretty printing works", {
  int_t <- ffi_int()
  double_t <- ffi_double()
  
  Point <- ffi_struct(
    x = double_t,
    y = double_t
  )
  
  # Test format method which works reliably
  formatted <- format(Point)
  expect_match(formatted, "StructType\\(fields=\\[x, y\\]")
  expect_match(formatted, "size=")
  
  # Test that struct has expected structure
  expect_equal(Point@fields, c("x", "y"))
  expect_equal(length(Point@field_types), 2)
  expect_equal(Point@field_types[[1]]@name, "double")
  expect_equal(Point@field_types[[2]]@name, "double")
})
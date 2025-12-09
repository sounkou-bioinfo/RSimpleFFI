test_that("API mode generic field accessors work with simple struct", {
  # Use existing Point2D struct from test_functions.c
  # typedef struct { int x; int y; } Point2D;
  
  # Create struct type
  Point2D <- ffi_struct(x = ffi_int(), y = ffi_int())
  
  # Allocate struct
  ptr <- ffi_alloc(Point2D)
  
  # Create mock struct type metadata for API mode
  # For now, we'll compute offsets manually since we haven't implemented
  # the code generation yet
  Point2D_type <- list(
    fields = list(
      x = list(type = ffi_int(), offset = 0),
      y = list(type = ffi_int(), offset = 4)  # int is 4 bytes
    )
  )
  
  # Test setting fields via API mode
  ffi_set_field(ptr, "x", 42L, Point2D_type)
  ffi_set_field(ptr, "y", 100L, Point2D_type)
  
  # Test getting fields via API mode
  x_val <- ffi_get_field(ptr, "x", Point2D_type)
  y_val <- ffi_get_field(ptr, "y", Point2D_type)
  
  expect_equal(x_val, 42L)
  expect_equal(y_val, 100L)
  
  # Test field pointer with lazy conversion
  x_ptr <- ffi_get_field_ptr(ptr, "x", Point2D_type)
  expect_true(inherits(x_ptr, "externalptr"))
  
  # Convert field pointer to R value
  x_val2 <- ffi_field_to_r(x_ptr)
  expect_equal(x_val2, 42L)
})

test_that("API mode accessors validate inputs", {
  Point2D <- ffi_struct(x = ffi_int(), y = ffi_int())
  ptr <- ffi_alloc(Point2D)
  
  Point2D_type <- list(
    fields = list(
      x = list(type = ffi_int(), offset = 0)
    )
  )
  
  # Test error on non-pointer
  expect_error(
    ffi_get_field(123, "x", Point2D_type),
    "must be an external pointer"
  )
  
  # Test error on unknown field
  expect_error(
    ffi_get_field(ptr, "nonexistent", Point2D_type),
    "Unknown field"
  )
  
  # Test error on missing fields metadata
  bad_type <- list()
  expect_error(
    ffi_get_field(ptr, "x", bad_type),
    "must contain 'fields'"
  )
})

test_that("API mode works with different types", {
  # Test with double field
  PointD <- ffi_struct(x = ffi_double(), y = ffi_double())
  ptr <- ffi_alloc(PointD)
  
  PointD_type <- list(
    fields = list(
      x = list(type = ffi_double(), offset = 0),
      y = list(type = ffi_double(), offset = 8)  # double is 8 bytes
    )
  )
  
  ffi_set_field(ptr, "x", 3.14, PointD_type)
  ffi_set_field(ptr, "y", 2.71, PointD_type)
  
  expect_equal(ffi_get_field(ptr, "x", PointD_type), 3.14)
  expect_equal(ffi_get_field(ptr, "y", PointD_type), 2.71)
})

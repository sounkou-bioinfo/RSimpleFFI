# Test: Function Returning Struct in RSimpleFFI

library(testthat)
library(RSimpleFFI)


# Define the struct type matching the C function's return type
Point2D <- ffi_struct(x = ffi_int(), y = ffi_int())

# Define the CIF for the function: Point2D test_create_point2d(int x, int y)
test_create_point2d_cif <- ffi_cif(Point2D, ffi_int(), ffi_int())

# Get the symbol for the function (exported from the DLL)
test_create_point2d_sym <- ffi_symbol("test_create_point2d")

# Call the function with example arguments
result_ptr <- ffi_call(
  test_create_point2d_cif,
  test_create_point2d_sym,
  10L,
  20L
)

# Test the returned struct fields
x_val <- ffi_get_field(result_ptr, "x", Point2D)
y_val <- ffi_get_field(result_ptr, "y", Point2D)

# Expectations
expect_equal(x_val, 10L)
expect_equal(y_val, 20L)

# Test: Function with struct input (test_move_point2d)
move_cif <- ffi_cif(ffi_void(), ffi_pointer(), ffi_int(), ffi_int())
move_sym <- ffi_symbol("test_move_point2d")

# Create a struct instance
p <- ffi_alloc(Point2D)
ffi_set_field(p, "x", 5L, Point2D)
ffi_set_field(p, "y", 7L, Point2D)
expect_equal(ffi_get_field(p, "x", Point2D), 5L)
expect_equal(ffi_get_field(p, "y", Point2D), 7L)

# Call the function to move the point by (3, 4)
ffi_call(move_cif, move_sym, p, 3L, 4L)

# Check the new values
x_new <- ffi_get_field(p, "x", Point2D)
y_new <- ffi_get_field(p, "y", Point2D)
expect_equal(x_new, 8L)
expect_equal(y_new, 11L)

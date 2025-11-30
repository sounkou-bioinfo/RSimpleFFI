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
result_ptr <- ffi_call(test_create_point2d_cif, test_create_point2d_sym, 10L, 20L)

# Test the returned struct fields
x_val <- ffi_get_field(result_ptr, "x", Point2D)
y_val <- ffi_get_field(result_ptr, "y", Point2D)

# Expectations
expect_equal(x_val, 10L)
expect_equal(y_val, 20L)

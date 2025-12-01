# Example: Nested Structs in RSimpleFFI
library(testthat)
library(RSimpleFFI)

test_that("Nested struct fields can be set and accessed", {
    # Define the inner struct type
    inner <- ffi_struct(
        x = ffi_int32(),
        y = ffi_double()
    )

    # Define the outer struct type, with an inline nested struct
    outer <- ffi_struct(
        id = ffi_int32(),
        point = inner, # Inline nested struct
        label = ffi_string()
    )

    # Allocate memory for the outer struct
    buf <- ffi_alloc_buffer(ffi_sizeof(outer))

    # Set fields in the outer struct
    ffi_set_field(buf, "id", 42L, outer)
    ffi_set_field(buf, "label", "example", outer)

    # Access the nested struct within the parent buffer
    point_ptr <- ffi_get_field(buf, "point", outer)
    ffi_set_field(point_ptr, "x", 10L, inner)
    ffi_set_field(point_ptr, "y", 3.14, inner)

    # Get fields from the outer struct
    id <- ffi_get_field(buf, "id", outer)
    label <- ffi_get_field(buf, "label", outer)

    # Get fields from the nested struct
    x <- ffi_get_field(point_ptr, "x", inner)
    y <- ffi_get_field(point_ptr, "y", inner)

    expect_equal(id, 42L)
    # String fields are now returned directly as character
    expect_equal(label, "example")
    expect_equal(x, 10L)
    expect_equal(y, 3.14)
})

test_that("signed bit-field struct by value and pointer", {
    # Accessor with signed fields
    accessor <- ffi_create_bitfield_accessors(list(
        signed4 = list(width = 4L, signed = TRUE),
        signed5 = list(width = 5L, signed = TRUE),
        padding = 7L
    ), base_type = ffi_int8())

    # Default accessor (32-bit base type) should compute different offsets
    default_accessor <- ffi_create_bitfield_accessors(list(
        signed4 = list(width = 4L, signed = TRUE),
        signed5 = list(width = 5L, signed = TRUE),
        padding = 7L
    ))
    expect_equal(default_accessor$field_offsets, c(0L, 4L, 9L))

    # The accessor should compute offsets based on the base type (int8 here)
    expect_equal(accessor$field_offsets, c(0L, 8L, 16L))

    # Use the explicit pack function (returns uint16) for by-value checks
    pack_fn <- ffi_function("test_signed_bitfield_pack", ffi_uint16(), ffi_int(), ffi_int())
    packed <- pack_fn(-3L, -5L)
    # Unpack via accessor
    vals <- accessor$unpack(as.integer(packed))
    expect_equal(vals$signed4, -3)
    expect_equal(vals$signed5, -5)

    # Also test pointer-based helpers using the same packed value
    c_packed <- packed

    # Allocate a 32-bit buffer (int) and write the packed 16-bit value into it
    buf <- ffi_alloc(ffi_int(), 1L)
    ffi_fill_typed_buffer(buf, as.integer(c_packed), ffi_int())

    ptr_get <- ffi_function("test_signed_bitfield_struct_ptr_get4", ffi_int(), ffi_pointer())
    res <- ptr_get(buf)
    expect_equal(as.integer(res), -3)

    # Test pointer setter
    ptr_set <- ffi_function("test_signed_bitfield_struct_ptr_set4", ffi_void(), ffi_pointer(), ffi_int())
    ptr_set(buf, -2L)
    res2 <- ptr_get(buf)
    expect_equal(as.integer(res2), -2)
})

test_that("bitfield accessor allocates correct buffer and packs/unpacks", {
    # Create bitfield accessor for 1+3+4 bits
    acc <- ffi_create_bitfield_accessors(
        list(
            enabled = 1L,
            mode = 3L,
            priority = 4L
        )
    )
    # Allocate buffer
    ptr <- ffi_alloc(acc)
    expect_true(inherits(ptr, "externalptr"))

    # Pack values
    packed <- acc$pack(list(enabled = 1L, mode = 5L, priority = 12L))
    expect_equal(packed, 203L)

    # Unpack values
    unpacked <- acc$unpack(packed)
    expect_equal(unpacked$enabled, 1L)
    expect_equal(unpacked$mode, 5L)
    expect_equal(unpacked$priority, 12L)

    # Compare with C bitfield packing
    pack_c <- ffi_function(
        "test_pack_bitfield",
        ffi_uint8(),
        ffi_uint8(),
        ffi_uint8(),
        ffi_uint8()
    )
    packed_c <- pack_c(1L, 5L, 12L)
    expect_equal(packed, packed_c)

    # Compare with C bitfield unpacking
    unpack_c <- ffi_function(
        "test_unpack_bitfield",
        ffi_void(),
        ffi_uint8(),
        ffi_pointer(),
        ffi_pointer(),
        ffi_pointer()
    )
    enabled <- as.integer(0)
    mode <- as.integer(0)
    priority <- as.integer(0)
    # Allocate pointers for output
    enabled_ptr <- ffi_alloc(ffi_uint8(), 1L)
    mode_ptr <- ffi_alloc(ffi_uint8(), 1L)
    priority_ptr <- ffi_alloc(ffi_uint8(), 1L)
    unpack_c(packed, enabled_ptr, mode_ptr, priority_ptr)
    enabled_val <- as.integer(ffi_copy_array(enabled_ptr, 1L, ffi_uint8())[[1]])
    mode_val <- as.integer(ffi_copy_array(mode_ptr, 1L, ffi_uint8())[[1]])
    priority_val <- as.integer(ffi_copy_array(priority_ptr, 1L, ffi_uint8())[[1]])
    expect_equal(enabled_val, 1L)
    expect_equal(mode_val, 5L)
    expect_equal(priority_val, 12L)
})

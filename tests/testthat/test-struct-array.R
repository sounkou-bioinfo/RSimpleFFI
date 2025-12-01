# Tests for array of structs functionality

test_that("ffi_alloc with n > 1 allocates struct array", {
    Point <- ffi_struct(x = ffi_int(), y = ffi_int())

    # Should not error
    points <- ffi_alloc(Point, 5L)
    expect_true(inherits(points, "externalptr"))
})

test_that("ffi_get_element returns valid pointers", {
    Point <- ffi_struct(x = ffi_int(), y = ffi_int())
    points <- ffi_alloc(Point, 3L)

    # Get each element
    p1 <- ffi_get_element(points, 1L, Point)
    p2 <- ffi_get_element(points, 2L, Point)
    p3 <- ffi_get_element(points, 3L, Point)

    expect_true(inherits(p1, "externalptr"))
    expect_true(inherits(p2, "externalptr"))
    expect_true(inherits(p3, "externalptr"))
})

test_that("struct array elements can be read and written", {
    Point <- ffi_struct(x = ffi_int(), y = ffi_int())
    points <- ffi_alloc(Point, 5L)

    # Set values for each point
    for (i in 1:5) {
        p <- ffi_get_element(points, i, Point)
        ffi_set_field(p, "x", as.integer(i * 10), Point)
        ffi_set_field(p, "y", as.integer(i * 20), Point)
    }

    # Read back and verify
    for (i in 1:5) {
        p <- ffi_get_element(points, i, Point)
        expect_equal(ffi_get_field(p, "x", Point), i * 10L)
        expect_equal(ffi_get_field(p, "y", Point), i * 20L)
    }
})

test_that("struct array with alignment padding works", {
    # int (4) + padding (4) + double (8) = 16 bytes
    Mixed <- ffi_struct(a = ffi_int(), b = ffi_double())
    expect_equal(ffi_sizeof(Mixed), 16L)

    arr <- ffi_alloc(Mixed, 3L)

    # Set values
    for (i in 1:3) {
        m <- ffi_get_element(arr, i, Mixed)
        ffi_set_field(m, "a", as.integer(i), Mixed)
        ffi_set_field(m, "b", i * 1.5, Mixed)
    }

    # Read back
    for (i in 1:3) {
        m <- ffi_get_element(arr, i, Mixed)
        expect_equal(ffi_get_field(m, "a", Mixed), i)
        expect_equal(ffi_get_field(m, "b", Mixed), i * 1.5)
    }
})

test_that("ffi_get_element validates inputs", {
    Point <- ffi_struct(x = ffi_int(), y = ffi_int())
    points <- ffi_alloc(Point, 3L)

    # Invalid index
    expect_error(ffi_get_element(points, 0L, Point), "positive integer")
    expect_error(ffi_get_element(points, -1L, Point), "positive integer")

    # Invalid struct_type
    expect_error(ffi_get_element(points, 1L, ffi_int()), "StructType")

    # Invalid ptr
    expect_error(ffi_get_element("not a pointer", 1L, Point), "external pointer")
})

test_that("single struct allocation still works", {
    Point <- ffi_struct(x = ffi_int(), y = ffi_int())

    # n = 1 should use R_alloc_struct
    p <- ffi_alloc(Point, 1L)
    expect_true(inherits(p, "externalptr"))

    # Can read/write
    ffi_set_field(p, "x", 42L, Point)
    ffi_set_field(p, "y", 99L, Point)
    expect_equal(ffi_get_field(p, "x", Point), 42L)
    expect_equal(ffi_get_field(p, "y", Point), 99L)
})

test_that("default n = 1 works", {
    Point <- ffi_struct(x = ffi_int(), y = ffi_int())

    # Default should be n = 1
    p <- ffi_alloc(Point)
    expect_true(inherits(p, "externalptr"))
})

# ============================================================================
# Tests using C test functions
# ============================================================================

test_that("struct size matches C sizeof", {
    # Verify R struct sizes match C
    sizeof_point2d <- ffi_symbol("test_sizeof_point2d")
    sizeof_mixed <- ffi_symbol("test_sizeof_mixed")

    cif_void_to_int <- ffi_cif(ffi_int())

    c_point_size <- ffi_call(cif_void_to_int, sizeof_point2d)
    c_mixed_size <- ffi_call(cif_void_to_int, sizeof_mixed)

    Point <- ffi_struct(x = ffi_int(), y = ffi_int())
    Mixed <- ffi_struct(a = ffi_int(), b = ffi_double())

    expect_equal(ffi_sizeof(Point), c_point_size)
    expect_equal(ffi_sizeof(Mixed), c_mixed_size)
})

test_that("C function can sum struct array", {
    Point <- ffi_struct(x = ffi_int(), y = ffi_int())
    points <- ffi_alloc(Point, 3L)

    # Set values: (1,2), (3,4), (5,6) -> sum = 1+2+3+4+5+6 = 21
    for (i in 1:3) {
        p <- ffi_get_element(points, i, Point)
        ffi_set_field(p, "x", as.integer(2 * i - 1), Point)
        ffi_set_field(p, "y", as.integer(2 * i), Point)
    }

    sum_func <- ffi_symbol("test_sum_point2d_array")
    cif <- ffi_cif(ffi_int(), ffi_pointer(), ffi_int())

    result <- ffi_call(cif, sum_func, points, 3L)
    expect_equal(result, 21L)
})

test_that("C function can modify struct array", {
    Point <- ffi_struct(x = ffi_int(), y = ffi_int())
    points <- ffi_alloc(Point, 2L)

    # Set initial values
    p1 <- ffi_get_element(points, 1L, Point)
    p2 <- ffi_get_element(points, 2L, Point)
    ffi_set_field(p1, "x", 10L, Point)
    ffi_set_field(p1, "y", 20L, Point)
    ffi_set_field(p2, "x", 30L, Point)
    ffi_set_field(p2, "y", 40L, Point)

    # Call C function that multiplies x by 2, y by 3
    scale_func <- ffi_symbol("test_scale_point2d_array")
    cif <- ffi_cif(ffi_void(), ffi_pointer(), ffi_int())
    ffi_call(cif, scale_func, points, 2L)

    # Verify changes
    expect_equal(ffi_get_field(p1, "x", Point), 20L)
    expect_equal(ffi_get_field(p1, "y", Point), 60L)
    expect_equal(ffi_get_field(p2, "x", Point), 60L)
    expect_equal(ffi_get_field(p2, "y", Point), 120L)
})

test_that("C function can init and sum MixedStruct array", {
    Mixed <- ffi_struct(a = ffi_int(), b = ffi_double())
    arr <- ffi_alloc(Mixed, 3L)

    # C function inits: a[i] = i+1, b[i] = (i+1)*0.5
    init_func <- ffi_symbol("test_init_mixed_array")
    cif_init <- ffi_cif(ffi_void(), ffi_pointer(), ffi_int())
    ffi_call(cif_init, init_func, arr, 3L)

    # Verify values from R side
    m1 <- ffi_get_element(arr, 1L, Mixed)
    m2 <- ffi_get_element(arr, 2L, Mixed)
    m3 <- ffi_get_element(arr, 3L, Mixed)

    expect_equal(ffi_get_field(m1, "a", Mixed), 1L)
    expect_equal(ffi_get_field(m1, "b", Mixed), 0.5)
    expect_equal(ffi_get_field(m2, "a", Mixed), 2L)
    expect_equal(ffi_get_field(m2, "b", Mixed), 1.0)
    expect_equal(ffi_get_field(m3, "a", Mixed), 3L)
    expect_equal(ffi_get_field(m3, "b", Mixed), 1.5)

    # Sum via C: (1+0.5) + (2+1.0) + (3+1.5) = 9.0
    sum_func <- ffi_symbol("test_sum_mixed_array")
    cif_sum <- ffi_cif(ffi_double(), ffi_pointer(), ffi_int())
    result <- ffi_call(cif_sum, sum_func, arr, 3L)
    expect_equal(result, 9.0)
})

test_that("AlignedStruct with char padding works with C", {
    # char (1) + pad (3) + int (4) + double (8) = 16 bytes
    Aligned <- ffi_struct(c = ffi_char(), i = ffi_int(), d = ffi_double())

    sizeof_aligned <- ffi_symbol("test_sizeof_aligned")
    cif <- ffi_cif(ffi_int())
    c_size <- ffi_call(cif, sizeof_aligned)

    expect_equal(ffi_sizeof(Aligned), c_size)

    # Allocate and init via C
    arr <- ffi_alloc(Aligned, 2L)
    init_func <- ffi_symbol("test_init_aligned_array")
    cif_init <- ffi_cif(ffi_void(), ffi_pointer(), ffi_int())
    ffi_call(cif_init, init_func, arr, 2L)

    # Sum via C
    sum_func <- ffi_symbol("test_sum_aligned_array")
    cif_sum <- ffi_cif(ffi_double(), ffi_pointer(), ffi_int())
    result <- ffi_call(cif_sum, sum_func, arr, 2L)

    # c='A'(65) + i=10 + d=1.5 + c='B'(66) + i=20 + d=3.0 = 165.5
    expect_equal(result, 165.5)
})

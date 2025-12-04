# SEXP Helper Tests

library(testthat)
library(RSimpleFFI)

test_that("sexp_ptr creates protected pointer", {
    x <- c(1L, 2L, 3L)
    ptr <- sexp_ptr(x)

    expect_true(inherits(ptr, "externalptr"))
    expect_true(is_protected_ptr(ptr))
})

test_that("sexp_ptr works with various types", {
    # Integer vector
    int_vec <- c(1L, 2L, 3L)
    expect_true(is_protected_ptr(sexp_ptr(int_vec)))

    # Double vector
    dbl_vec <- c(1.0, 2.0, 3.0)
    expect_true(is_protected_ptr(sexp_ptr(dbl_vec)))

    # Character vector
    chr_vec <- c("a", "b", "c")
    expect_true(is_protected_ptr(sexp_ptr(chr_vec)))

    # List
    lst <- list(a = 1, b = 2)
    expect_true(is_protected_ptr(sexp_ptr(lst)))

    # NULL
    expect_true(is_protected_ptr(sexp_ptr(NULL)))
})

test_that("data_ptr creates protected pointer to data", {
    x <- c(1.5, 2.5, 3.5)
    ptr <- data_ptr(x)

    expect_true(inherits(ptr, "externalptr"))
    expect_true(is_protected_ptr(ptr))
})

test_that("data_ptr works with different vector types", {
    # Integer
    int_vec <- c(1L, 2L, 3L)
    expect_true(is_protected_ptr(data_ptr(int_vec)))

    # Double
    dbl_vec <- c(1.0, 2.0, 3.0)
    expect_true(is_protected_ptr(data_ptr(dbl_vec)))

    # Logical
    lgl_vec <- c(TRUE, FALSE, TRUE)
    expect_true(is_protected_ptr(data_ptr(lgl_vec)))

    # Raw
    raw_vec <- as.raw(c(1, 2, 3))
    expect_true(is_protected_ptr(data_ptr(raw_vec)))

    # Complex
    cplx_vec <- complex(real = c(1, 2), imaginary = c(3, 4))
    expect_true(is_protected_ptr(data_ptr(cplx_vec)))
})

test_that("data_ptr_ro creates read-only protected pointer", {
    x <- c(1.0, 2.0, 3.0)
    ptr <- data_ptr_ro(x)

    expect_true(inherits(ptr, "externalptr"))
    expect_true(is_protected_ptr(ptr))
})

test_that("data_ptr_ro works with ALTREP sequences", {
    # 1:n creates an ALTREP compact sequence
    x <- 1:100
    ptr <- data_ptr_ro(x)

    expect_true(inherits(ptr, "externalptr"))
    expect_true(is_protected_ptr(ptr))
})

test_that("ptr_to_sexp recovers original object from sexp_ptr", {
    x <- c(1L, 2L, 3L, 4L, 5L)
    ptr <- sexp_ptr(x)
    recovered <- ptr_to_sexp(ptr)

    expect_identical(recovered, x)
})

test_that("ptr_to_sexp recovers original object from data_ptr", {
    x <- c(1.5, 2.5, 3.5)
    ptr <- data_ptr(x)
    recovered <- ptr_to_sexp(ptr)

    expect_identical(recovered, x)
})

test_that("is_protected_ptr returns FALSE for non-protected pointers", {
    # Regular external pointer (not from our helpers)
    regular_ptr <- ffi_alloc(ffi_int(), 1L)
    expect_false(is_protected_ptr(regular_ptr))

    # Non-external pointer

    expect_false(is_protected_ptr(NULL))
    expect_false(is_protected_ptr(1L))
    expect_false(is_protected_ptr("string"))
})

test_that("release_ptr releases protection", {
    x <- c(1L, 2L, 3L)
    ptr <- sexp_ptr(x)

    expect_true(is_protected_ptr(ptr))

    # Release should work without error
    expect_silent(release_ptr(ptr))
})

test_that("release_ptr errors on non-external pointer", {
    expect_error(release_ptr(1L), "Not an external pointer")
    expect_error(release_ptr("string"), "Not an external pointer")
})

test_that("ptr_to_sexp errors on released pointer", {
    x <- c(1L, 2L, 3L)
    ptr <- sexp_ptr(x)
    release_ptr(ptr)

    expect_error(ptr_to_sexp(ptr), "released")
})

test_that("ptr_to_sexp errors on non-protected pointer", {
    regular_ptr <- ffi_alloc(ffi_int(), 1L)
    expect_error(ptr_to_sexp(regular_ptr), "Not a protected SEXP pointer")
})

test_that("sexp_ptr works with Rf_length via FFI", {
    x <- c(1L, 2L, 3L, 4L, 5L)
    ptr <- sexp_ptr(x)

    rf_length <- ffi_function("Rf_length", ffi_int(), ffi_pointer())
    result <- rf_length(ptr)


    expect_equal(result, 5L)
})

test_that("data_ptr provides valid pointer for C functions", {
    # Create a double vector and get its data pointer
    x <- c(1.0, 2.0, 3.0, 4.0, 5.0)
    ptr <- data_ptr(x)

    # The pointer should be valid (not NULL)
    expect_false(ffi_is_null(ptr))
})

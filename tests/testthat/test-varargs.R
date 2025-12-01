# Test varargs (variadic function) support

test_that("ffi_cif_var creates variadic CIF", {
    # Create CIF for test_varargs_sum(int nargs, ...)
    # 1 fixed arg (nargs), 3 variadic int args
    cif <- ffi_cif_var(ffi_double(),
        nfixedargs = 1L,
        ffi_int(), ffi_int(), ffi_int(), ffi_int()
    )

    expect_s3_class(cif, "RSimpleFFI::CIF")
    expect_equal(length(cif@arg_types), 4)
})

test_that("ffi_cif_var validates arguments", {
    # return_type must be FFIType

    expect_error(ffi_cif_var("int", 1L), "must be an FFIType")

    # nfixedargs must be non-negative integer
    expect_error(ffi_cif_var(ffi_int(), -1L), "non-negative")
    expect_error(ffi_cif_var(ffi_int(), "1"), "non-negative")

    # nfixedargs cannot exceed total args
    expect_error(
        ffi_cif_var(ffi_int(), 3L, ffi_int(), ffi_int()),
        "cannot exceed"
    )

    # All arg types must be FFIType
    expect_error(ffi_cif_var(ffi_int(), 1L, "int"), "must be FFIType")
})

test_that("ffi_cif_var with zero variadic args works", {
    # Edge case: all args are fixed (0 variadic)
    cif <- ffi_cif_var(ffi_int(),
        nfixedargs = 2L,
        ffi_int(), ffi_int()
    )
    expect_s3_class(cif, "RSimpleFFI::CIF")
})

test_that("ffi_cif_var with zero fixed args works", {
    # Edge case: all args are variadic
    cif <- ffi_cif_var(ffi_int(),
        nfixedargs = 0L,
        ffi_int(), ffi_int()
    )
    expect_s3_class(cif, "RSimpleFFI::CIF")
})

test_that("varargs call with integers works", {
    # test_varargs_sum(int nargs, ...) - sums nargs integers
    sym <- ffi_symbol("test_varargs_sum")

    # Sum 3 integers: 10 + 20 + 30 = 60
    cif <- ffi_cif_var(ffi_double(),
        nfixedargs = 1L,
        ffi_int(), ffi_int(), ffi_int(), ffi_int()
    )
    result <- ffi_call(cif, sym, 3L, 10L, 20L, 30L)
    expect_equal(result, 60)

    # Sum 1 integer: 42
    cif1 <- ffi_cif_var(ffi_double(),
        nfixedargs = 1L,
        ffi_int(), ffi_int()
    )
    result1 <- ffi_call(cif1, sym, 1L, 42L)
    expect_equal(result1, 42)

    # Sum 0 integers (edge case)
    cif0 <- ffi_cif_var(ffi_double(), nfixedargs = 1L, ffi_int())
    result0 <- ffi_call(cif0, sym, 0L)
    expect_equal(result0, 0)
})

test_that("varargs call with doubles works", {
    # test_varargs_sum_doubles(int nargs, ...) - sums nargs doubles
    sym <- ffi_symbol("test_varargs_sum_doubles")

    # Sum 3 doubles: 1.5 + 2.5 + 3.0 = 7.0
    cif <- ffi_cif_var(ffi_double(),
        nfixedargs = 1L,
        ffi_int(), ffi_double(), ffi_double(), ffi_double()
    )
    result <- ffi_call(cif, sym, 3L, 1.5, 2.5, 3.0)
    expect_equal(result, 7.0)

    # Single double
    cif1 <- ffi_cif_var(ffi_double(),
        nfixedargs = 1L,
        ffi_int(), ffi_double()
    )
    result1 <- ffi_call(cif1, sym, 1L, 99.5)
    expect_equal(result1, 99.5)
})

test_that("varargs with mixed fixed args works", {
    # test_varargs_mixed(const char* prefix, int nargs, ...) - 2 fixed args
    sym <- ffi_symbol("test_varargs_mixed")

    # Prefix string + nargs + 2 variadic ints
    msg <- "test: "
    cif <- ffi_cif_var(ffi_int(),
        nfixedargs = 2L,
        ffi_string(), ffi_int(), ffi_int(), ffi_int()
    )
    result <- ffi_call(cif, sym, msg, 2L, 100L, 200L)
    expect_equal(result, 300L)
})

test_that("varargs different counts work", {
    sym <- ffi_symbol("test_varargs_sum")

    # Test with varying number of variadic arguments
    for (n in 1:5) {
        args <- as.list(rep(10L, n))
        # Build CIF for n variadic args
        arg_types <- c(list(ffi_int()), rep(list(ffi_int()), n))
        cif <- do.call(ffi_cif_var, c(list(ffi_double(), 1L), arg_types))

        # Call with nargs = n and n values of 10
        call_args <- c(list(cif, sym, as.integer(n)), args)
        result <- do.call(ffi_call, call_args)
        expect_equal(result, n * 10, info = paste("n =", n))
    }
})

test_that("varargs with truly mixed types works", {
    # test_varargs_mixed_types(int npairs, ...) expects alternating int, double
    sym <- ffi_symbol("test_varargs_mixed_types")

    # 2 pairs: (10, 1.5) + (20, 2.5) = 34
    cif <- ffi_cif_var(ffi_double(),
        nfixedargs = 1L,
        ffi_int(),
        ffi_int(), ffi_double(),
        ffi_int(), ffi_double()
    )
    result <- ffi_call(cif, sym, 2L, 10L, 1.5, 20L, 2.5)
    expect_equal(result, 34)

    # 3 pairs: (1, 0.1) + (2, 0.2) + (3, 0.3) = 6.6
    cif3 <- ffi_cif_var(ffi_double(),
        nfixedargs = 1L,
        ffi_int(),
        ffi_int(), ffi_double(),
        ffi_int(), ffi_double(),
        ffi_int(), ffi_double()
    )
    result3 <- ffi_call(cif3, sym, 3L, 1L, 0.1, 2L, 0.2, 3L, 0.3)
    expect_equal(result3, 6.6)
})

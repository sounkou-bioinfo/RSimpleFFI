# Test global variable access functions

test_that("ffi_read_global reads integer globals", {
    addr <- getNativeSymbolInfo("test_global_int", "RSimpleFFI")$address
    val <- ffi_read_global(addr, ffi_int())
    expect_equal(val, 42L)
})

test_that("ffi_read_global reads double globals", {
    addr <- getNativeSymbolInfo("test_global_double", "RSimpleFFI")$address
    val <- ffi_read_global(addr, ffi_double())
    expect_equal(val, 3.14159, tolerance = 1e-6)
})

test_that("ffi_deref_pointer reads pointer globals", {
    addr <- getNativeSymbolInfo("test_global_string", "RSimpleFFI")$address
    ptr <- ffi_deref_pointer(addr)
    val <- pointer_to_string(ptr)
    expect_equal(val, "Hello from global!")
})

test_that("ffi_copy_array works with global arrays", {
    addr <- getNativeSymbolInfo("test_global_array", "RSimpleFFI")$address
    vals <- ffi_copy_array(addr, 5L, ffi_int())
    expect_equal(vals, c(10L, 20L, 30L, 40L, 50L))
})

test_that("ffi_deref_pointer works with R_GlobalEnv", {
    addr <- getNativeSymbolInfo("R_GlobalEnv")$address
    globalenv_sexp <- ffi_deref_pointer(addr)
    expect_true(typeof(globalenv_sexp) == "externalptr")
    expect_false(is_null_pointer(globalenv_sexp))
})

test_that("can call R functions via C API using R_GlobalEnv", {
    # Get R_GlobalEnv
    R_GlobalEnv <- ffi_deref_pointer(getNativeSymbolInfo("R_GlobalEnv")$address)

    # Define R API functions
    rf_install <- ffi_function("Rf_install", ffi_pointer(), ffi_string())
    rf_lang2 <- ffi_function("Rf_lang2", ffi_pointer(), ffi_pointer(), ffi_pointer())
    rf_eval <- ffi_function("Rf_eval", ffi_pointer(), ffi_pointer(), ffi_pointer())
    rf_INTEGER_ELT <- ffi_function("INTEGER_ELT", ffi_int(), ffi_pointer(), ffi_long())
    rf_ScalarInteger <- ffi_function("Rf_ScalarInteger", ffi_pointer(), ffi_int())

    # Call abs(-42) via C API
    abs_sym <- rf_install("abs")
    neg_val <- rf_ScalarInteger(-42L)
    abs_call <- rf_lang2(abs_sym, neg_val)
    abs_result <- rf_eval(abs_call, R_GlobalEnv)

    expect_equal(rf_INTEGER_ELT(abs_result, 0L), 42L)
})

# Test compile_and_load
comp_dir <- file.path(tempdir(), "persistent_dll_dir")
dir.create(comp_dir, showWarnings = FALSE, recursive = TRUE)

test_that("DLL loading works with R's native facilities", {
  # DLL loading test - no skips!

  # Create types needed for testing
  int_t <- ffi_int()

  # Create a simple test library with native C functions (not SEXP-based)
  test_c_code <- "
// Pure C functions for FFI testing
int test_add_dll(int a, int b) {
    return a + b;
}

int test_multiply_dll(int a, int b) {
    return a * b;
}

double test_add_double_dll(double a, double b) {
    return a + b;
}
"
  expect_no_error({
    lib_handle <- dll_compile_and_load(test_c_code, "test_dll", compilation_directory = comp_dir)
  })

  # Test dll_info
  expect_no_error({
    info <- dll_info(lib_handle)
    expect_type(info, "list")
    expect_true("path" %in% names(info))
    expect_equal(info$path, lib_handle)
  })

  # Test symbol loading
  expect_no_error({
    symbol_info <- dll_symbol("test_add_dll")
    expect_type(symbol_info, "list")
    expect_true("address" %in% names(symbol_info))
  })

  # Test dll_is_loaded
  expect_true(dll_is_loaded("test_add_dll"))

  # Test dll_list_loaded
  loaded_libs <- dll_list_loaded()
  expect_type(loaded_libs, "character")
  expect_true(lib_handle %in% loaded_libs)

  # Test creating FFI function from DLL
  expect_no_error({
    add_fn <- dll_ffi_symbol("test_add_dll", int_t, int_t, int_t)
    expect_type(add_fn, "closure") # ffi_function returns a closure
  })

  # Test calling the DLL function via FFI
  expect_no_error({
    add_fn <- dll_ffi_symbol("test_add_dll", int_t, int_t, int_t)
    result <- add_fn(5L, 3L) # Call the closure directly
    expect_equal(result, 8L)
  })

  # Test multiply function too
  expect_no_error({
    multiply_fn <- dll_ffi_symbol("test_multiply_dll", int_t, int_t, int_t)
    result <- multiply_fn(4L, 7L) # Call the closure directly
    expect_equal(result, 28L)
  })

  # Test dll_unload
  expect_no_error({
    result <- dll_unload(lib_handle)
    expect_true(result)
  })

  # After unloading, symbol should not be available
  expect_error({
    dll_symbol("test_add_dll")
  })
})

test_that("DLL loading error handling works", {
  # Test loading nonexistent file
  expect_error(
    {
      dll_load("/nonexistent/path/library.so")
    },
    "Library file not found"
  )

  # Test getting nonexistent symbol
  expect_error(
    {
      dll_symbol("definitely_nonexistent_symbol_12345")
    },
    "not found"
  )

  # Test unloading invalid handle
  expect_warning({
    result <- dll_unload("/nonexistent/path")
    expect_false(result)
  })

  # Test dll_info with invalid handle
  expect_error(
    {
      dll_info("/nonexistent/path")
    },
    "Library not found in loaded DLLs"
  )
})

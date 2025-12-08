test_that("generate_api_offset_extractor creates valid C code", {
  code <- generate_api_offset_extractor("Point2D", c("x", "y"))
  
  expect_true(is.character(code))
  expect_true(grepl("rffi_Point2D_offsets", code))
  expect_true(grepl("offsetof\\(Point2D, x\\)", code))
  expect_true(grepl("offsetof\\(Point2D, y\\)", code))
  expect_true(grepl("Rf_allocVector", code))
})

test_that("generate_api_constructor creates valid C code", {
  code <- generate_api_constructor("Point2D")
  
  expect_true(is.character(code))
  expect_true(grepl("rffi_Point2D_new", code))
  expect_true(grepl("calloc", code))
  expect_true(grepl("R_MakeExternalPtr", code))
  expect_true(grepl("finalizer", code))
})

test_that("generate_api_struct_helpers combines code correctly", {
  code <- generate_api_struct_helpers("Point2D", c("x", "y"))
  
  expect_true(is.character(code))
  expect_true(grepl("#include <R.h>", code))
  expect_true(grepl("rffi_Point2D_new", code))
  expect_true(grepl("rffi_Point2D_offsets", code))
})

test_that("ffi_compile_shlib compiles and loads code", {
  skip_on_cran()
  
  # Generate code (now includes struct typedef automatically)
  code <- generate_api_struct_helpers(
    "Point2D", 
    c("x", "y"),
    field_types = c("int", "int"),  # Explicit C types
    header_includes = NULL
  )
  
  # Compile (no need to add typedef manually)
  lib <- ffi_compile_shlib(code, verbose = FALSE)
  
  expect_s3_class(lib, "rffi_compiled_lib")
  expect_true(file.exists(lib$path))
  expect_true(dir.exists(lib$tmpdir))
  
  # Test getting symbols
  new_fn <- ffi_get_symbol(lib, "rffi_Point2D_new")
  offsets_fn <- ffi_get_symbol(lib, "rffi_Point2D_offsets")
  
  expect_true(is.function(new_fn))
  expect_true(is.function(offsets_fn))
  
  # Test calling functions
  ptr <- new_fn()
  expect_true(inherits(ptr, "externalptr"))
  
  offsets <- offsets_fn()
  expect_true(is.list(offsets))
  expect_equal(names(offsets), c("x", "y"))
  expect_equal(offsets$x, 0)  # x is first field
  expect_equal(offsets$y, 4)  # y follows 4-byte int
  
  # Cleanup
  ffi_cleanup_lib(lib)
})

test_that("compiled helpers integrate with API mode", {
  skip_on_cran()
  
  # Generate complete helper code (includes typedef)
  code <- generate_api_struct_helpers(
    "Point2D", 
    c("x", "y"),
    field_types = c("int", "int")
  )
  
  # Compile
  lib <- ffi_compile_shlib(code)
  
  # Extract functions
  new_fn <- ffi_get_symbol(lib, "rffi_Point2D_new")
  offsets_fn <- ffi_get_symbol(lib, "rffi_Point2D_offsets")
  
  # Use constructor
  ptr <- new_fn()
  expect_true(inherits(ptr, "externalptr"))
  
  # Get offsets
  offsets <- offsets_fn()
  expect_equal(offsets$x, 0)
  expect_equal(offsets$y, 4)
  
  # Use with API mode accessors (direct C calls)
  .Call("R_struct_set_field", ptr, offsets$x, ffi_int()@ref, 42L, PACKAGE = "RSimpleFFI")
  .Call("R_struct_set_field", ptr, offsets$y, ffi_int()@ref, 100L, PACKAGE = "RSimpleFFI")
  
  field_ptr_x <- .Call("R_struct_get_field_ptr", ptr, offsets$x, ffi_int()@ref, PACKAGE = "RSimpleFFI")
  field_ptr_y <- .Call("R_struct_get_field_ptr", ptr, offsets$y, ffi_int()@ref, PACKAGE = "RSimpleFFI")
  
  expect_equal(.Call("R_field_to_r", field_ptr_x, PACKAGE = "RSimpleFFI"), 42L)
  expect_equal(.Call("R_field_to_r", field_ptr_y, PACKAGE = "RSimpleFFI"), 100L)
  
  # Cleanup
  ffi_cleanup_lib(lib)
})


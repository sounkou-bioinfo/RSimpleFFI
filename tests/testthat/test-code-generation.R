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
  
  # Generate helpers for Point2D from test_functions.c
  code <- generate_api_struct_helpers(
    "Point2D", 
    c("x", "y"),
    header_includes = NULL  # Point2D is declared in init.c
  )
  
  # Add Point2D struct definition for compilation
  full_code <- paste0(
    "typedef struct { int x; int y; } Point2D;\n\n",
    code
  )
  
  # Compile
  lib <- ffi_compile_shlib(full_code, verbose = FALSE)
  
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
  
  # Generate and compile helpers
  code <- generate_api_struct_helpers(
    "Point2D",
    c("x", "y")
  )
  
  full_code <- paste0(
    "typedef struct { int x; int y; } Point2D;\n\n",
    code
  )
  
  lib <- ffi_compile_shlib(full_code)
  
  # Get functions
  new_fn <- ffi_get_symbol(lib, "rffi_Point2D_new")
  offsets_fn <- ffi_get_symbol(lib, "rffi_Point2D_offsets")
  
  # Create struct
  ptr <- new_fn()
  
  # Get offsets
  offsets <- offsets_fn()
  
  # Build type metadata for API mode
  Point2D_type <- list(
    fields = list(
      x = list(type = ffi_int(), offset = offsets$x),
      y = list(type = ffi_int(), offset = offsets$y)
    )
  )
  
  # Use API mode accessors
  ffi_set_field(ptr, "x", Point2D_type, 42L)
  ffi_set_field(ptr, "y", Point2D_type, 100L)
  
  x_val <- ffi_get_field(ptr, "x", Point2D_type)
  y_val <- ffi_get_field(ptr, "y", Point2D_type)
  
  expect_equal(x_val, 42L)
  expect_equal(y_val, 100L)
  
  # Cleanup
  ffi_cleanup_lib(lib)
})

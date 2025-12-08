test_that("ffi_create_helpers validates inputs", {
  expect_error(
    ffi_create_helpers(123, list(x = ffi_int())),
    "struct_name must be a single character string"
  )
  
  expect_error(
    ffi_create_helpers("Point", c("not", "a", "list")),
    "field_types must be a named list"
  )
  
  expect_error(
    ffi_create_helpers("Point", list(ffi_int())),  # unnamed
    "field_types must be a named list"
  )
  
  expect_error(
    ffi_create_helpers("Point", list()),  # empty unnamed also triggers named list check
    "field_types must be a named list"
  )
  
  expect_error(
    ffi_create_helpers("Point", structure(list(), names = character(0))),  # empty but named
    "field_types must contain at least one field"
  )
  
  expect_error(
    ffi_create_helpers("Point", list(x = "not a type")),
    "is not an FFIType object"
  )
})

test_that("ffi_create_helpers creates basic struct helpers", {
  helpers <- ffi_create_helpers(
    "Point2D",
    list(x = ffi_int(), y = ffi_int())
  )
  
  expect_s3_class(helpers, "rffi_struct_helpers")
  expect_equal(helpers$struct_name, "Point2D")
  expect_true(is.function(helpers$new))
  expect_true(is.function(helpers$get))
  expect_true(is.function(helpers$set))
  expect_true(is.list(helpers$fields))
  expect_s3_class(helpers$lib, "rffi_compiled_lib")
})

test_that("ffi_create_helpers field metadata is correct", {
  helpers <- ffi_create_helpers(
    "Point2D",
    list(x = ffi_int(), y = ffi_int())
  )
  
  expect_equal(names(helpers$fields), c("x", "y"))
  
  # Check x field metadata
  expect_true(is.list(helpers$fields$x))
  expect_true("offset" %in% names(helpers$fields$x))
  expect_true("type" %in% names(helpers$fields$x))
  expect_equal(helpers$fields$x$offset, 0)  # First field at offset 0
  expect_true(S7::S7_inherits(helpers$fields$x$type, FFIType))
  
  # Check y field metadata
  expect_true(is.list(helpers$fields$y))
  expect_equal(helpers$fields$y$offset, 4)  # Second int at offset 4
  expect_true(S7::S7_inherits(helpers$fields$y$type, FFIType))
})

test_that("ffi_create_helpers constructor works", {
  helpers <- ffi_create_helpers(
    "Point2D",
    list(x = ffi_int(), y = ffi_int())
  )
  
  ptr <- helpers$new()
  
  expect_true(inherits(ptr, "externalptr"))
  expect_false(is.null(ptr))
})

test_that("ffi_create_helpers set and get methods work", {
  helpers <- ffi_create_helpers(
    "Point2D",
    list(x = ffi_int(), y = ffi_int())
  )
  
  ptr <- helpers$new()
  
  # Set fields
  helpers$set(ptr, "x", 42L)
  helpers$set(ptr, "y", 100L)
  
  # Get fields back
  expect_equal(helpers$get(ptr, "x"), 42L)
  expect_equal(helpers$get(ptr, "y"), 100L)
})

test_that("ffi_create_helpers handles different types", {
  helpers <- ffi_create_helpers(
    "Mixed",
    list(
      i = ffi_int(),
      d = ffi_double(),
      f = ffi_float()
    )
  )
  
  ptr <- helpers$new()
  
  # Set different types
  helpers$set(ptr, "i", 42L)
  helpers$set(ptr, "d", 3.14)
  helpers$set(ptr, "f", 2.5)
  
  # Get back
  expect_equal(helpers$get(ptr, "i"), 42L)
  expect_equal(helpers$get(ptr, "d"), 3.14, tolerance = 1e-10)
  expect_equal(helpers$get(ptr, "f"), 2.5, tolerance = 1e-6)
})

test_that("ffi_create_helpers validates field names", {
  helpers <- ffi_create_helpers(
    "Point2D",
    list(x = ffi_int(), y = ffi_int())
  )
  
  ptr <- helpers$new()
  
  expect_error(
    helpers$get(ptr, "z"),
    "Unknown field: z"
  )
  
  expect_error(
    helpers$set(ptr, "z", 42L),
    "Unknown field: z"
  )
})

test_that("ffi_create_helpers print method works", {
  helpers <- ffi_create_helpers(
    "Point2D",
    list(x = ffi_int(), y = ffi_int())
  )
  
  output <- capture.output(print(helpers))
  expect_match(output[1], "rffi_struct_helpers: Point2D")
  expect_match(output[2], "Fields \\(2\\)")
  expect_match(paste(output, collapse = "\n"), "x: offset=0")
  expect_match(paste(output, collapse = "\n"), "y: offset=4")
})

test_that("ffi_create_helpers handles struct with many fields", {
  helpers <- ffi_create_helpers(
    "LargeStruct",
    list(
      a = ffi_int(),
      b = ffi_int(),
      c = ffi_int(),
      d = ffi_int(),
      e = ffi_int()
    )
  )
  
  ptr <- helpers$new()
  
  # Set all fields
  helpers$set(ptr, "a", 1L)
  helpers$set(ptr, "b", 2L)
  helpers$set(ptr, "c", 3L)
  helpers$set(ptr, "d", 4L)
  helpers$set(ptr, "e", 5L)
  
  # Verify all
  expect_equal(helpers$get(ptr, "a"), 1L)
  expect_equal(helpers$get(ptr, "b"), 2L)
  expect_equal(helpers$get(ptr, "c"), 3L)
  expect_equal(helpers$get(ptr, "d"), 4L)
  expect_equal(helpers$get(ptr, "e"), 5L)
  
  # Check offsets are sequential (4 bytes per int)
  expect_equal(helpers$fields$a$offset, 0)
  expect_equal(helpers$fields$b$offset, 4)
  expect_equal(helpers$fields$c$offset, 8)
  expect_equal(helpers$fields$d$offset, 12)
  expect_equal(helpers$fields$e$offset, 16)
})

test_that("ffi_create_helpers with alignment padding", {
  # struct { char c; int i; } has padding between c and i
  helpers <- ffi_create_helpers(
    "Padded",
    list(
      c = ffi_char(),
      i = ffi_int()
    )
  )
  
  ptr <- helpers$new()
  
  helpers$set(ptr, "c", 65L)  # 'A'
  helpers$set(ptr, "i", 42L)
  
  expect_equal(helpers$get(ptr, "c"), 65L)
  expect_equal(helpers$get(ptr, "i"), 42L)
  
  # Check that offset of i reflects alignment (usually 4 on most platforms)
  expect_true(helpers$fields$i$offset >= 4)
})

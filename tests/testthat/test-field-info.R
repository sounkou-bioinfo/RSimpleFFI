# Tests for field info and struct introspection

test_that("ffi_field_info returns FieldInfo object", {
  Point <- ffi_struct(x = ffi_int(), y = ffi_int())

  info <- ffi_field_info(Point, "x")
  expect_s3_class(info, "RSimpleFFI::FieldInfo")
  expect_equal(info@name, "x")
  expect_equal(info@offset, 0L)
  expect_equal(info@size, 4L)
  expect_equal(info@index, 1L)
})

test_that("ffi_field_info works with field index", {
  Point <- ffi_struct(x = ffi_int(), y = ffi_int())

  info1 <- ffi_field_info(Point, 1L)
  info2 <- ffi_field_info(Point, 2L)

  expect_equal(info1@name, "x")
  expect_equal(info2@name, "y")
})

test_that("ffi_field_info shows alignment padding", {
  # int (4) + padding (4) + double (8)
  Mixed <- ffi_struct(a = ffi_int(), b = ffi_double())

  info_a <- ffi_field_info(Mixed, "a")
  info_b <- ffi_field_info(Mixed, "b")

  expect_equal(info_a@offset, 0L)
  expect_equal(info_a@size, 4L)

  # double should be at offset 8 due to 8-byte alignment

  expect_equal(info_b@offset, 8L)
  expect_equal(info_b@size, 8L)
})

test_that("ffi_field_info errors on invalid field", {
  Point <- ffi_struct(x = ffi_int(), y = ffi_int())

  expect_error(ffi_field_info(Point, "z"), "No such field")
  expect_error(ffi_field_info(Point, 3L), "out of range")
  expect_error(ffi_field_info(Point, 0L), "out of range")
})

test_that("ffi_offsetof returns correct offsets", {
  Point <- ffi_struct(x = ffi_int(), y = ffi_int())

  expect_equal(ffi_offsetof(Point, "x"), 0L)
  expect_equal(ffi_offsetof(Point, "y"), 4L)
})

test_that("ffi_offsetof with alignment padding", {
  Mixed <- ffi_struct(a = ffi_int(), b = ffi_double())

  expect_equal(ffi_offsetof(Mixed, "a"), 0L)
  expect_equal(ffi_offsetof(Mixed, "b"), 8L) # Padded for 8-byte alignment
})

test_that("ffi_all_offsets returns named vector", {
  Point <- ffi_struct(x = ffi_int(), y = ffi_int())

  offsets <- ffi_all_offsets(Point)

  expect_type(offsets, "integer")
  expect_named(offsets, c("x", "y"))
  expect_equal(offsets[["x"]], 0L)
  expect_equal(offsets[["y"]], 4L)
})

test_that("ffi_all_offsets with multiple fields and padding", {
  # char (1) + padding (3) + int (4) + padding (0) + double (8) = 16
  Complex <- ffi_struct(
    c = ffi_char(),
    i = ffi_int(),
    d = ffi_double()
  )

  offsets <- ffi_all_offsets(Complex)

  expect_equal(offsets[["c"]], 0L)
  expect_equal(offsets[["i"]], 4L) # Aligned to 4-byte boundary
  expect_equal(offsets[["d"]], 8L) # Aligned to 8-byte boundary
})

test_that("FieldInfo format method works", {
  Point <- ffi_struct(x = ffi_int(), y = ffi_int())
  info <- ffi_field_info(Point, "x")

  formatted <- format(info)
  expect_type(formatted, "character")
  expect_match(formatted, "FieldInfo")
  expect_match(formatted, "x")
  expect_match(formatted, "int")
  expect_match(formatted, "offset=0")
})

test_that("ffi_field_info validates struct_type", {
  expect_error(ffi_field_info(ffi_int(), "x"), "StructType")
  expect_error(ffi_field_info("not a type", "x"), "StructType")
})

test_that("ffi_all_offsets validates struct_type", {
  expect_error(ffi_all_offsets(ffi_int()), "StructType")
})

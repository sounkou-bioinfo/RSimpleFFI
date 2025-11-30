test_that("ffi_raw type can be created and has correct properties", {
  raw_t <- ffi_raw()
  expect_s7_class(raw_t, RSimpleFFI::FFIType)
  expect_equal(raw_t@name, "raw")
  expect_gt(raw_t@size, 0)
})
test_that("All extended FFI types can be created and have correct properties", {
  type_fns <- list(
    char = ffi_char,
    uchar = ffi_uchar,
    short = ffi_short,
    ushort = ffi_ushort,
    int8 = ffi_int8,
    int16 = ffi_int16,
    int32 = ffi_int32,
    int64 = ffi_int64,
    uint = ffi_uint,
    uint8 = ffi_uint8,
    uint16 = ffi_uint16,
    uint32 = ffi_uint32,
    uint64 = ffi_uint64,
    long = ffi_long,
    ulong = ffi_ulong,
    longlong = ffi_longlong,
    ulonglong = ffi_ulonglong,
    longdouble = ffi_longdouble,
    size_t = ffi_size_t,
    ssize_t = ffi_ssize_t,
    bool = ffi_bool,
    wchar_t = ffi_wchar_t
  )
  for (type_name in names(type_fns)) {
    t <- type_fns[[type_name]]()
    expect_s7_class(t, RSimpleFFI::FFIType)
    expect_equal(t@name, type_name)
    expect_gt(t@size, 0)
  }
})
# Basic Type Tests

library(testthat)
library(RSimpleFFI)

test_that("Built-in types can be created", {
  # Test type creation
  void_t <- ffi_void()
  int_t <- ffi_int()
  double_t <- ffi_double()
  float_t <- ffi_float()
  pointer_t <- ffi_pointer()
  string_t <- ffi_string()

  # Check properties
  expect_s7_class(void_t, RSimpleFFI::FFIType)
  expect_s7_class(int_t, RSimpleFFI::FFIType)
  expect_s7_class(double_t, RSimpleFFI::FFIType)
  expect_s7_class(float_t, RSimpleFFI::FFIType)
  expect_s7_class(pointer_t, RSimpleFFI::FFIType)
  expect_s7_class(string_t, RSimpleFFI::FFIType)

  # Check names
  expect_equal(void_t@name, "void")
  expect_equal(int_t@name, "int")
  expect_equal(double_t@name, "double")
  expect_equal(float_t@name, "float")
  expect_equal(pointer_t@name, "pointer")
  expect_equal(string_t@name, "string")

  # Check sizes are reasonable
  expect_gt(int_t@size, 0)
  expect_gt(double_t@size, int_t@size) # double usually larger than int
  expect_gt(pointer_t@size, 0)
})

test_that("Type size function works", {
  int_t <- ffi_int()
  double_t <- ffi_double()

  expect_equal(ffi_sizeof(int_t), int_t@size)
  expect_equal(ffi_sizeof(double_t), double_t@size)
})

test_that("Invalid type names are rejected", {
  expect_error(create_builtin_type("invalid_type"), "Unknown FFI type")
  expect_error(create_builtin_type(""), "Unknown FFI type")
})

test_that("Type validation works", {
  # Invalid ref (must be external pointer)
  expect_error(
    FFIType(name = "test", size = 8L, ref = NULL),
    "@ref must be an external pointer"
  )

  # Invalid name (not length 1)
  expect_error(
    {
      FFIType(name = c("a", "b"), size = 8L, ref = NULL)
    },
    "@name must be length 1"
  )

  # Invalid size (not positive)
  expect_error(
    {
      FFIType(name = "test", size = 0L, ref = NULL)
    },
    "@size must be positive"
  )
})

test_that("Pretty printing works", {
  int_t <- ffi_int()
  # Test format method which works reliably
  formatted_int <- format(int_t)
  expect_match(formatted_int, "FFIType\\(int, size=\\d+\\)")

  double_t <- ffi_double()
  formatted <- format(double_t)
  expect_match(formatted, "FFIType\\(double, size=\\d+\\)")
})

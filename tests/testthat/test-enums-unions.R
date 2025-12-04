test_that("enum type creation works", {
  Color <- ffi_enum(RED = 0L, GREEN = 1L, BLUE = 2L)

  expect_true(S7::S7_inherits(Color, EnumType))
  expect_true(S7::S7_inherits(Color, FFIType))
  expect_equal(Color@values, c(RED = 0L, GREEN = 1L, BLUE = 2L))
  expect_equal(names(Color@values), c("RED", "GREEN", "BLUE"))
  expect_true(S7::S7_inherits(Color@underlying_type, FFIType))
})

test_that("enum with custom underlying type works", {
  # Using uint8 for small enums
  SmallEnum <- ffi_enum(A = 0L, B = 1L, underlying_type = ffi_uint8())

  expect_equal(SmallEnum@size, 1L)
  expect_equal(SmallEnum@underlying_type@name, "uint8")
})

test_that("enum allocation works", {
  Status <- ffi_enum(OK = 0L, ERROR = 1L, PENDING = 2L)

  # Single enum value
  ptr <- ffi_alloc(Status)
  expect_true(inherits(ptr, "externalptr"))

  # Array of enum values
  arr <- ffi_alloc(Status, 5L)
  expect_true(inherits(arr, "externalptr"))
})

test_that("enum value conversions work", {
  Color <- ffi_enum(RED = 0L, GREEN = 1L, BLUE = 2L)

  # Name to int
  expect_equal(ffi_enum_to_int(Color, "RED"), 0L)
  expect_equal(ffi_enum_to_int(Color, "GREEN"), 1L)
  expect_equal(ffi_enum_to_int(Color, "BLUE"), 2L)

  # Int to name
  expect_equal(ffi_int_to_enum(Color, 0L), "RED")
  expect_equal(ffi_int_to_enum(Color, 1L), "GREEN")
  expect_equal(ffi_int_to_enum(Color, 2L), "BLUE")

  # Not found
  expect_true(is.na(ffi_int_to_enum(Color, 99L)))

  # Errors
  expect_error(ffi_enum_to_int(Color, "YELLOW"), "No such enum constant")
})

test_that("struct can contain enum fields", {
  Status <- ffi_enum(OK = 0L, ERROR = 1L, PENDING = 2L)

  Result <- ffi_struct(
    status = Status,
    code = ffi_int()
  )

  ptr <- ffi_alloc(Result)

  # Set enum value using integer
  ffi_set_field(ptr, "status", ffi_enum_to_int(Status, "ERROR"), Result)
  ffi_set_field(ptr, "code", 42L, Result)

  # Get values back
  status_val <- ffi_get_field(ptr, "status", Result)
  code_val <- ffi_get_field(ptr, "code", Result)

  expect_equal(status_val, 1L)
  expect_equal(code_val, 42L)
  expect_equal(ffi_int_to_enum(Status, status_val), "ERROR")
})

test_that("array of enums works", {
  Priority <- ffi_enum(LOW = 0L, MEDIUM = 1L, HIGH = 2L)

  # Create array type
  PriorityArray <- ffi_array_type(Priority, 3L)

  expect_equal(PriorityArray@element_type@name, "enum")
  expect_equal(PriorityArray@length, 3L)

  # Allocate array
  arr <- ffi_alloc(PriorityArray)
  expect_true(inherits(arr, "externalptr"))
})

test_that("union type creation works", {
  Value <- ffi_union(
    i = ffi_int(),
    d = ffi_double(),
    c = ffi_uint8()
  )

  expect_true(S7::S7_inherits(Value, UnionType))
  expect_true(S7::S7_inherits(Value, FFIType))
  expect_equal(Value@fields, c("i", "d", "c"))
  expect_equal(length(Value@field_types), 3L)

  # Union size should be at least as large as largest member (double = 8)
  expect_gte(Value@size, 8L)
})

test_that("union allocation works", {
  Value <- ffi_union(
    i = ffi_int(),
    f = ffi_float()
  )

  # Single union
  ptr <- ffi_alloc(Value)
  expect_true(inherits(ptr, "externalptr"))

  # Array of unions
  arr <- ffi_alloc(Value, 3L)
  expect_true(inherits(arr, "externalptr"))
})

test_that("union field access works with character field name", {
  Value <- ffi_union(
    i = ffi_int(),
    d = ffi_double()
  )

  ptr <- ffi_alloc(Value)

  # Set as integer
  ffi_set_field(ptr, "i", 42L, Value)
  val_i <- ffi_get_field(ptr, "i", Value)
  expect_equal(val_i, 42L)

  # Set as double (overwrites the same memory)
  ffi_set_field(ptr, "d", 3.14, Value)
  val_d <- ffi_get_field(ptr, "d", Value)
  expect_equal(val_d, 3.14, tolerance = 1e-10)

  # Reading as int after setting as double gives reinterpreted bits
  # (this is expected union behavior)
  val_i2 <- ffi_get_field(ptr, "i", Value)
  expect_true(is.integer(val_i2))
})

test_that("union field access works with integer field index", {
  Value <- ffi_union(
    x = ffi_int(),
    y = ffi_float()
  )

  ptr <- ffi_alloc(Value)

  # Set using index
  ffi_set_field(ptr, 1L, 100L, Value)
  val1 <- ffi_get_field(ptr, 1L, Value)
  expect_equal(val1, 100L)

  # Field 2
  ffi_set_field(ptr, 2L, 2.5, Value)
  val2 <- ffi_get_field(ptr, 2L, Value)
  expect_equal(val2, 2.5, tolerance = 1e-6)
})

test_that("union with enum field works", {
  Status <- ffi_enum(IDLE = 0L, RUNNING = 1L, STOPPED = 2L)

  State <- ffi_union(
    status = Status,
    counter = ffi_int()
  )

  ptr <- ffi_alloc(State)

  # Set as status enum
  ffi_set_field(ptr, "status", ffi_enum_to_int(Status, "RUNNING"), State)
  status_val <- ffi_get_field(ptr, "status", State)
  expect_equal(status_val, 1L)
  expect_equal(ffi_int_to_enum(Status, status_val), "RUNNING")

  # Set as counter (overwrites)
  ffi_set_field(ptr, "counter", 42L, State)
  counter_val <- ffi_get_field(ptr, "counter", State)
  expect_equal(counter_val, 42L)
})

test_that("struct with union field works", {
  Value <- ffi_union(
    i = ffi_int(),
    f = ffi_float()
  )

  Tagged <- ffi_struct(
    tag = ffi_int(),
    value = Value
  )

  ptr <- ffi_alloc(Tagged)

  # Set tag
  ffi_set_field(ptr, "tag", 1L, Tagged)

  # Get the union field
  union_ptr <- ffi_get_field(ptr, "value", Tagged)
  expect_true(inherits(union_ptr, "externalptr"))

  # Set union's int field through nested access
  ffi_set_field(union_ptr, "i", 123L, Value)
  val <- ffi_get_field(union_ptr, "i", Value)
  expect_equal(val, 123L)
})

test_that("union getter/setter error handling works", {
  Value <- ffi_union(x = ffi_int(), y = ffi_double())
  ptr <- ffi_alloc(Value)

  # Invalid field name
  expect_error(
    ffi_get_field(ptr, "z", Value),
    "No such field 'z' in union"
  )
  expect_error(
    ffi_set_field(ptr, "z", 1L, Value),
    "No such field 'z' in union"
  )

  # Out of range index
  expect_error(
    ffi_get_field(ptr, 0L, Value),
    "Field index out of range"
  )
  expect_error(
    ffi_get_field(ptr, 10L, Value),
    "Field index out of range"
  )
  expect_error(
    ffi_set_field(ptr, 0L, 1L, Value),
    "Field index out of range"
  )
  expect_error(
    ffi_set_field(ptr, 10L, 1L, Value),
    "Field index out of range"
  )
})

test_that("enum allocation validates inputs", {
  Status <- ffi_enum(OK = 0L, ERROR = 1L)

  expect_error(ffi_alloc(Status, 0L), "n must be a positive integer")
  expect_error(ffi_alloc(Status, -1L), "n must be a positive integer")
  expect_error(ffi_alloc(Status, c(1L, 2L)), "n must be a positive integer")
})

test_that("enum helper functions validate inputs", {
  Color <- ffi_enum(RED = 0L, GREEN = 1L)

  # Wrong type for enum_type
  expect_error(
    ffi_enum_to_int(ffi_int(), "RED"),
    "enum_type must be an EnumType"
  )
  expect_error(
    ffi_int_to_enum(ffi_int(), 0L),
    "enum_type must be an EnumType"
  )

  # Wrong type for name/value
  expect_error(
    ffi_enum_to_int(Color, 1L),
    "name must be a single character string"
  )
  expect_error(
    ffi_enum_to_int(Color, c("RED", "GREEN")),
    "name must be a single character string"
  )
  expect_error(
    ffi_int_to_enum(Color, "RED"),
    "value must be a single integer"
  )
  expect_error(
    ffi_int_to_enum(Color, c(0L, 1L)),
    "value must be a single integer"
  )
})

test_that("packed union creation works", {
  # Normal union has alignment = 4 (from int)
  U_normal <- ffi_union(c = ffi_char(), i = ffi_int())
  expect_equal(ffi_sizeof(U_normal), 4L)
  expect_null(U_normal@pack)

  # Packed union has alignment = 1 but same size
  U_packed <- ffi_union(c = ffi_char(), i = ffi_int(), .pack = 1L)
  expect_equal(ffi_sizeof(U_packed), 4L)
  expect_equal(U_packed@pack, 1L)
})

test_that("packed union affects struct layout", {
  # Normal union in struct
  U_normal <- ffi_union(c = ffi_char(), i = ffi_int())
  S1 <- ffi_struct(u = U_normal, after = ffi_char())

  # Struct with normal union has trailing padding (size 8)
  expect_equal(ffi_sizeof(S1), 8L)
  expect_equal(ffi_offsetof(S1, "after"), 4L)

  # Packed union in struct
  U_packed <- ffi_union(c = ffi_char(), i = ffi_int(), .pack = 1L)
  S2 <- ffi_struct(u = U_packed, after = ffi_char())

  # Struct with packed union has no trailing padding (size 5)
  expect_equal(ffi_sizeof(S2), 5L)
  expect_equal(ffi_offsetof(S2, "after"), 4L)
})

test_that("packed union validates .pack parameter", {
  expect_error(
    ffi_union(x = ffi_int(), .pack = 0L),
    ".pack must be NULL or an integer between 1 and 16"
  )
  expect_error(
    ffi_union(x = ffi_int(), .pack = 3L),
    ".pack must be a power of 2"
  )
  expect_error(
    ffi_union(x = ffi_int(), .pack = 32L),
    ".pack must be NULL or an integer between 1 and 16"
  )
})

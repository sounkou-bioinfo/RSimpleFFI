test_that("hFILE-like struct with mixed regular fields and bitfields works", {
  # This is the KEY test case that motivated API mode!
  # hFILE-like struct from htslib with:
  # - Regular pointer fields (buffer, begin, end, limit)
  # - Bitfield section (has_errno:1, is_pipe:1, mobile:1, etc.)
  # - Regular fields after bitfields (offset, backend)
  #
  # Problem: Can't use offset reflection because bitfields can't have their address taken
  # Solution: API mode generates C code that uses offsetof() - compiler knows layout
  
  helpers <- ffi_create_helpers(
    "hFILE_like",
    list(
      buffer = ffi_pointer(),
      begin = ffi_pointer(),
      end_ptr = ffi_pointer(),
      limit = ffi_pointer(),
      bitfield_flags = ffi_uint(),
      offset_field = ffi_int(),
      backend = ffi_pointer()
    )
  )
  
  ptr <- helpers$new()
  
  # Set regular pointer fields
  null_ptr <- ffi_alloc(ffi_int(), 1)
  helpers$set(ptr, "buffer", null_ptr)
  helpers$set(ptr, "begin", null_ptr)
  helpers$set(ptr, "end_ptr", null_ptr)
  helpers$set(ptr, "limit", null_ptr)
  
  # Set bitfield flags as packed integer
  helpers$set(ptr, "bitfield_flags", 21L)
  
  # Set regular fields after bitfields
  helpers$set(ptr, "offset_field", 1024L)
  helpers$set(ptr, "backend", null_ptr)
  
  # Verify all reads work
  expect_true(inherits(helpers$get(ptr, "buffer"), "externalptr"))
  expect_true(inherits(helpers$get(ptr, "begin"), "externalptr"))
  expect_true(inherits(helpers$get(ptr, "end_ptr"), "externalptr"))
  expect_true(inherits(helpers$get(ptr, "limit"), "externalptr"))
  expect_equal(helpers$get(ptr, "bitfield_flags"), 21L)
  expect_equal(helpers$get(ptr, "offset_field"), 1024L)
  expect_true(inherits(helpers$get(ptr, "backend"), "externalptr"))
  
  # Verify field metadata
  expect_equal(length(helpers$fields), 7)
  
  # All offsets should be non-negative and properly aligned
  for (field_name in names(helpers$fields)) {
    expect_true(helpers$fields[[field_name]]$offset >= 0)
  }
  
  # Verify pointer fields come first
  expect_equal(helpers$fields$buffer$offset, 0)
  
  # Bitfield comes after 4 pointers
  expect_true(helpers$fields$bitfield_flags$offset >= 32)
  
  # offset_field and backend come after bitfield
  expect_true(helpers$fields$offset_field$offset > helpers$fields$bitfield_flags$offset)
  expect_true(helpers$fields$backend$offset > helpers$fields$offset_field$offset)
})

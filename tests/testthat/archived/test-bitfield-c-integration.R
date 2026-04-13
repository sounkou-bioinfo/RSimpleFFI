test_that("bit-field helpers work with actual C functions", {
  skip_if_not(TRUE, "Bitfield C functions available")

  # Test: Pack in R, verify in C
  settings <- ffi_create_bitfield_accessors(
    list(enabled = 1L, mode = 3L, priority = 4L, reserved = 24L)
  )

  # Pack values in R
  packed <- settings$pack(list(
    enabled = 1L,
    mode = 5L,
    priority = 10L,
    reserved = 0L
  ))

  # Create C functions to verify
  get_enabled <- ffi_function(
    "test_bitfield_get_enabled",
    ffi_int(),
    ffi_uint32()
  )
  get_mode <- ffi_function("test_bitfield_get_mode", ffi_int(), ffi_uint32())
  get_priority <- ffi_function(
    "test_bitfield_get_priority",
    ffi_int(),
    ffi_uint32()
  )

  # C should read the same values we packed
  expect_equal(get_enabled(as.integer(packed)), 1L)
  expect_equal(get_mode(as.integer(packed)), 5L)
  expect_equal(get_priority(as.integer(packed)), 10L)
})

test_that("C-packed bit-fields can be unpacked in R", {
  skip_if_not(TRUE, "Bitfield C functions available")

  # Pack in C
  pack_func <- ffi_function(
    "test_bitfield_pack",
    ffi_uint32(),
    ffi_int(),
    ffi_int(),
    ffi_int()
  )

  c_packed <- pack_func(1L, 5L, 10L)

  # Unpack in R
  settings <- ffi_create_bitfield_accessors(
    list(enabled = 1L, mode = 3L, priority = 4L, reserved = 24L)
  )

  unpacked <- settings$unpack(c_packed)

  expect_equal(unpacked$enabled, 1L)
  expect_equal(unpacked$mode, 5L)
  expect_equal(unpacked$priority, 10L)
  expect_equal(unpacked$reserved, 0L)
})

test_that("bit-field modifications round-trip through C", {
  skip_if_not(TRUE, "Bitfield C functions available")

  settings <- ffi_create_bitfield_accessors(
    list(enabled = 1L, mode = 3L, priority = 4L, reserved = 24L)
  )

  # Start with R-packed value
  packed <- settings$pack(list(
    enabled = 1L,
    mode = 3L,
    priority = 5L,
    reserved = 0L
  ))

  # Modify in C (increment priority)
  increment_func <- ffi_function(
    "test_bitfield_increment_priority",
    ffi_uint32(),
    ffi_uint32()
  )

  packed <- increment_func(as.integer(packed))

  # Verify in R
  expect_equal(settings$get(packed, "priority"), 6L)
  expect_equal(settings$get(packed, "enabled"), 1L) # unchanged
  expect_equal(settings$get(packed, "mode"), 3L) # unchanged

  # Toggle in C
  toggle_func <- ffi_function(
    "test_bitfield_toggle_enabled",
    ffi_uint32(),
    ffi_uint32()
  )

  packed <- toggle_func(as.integer(packed))

  # Verify toggle
  expect_equal(settings$get(packed, "enabled"), 0L)
  expect_equal(settings$get(packed, "priority"), 6L) # still unchanged
})

test_that("C function can verify R-packed bit-fields", {
  skip_if_not(TRUE, "Bitfield C functions available")

  settings <- ffi_create_bitfield_accessors(
    list(enabled = 1L, mode = 3L, priority = 4L, reserved = 24L)
  )

  # Pack in R
  packed <- settings$pack(list(
    enabled = 1L,
    mode = 7L,
    priority = 12L,
    reserved = 0L
  ))

  # Verify in C
  verify_func <- ffi_function(
    "test_bitfield_verify",
    ffi_int(),
    ffi_uint32(),
    ffi_int(),
    ffi_int(),
    ffi_int()
  )

  # Should match
  result <- verify_func(as.integer(packed), 1L, 7L, 12L)
  expect_equal(result, 1L)

  # Should not match wrong values
  result <- verify_func(as.integer(packed), 0L, 7L, 12L) # wrong enabled
  expect_equal(result, 0L)

  result <- verify_func(as.integer(packed), 1L, 5L, 12L) # wrong mode
  expect_equal(result, 0L)
})

test_that("smaller bit-field structs work (8-bit)", {
  skip_if_not(TRUE, "Bitfield C functions available")

  # TCP-like flags: syn, ack, fin, rst (1 bit each)
  packet <- ffi_create_bitfield_accessors(
    list(syn = 1L, ack = 1L, fin = 1L, rst = 1L, reserved = 4L)
  )

  # Create SYN+ACK in R
  syn_ack <- packet$pack(list(
    syn = 1L,
    ack = 1L,
    fin = 0L,
    rst = 0L,
    reserved = 0L
  ))

  # Verify in C
  has_ack <- ffi_function("test_packet_has_ack", ffi_int(), ffi_uint8())
  expect_equal(has_ack(as.integer(syn_ack)), 1L)

  # Create in C, read in R
  create_synack <- ffi_function("test_packet_create_synack", ffi_uint8())
  c_syn_ack <- create_synack()

  expect_equal(packet$get(c_syn_ack, "syn"), 1L)
  expect_equal(packet$get(c_syn_ack, "ack"), 1L)
  expect_equal(packet$get(c_syn_ack, "fin"), 0L)
  expect_equal(packet$get(c_syn_ack, "rst"), 0L)

  # Count flags
  count_func <- ffi_function("test_packet_count_flags", ffi_int(), ffi_uint8())

  # SYN+ACK should have 2 flags set
  expect_equal(count_func(as.integer(c_syn_ack)), 2L)

  # Create FIN+ACK+RST in R
  fin_ack_rst <- packet$pack(list(
    syn = 0L,
    ack = 1L,
    fin = 1L,
    rst = 1L,
    reserved = 0L
  ))
  expect_equal(count_func(as.integer(fin_ack_rst)), 3L)
})

test_that("extreme bit-field values are handled correctly", {
  skip_if_not(TRUE, "Bitfield C functions available")

  settings <- ffi_create_bitfield_accessors(
    list(enabled = 1L, mode = 3L, priority = 4L, reserved = 24L)
  )

  verify_func <- ffi_function(
    "test_bitfield_verify",
    ffi_int(),
    ffi_uint32(),
    ffi_int(),
    ffi_int(),
    ffi_int()
  )

  # Test maximum values for each field
  # enabled: max = 1 (1 bit)
  # mode: max = 7 (3 bits = 0b111)
  # priority: max = 15 (4 bits = 0b1111)

  packed <- settings$pack(list(
    enabled = 1L,
    mode = 7L,
    priority = 15L,
    reserved = 0L
  ))
  expect_equal(verify_func(as.integer(packed), 1L, 7L, 15L), 1L)

  # Test zero values
  packed <- settings$pack(list(
    enabled = 0L,
    mode = 0L,
    priority = 0L,
    reserved = 0L
  ))
  expect_equal(verify_func(as.integer(packed), 0L, 0L, 0L), 1L)

  # Test that overflow is masked correctly
  # (values > max should wrap due to masking)
  packed <- settings$pack(list(
    enabled = 2L,
    mode = 8L,
    priority = 16L,
    reserved = 0L
  ))
  # 2 & 0x1 = 0, 8 & 0x7 = 0, 16 & 0xF = 0
  expect_equal(verify_func(as.integer(packed), 0L, 0L, 0L), 1L)
})

test_that("ffi_pack_bits works correctly", {
  # Simple case: 3 fields
  # enabled=1 (bit 0), mode=5 (bits 1-3), priority=12 (bits 4-7)
  # Binary: 11001011 = 203
  packed <- ffi_pack_bits(c(1L, 5L, 12L), c(1L, 3L, 4L))
  expect_equal(packed, 203L)

  # Edge case: single bit
  packed <- ffi_pack_bits(1L, 1L)
  expect_equal(packed, 1L)

  # Multiple bits
  packed <- ffi_pack_bits(c(1L, 1L, 1L, 1L), c(1L, 1L, 1L, 1L))
  expect_equal(packed, 15L) # 0b1111
})

test_that("ffi_unpack_bits works correctly", {
  # Simple case (203 = 0b11001011)
  values <- ffi_unpack_bits(203L, c(1L, 3L, 4L))
  expect_equal(values, c(1L, 5L, 12L))

  # Single bit
  values <- ffi_unpack_bits(1L, 1L)
  expect_equal(values, 1L)

  # Multiple bits
  values <- ffi_unpack_bits(15L, c(1L, 1L, 1L, 1L))
  expect_equal(values, c(1L, 1L, 1L, 1L))
})

test_that("pack/unpack round-trip works", {
  original <- c(1L, 5L, 12L)
  widths <- c(1L, 3L, 4L)

  packed <- ffi_pack_bits(original, widths)
  unpacked <- ffi_unpack_bits(packed, widths)

  expect_equal(unpacked, original)
})

test_that("ffi_extract_bit_field works correctly", {
  packed <- 203L # 0b11001011

  # Extract 1-bit field at offset 0
  expect_equal(ffi_extract_bit_field(packed, 0L, 1L), 1L)

  # Extract 3-bit field at offset 1 (bits 1-3 = 101 = 5)
  expect_equal(ffi_extract_bit_field(packed, 1L, 3L), 5L)

  # Extract 4-bit field at offset 4 (bits 4-7 = 1100 = 12)
  expect_equal(ffi_extract_bit_field(packed, 4L, 4L), 12L)
})

test_that("ffi_set_bit_field works correctly", {
  packed <- 203L # 0b11001011

  # Set 3-bit field at offset 1 to 7 (0b111)
  # Original: 11001011, change bits 1-3 to 111
  # Result:   11001111 = 207
  new_packed <- ffi_set_bit_field(packed, 7L, 1L, 3L)
  expect_equal(ffi_extract_bit_field(new_packed, 1L, 3L), 7L)

  # Verify other fields unchanged
  expect_equal(ffi_extract_bit_field(new_packed, 0L, 1L), 1L)
  expect_equal(ffi_extract_bit_field(new_packed, 4L, 4L), 12L)

  # Set 1-bit field at offset 0 to 0
  # Original: 11001011, change bit 0 to 0
  # Result:   11001010 = 202
  new_packed <- ffi_set_bit_field(packed, 0L, 0L, 1L)
  expect_equal(ffi_extract_bit_field(new_packed, 0L, 1L), 0L)
  expect_equal(new_packed, 202L)
})

test_that("ffi_create_bitfield_accessors works", {
  # Create accessors for a structure
  accessors <- ffi_create_bitfield_accessors(
    list(enabled = 1L, mode = 3L, priority = 4L)
  )

  # Test pack (enabled=1, mode=5, priority=12 -> 203)
  packed <- accessors$pack(list(enabled = 1L, mode = 5L, priority = 12L))
  expect_equal(packed, 203L)

  # Test unpack
  unpacked <- accessors$unpack(packed)
  expect_equal(unpacked$enabled, 1L)
  expect_equal(unpacked$mode, 5L)
  expect_equal(unpacked$priority, 12L)

  # Test get
  expect_equal(accessors$get(packed, "enabled"), 1L)
  expect_equal(accessors$get(packed, "mode"), 5L)
  expect_equal(accessors$get(packed, "priority"), 12L)

  # Test set
  new_packed <- accessors$set(packed, "mode", 7L)
  expect_equal(accessors$get(new_packed, "mode"), 7L)
  expect_equal(accessors$get(new_packed, "enabled"), 1L) # unchanged
  expect_equal(accessors$get(new_packed, "priority"), 12L) # unchanged
})

test_that("bitfield helpers validate input", {
  expect_error(ffi_pack_bits(c(1L, 2L), c(1L))) # mismatched lengths
  expect_error(ffi_pack_bits(1L, 0L)) # zero width
  expect_error(ffi_pack_bits(1L, -1L)) # negative width

  expect_error(ffi_unpack_bits(1L, 0L)) # zero width
  expect_error(ffi_unpack_bits(1L, -1L)) # negative width

  expect_error(ffi_extract_bit_field(1L, 0L, 0L)) # zero width
  expect_error(ffi_extract_bit_field(1L, -1L, 1L)) # negative offset

  expect_error(ffi_set_bit_field(1L, 1L, 0L, 0L)) # zero width
  expect_error(ffi_set_bit_field(1L, 1L, -1L, 1L)) # negative offset
})

test_that("bitfield accessors validate input", {
  expect_error(
    ffi_create_bitfield_accessors(c(1, 2, 3)), # not a named list
    "named list"
  )

  expect_error(
    ffi_create_bitfield_accessors(list(1L, b = 2L)), # unnamed field
    "named"
  )

  accessors <- ffi_create_bitfield_accessors(list(a = 1L, b = 2L))

  expect_error(
    accessors$get(1L, "unknown"),
    "Unknown field"
  )

  expect_error(
    accessors$set(1L, "unknown", 1L),
    "Unknown field"
  )
})

test_that("bitfield helpers work with realistic examples", {
  # Example: network packet flags
  # struct PacketFlags {
  #   unsigned int syn : 1;
  #   unsigned int ack : 1;
  #   unsigned int fin : 1;
  #   unsigned int rst : 1;
  #   unsigned int reserved : 4;
  # };

  packet_flags <- ffi_create_bitfield_accessors(
    list(syn = 1L, ack = 1L, fin = 1L, rst = 1L, reserved = 4L)
  )

  # Create SYN+ACK packet
  flags <- packet_flags$pack(list(
    syn = 1L,
    ack = 1L,
    fin = 0L,
    rst = 0L,
    reserved = 0L
  ))
  expect_equal(packet_flags$get(flags, "syn"), 1L)
  expect_equal(packet_flags$get(flags, "ack"), 1L)
  expect_equal(packet_flags$get(flags, "fin"), 0L)

  # Set FIN flag
  flags <- packet_flags$set(flags, "fin", 1L)
  expect_equal(packet_flags$get(flags, "fin"), 1L)

  # Verify round-trip
  unpacked <- packet_flags$unpack(flags)
  expect_equal(unpacked$syn, 1L)
  expect_equal(unpacked$ack, 1L)
  expect_equal(unpacked$fin, 1L)
  expect_equal(unpacked$rst, 0L)
})

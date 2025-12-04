# Tests for 64-bit bitfield operations

test_that("ffi_pack_bits64 packs values correctly", {
  # Simple case: 1 + 3 + 4 = 8 bits
  # enabled=1 (1 bit), mode=5 (3 bits), priority=12 (4 bits)
  # Result: 1 | (5 << 1) | (12 << 4) = 1 + 10 + 192 = 203
  packed <- ffi_pack_bits64(c(1L, 5L, 12L), c(1L, 3L, 4L))
  expect_equal(packed, 203)
})

test_that("ffi_unpack_bits64 unpacks values correctly", {
  packed <- ffi_pack_bits64(c(1L, 5L, 12L), c(1L, 3L, 4L))
  unpacked <- ffi_unpack_bits64(packed, c(1L, 3L, 4L))
  expect_equal(unpacked, c(1L, 5L, 12L))
})

test_that("ffi_pack_bits64 handles 32+ bit totals", {
  # Pack a 1-bit field and a 33-bit total (exceeds 32-bit R integer)
  packed <- ffi_pack_bits64(c(1L, 0x7FFFFFFFL), c(1L, 31L))
  expect_true(packed > 0)

  # Unpack and verify
  unpacked <- ffi_unpack_bits64(packed, c(1L, 31L))
  expect_equal(unpacked[1], 1L)
  expect_equal(unpacked[2], 0x7FFFFFFFL)
})

test_that("ffi_extract_bits64 extracts single fields", {
  packed <- ffi_pack_bits64(c(1L, 5L, 12L), c(1L, 3L, 4L))

  expect_equal(ffi_extract_bits64(packed, 0L, 1L), 1) # enabled

  expect_equal(ffi_extract_bits64(packed, 1L, 3L), 5) # mode
  expect_equal(ffi_extract_bits64(packed, 4L, 4L), 12) # priority
})

test_that("ffi_set_bits64 modifies single fields", {
  packed <- ffi_pack_bits64(c(1L, 5L, 12L), c(1L, 3L, 4L))

  # Change mode from 5 to 7
  new_packed <- ffi_set_bits64(packed, 7, 1L, 3L)
  expect_equal(ffi_extract_bits64(new_packed, 1L, 3L), 7)

  # Other fields unchanged
  expect_equal(ffi_extract_bits64(new_packed, 0L, 1L), 1)
  expect_equal(ffi_extract_bits64(new_packed, 4L, 4L), 12)
})

test_that("ffi_extract_signed_bits64 handles negative values", {
  # Pack 13 (0xD) in 4 bits - this represents -3 as signed 4-bit
  packed <- ffi_pack_bits64(c(13L), c(4L))

  # Unsigned extraction gives 13
  expect_equal(ffi_extract_bits64(packed, 0L, 4L), 13)

  # Signed extraction gives -3
  expect_equal(ffi_extract_signed_bits64(packed, 0L, 4L), -3)

  # Pack 7 (0b111) in 3 bits - represents -1 as signed 3-bit
  packed2 <- ffi_pack_bits64(c(7L), c(3L))
  expect_equal(ffi_extract_signed_bits64(packed2, 0L, 3L), -1)

  # Positive value stays positive
  packed3 <- ffi_pack_bits64(c(3L), c(4L)) # 3 fits in 4 signed bits
  expect_equal(ffi_extract_signed_bits64(packed3, 0L, 4L), 3)
})

test_that("ffi_extract_signed_bit_field (32-bit) handles negative values", {
  # Pack 13 in 4 bits
  packed <- ffi_pack_bits(c(13L), c(4L))

  # Signed extraction gives -3
  expect_equal(ffi_extract_signed_bit_field(packed, 0L, 4L), -3)

  # Pack 7 in 3 bits
  packed2 <- ffi_pack_bits(c(7L), c(3L))
  expect_equal(ffi_extract_signed_bit_field(packed2, 0L, 3L), -1)
})

test_that("64-bit operations match C bitfield behavior", {
  skip_if_not(exists("ffi_symbol")) # Need FFI to be loaded

  # Test against C functions
  sym_pack <- ffi_symbol("test_signed_bitfield_pack")
  sym_get4 <- ffi_symbol("test_signed_bitfield_get4")
  sym_get5 <- ffi_symbol("test_signed_bitfield_get5")

  cif_pack <- ffi_cif(ffi_uint16(), ffi_int(), ffi_int())
  cif_get <- ffi_cif(ffi_int(), ffi_uint16())

  # Pack -3 (4-bit) and -5 (5-bit)
  c_packed <- ffi_call(cif_pack, sym_pack, -3L, -5L)

  # Verify C extraction works
  c_val4 <- ffi_call(cif_get, sym_get4, c_packed)
  c_val5 <- ffi_call(cif_get, sym_get5, c_packed)

  expect_equal(c_val4, -3L)
  expect_equal(c_val5, -5L)

  # Our R extraction of the 4-bit field at offset 0 should match C
  r_val4 <- ffi_extract_signed_bits64(as.double(c_packed), 0L, 4L)
  expect_equal(r_val4, -3)
})

test_that("validation errors are raised", {
  expect_error(ffi_pack_bits64(c(1L), c(0L)), "positive")
  expect_error(ffi_pack_bits64(c(1L, 2L), c(1L)), "match")
  expect_error(ffi_pack_bits64(c(1L), c(65L)), "exceeds 64")

  expect_error(ffi_extract_bits64(1, -1L, 1L), "non-negative")
  expect_error(ffi_extract_bits64(1, 0L, 0L), "positive")
})

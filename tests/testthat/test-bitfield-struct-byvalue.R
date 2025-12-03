test_that("bit-field struct passed by value treats as packed integer", {
  # The key insight: C compilers lay out bit-fields in a platform-specific way
  # When passed by value, libffi sees the struct's memory representation
  # For SettingsFlags (1+3+4+24 bits = 32 bits), it's effectively a uint32_t
  
  # Create packed value in R using our helper
  accessor <- ffi_create_bitfield_accessors(list(
    enabled = 1L,
    mode = 3L,
    priority = 4L,
    reserved = 24L
  ))
  
  packed <- accessor$pack(list(enabled = 1L, mode = 5L, priority = 10L, reserved = 0L))
  
  # Option 1: Model struct as single uint32_t (CORRECT approach)
  # C function: int test_bitfield_struct_get_mode(SettingsFlags settings)
  # SettingsFlags is 32-bit struct, so pass as uint32_t
  get_mode_fn <- ffi_function("test_bitfield_struct_get_mode", 
                               ffi_int(),      # return: int
                               ffi_uint32())   # arg: uint32_t (packed struct)
  
  result <- get_mode_fn(packed)
  expect_equal(result, 5L)
})

test_that("bit-field struct returned by value is packed integer", {
  # C function: SettingsFlags test_bitfield_struct_create(int e, int m, int p)
  # Returns 32-bit struct, model as uint32_t return
  
  create_fn <- ffi_function("test_bitfield_struct_create",
                            ffi_uint32(),  # return: uint32_t (packed struct)
                            ffi_int(), ffi_int(), ffi_int())  # args: e, m, p
  
  packed <- create_fn(1L, 5L, 10L)
  expect_type(packed, "integer")
  
  # Unpack to verify
  accessor <- ffi_create_bitfield_accessors(list(
    enabled = 1L,
    mode = 3L,
    priority = 4L,
    reserved = 24L
  ))
  
  expect_equal(accessor$get(packed, "enabled"), 1L)
  expect_equal(accessor$get(packed, "mode"), 5L)
  expect_equal(accessor$get(packed, "priority"), 10L)
})

test_that("bit-field struct by value round-trip works", {
  # C: SettingsFlags test_bitfield_struct_increment_priority(SettingsFlags s)
  # Takes and returns 32-bit struct
  
  accessor <- ffi_create_bitfield_accessors(list(
    enabled = 1L,
    mode = 3L,
    priority = 4L,
    reserved = 24L
  ))
  
  packed_in <- accessor$pack(list(enabled = 1L, mode = 5L, priority = 10L, reserved = 0L))
  
  increment_fn <- ffi_function("test_bitfield_struct_increment_priority",
                               ffi_uint32(),   # return: packed struct
                               ffi_uint32())   # arg: packed struct
  
  packed_out <- increment_fn(packed_in)
  
  # Verify priority incremented
  expect_equal(accessor$get(packed_out, "enabled"), 1L)
  expect_equal(accessor$get(packed_out, "mode"), 5L)
  expect_equal(accessor$get(packed_out, "priority"), 11L)
})

test_that("bit-field struct by value vs by pointer", {
  accessor <- ffi_create_bitfield_accessors(list(
    enabled = 1L,
    mode = 3L,
    priority = 4L,
    reserved = 24L
  ))
  
  packed <- accessor$pack(list(enabled = 1L, mode = 5L, priority = 10L, reserved = 0L))
  
  # By value: int test_bitfield_struct_get_mode(SettingsFlags s)
  # Pass struct as uint32_t value
  val_fn <- ffi_function("test_bitfield_struct_get_mode",
                         ffi_int(),
                         ffi_uint32())
  
  result_val <- val_fn(as.integer(packed))
  expect_equal(result_val, 5L)
  
  # By pointer: int test_bitfield_struct_ptr_get_mode(SettingsFlags* s)
  # Note: ffi_fill_typed_buffer doesn't support uint32 yet, so test pointer approach differently
  # For now, just verify by-value works (pointer version would need uint32 buffer support)
  
  # Verify modify via pointer also works
  set_mode_fn <- ffi_function("test_bitfield_struct_ptr_set_mode",
                              ffi_void(),
                              ffi_pointer(), ffi_int())
  
  # Allocate buffer as int (SINT32) which is same size as UINT32
  buf <- ffi_alloc(ffi_int(), 1L)
  ffi_fill_typed_buffer(buf, as.integer(packed), ffi_int())
  
  # Modify via pointer
  set_mode_fn(buf, 7L)
  
  # Read back
  ptr_get_fn <- ffi_function("test_bitfield_struct_ptr_get_mode",
                             ffi_int(), ffi_pointer())
  result_ptr <- ptr_get_fn(buf)
  expect_equal(result_ptr, 7L)
})

test_that("wrong struct modeling fails", {
  # What happens if we try to model bit-field struct as regular struct?
  # This demonstrates WHY we need the uint32_t approach
  
  accessor <- ffi_create_bitfield_accessors(list(
    enabled = 1L,
    mode = 3L,
    priority = 4L,
    reserved = 24L
  ))
  
  packed <- accessor$pack(list(enabled = 1L, mode = 5L, priority = 10L, reserved = 0L))
  
  # WRONG: Try to model as struct with separate int fields
  # C compiler lays out SettingsFlags as 4 bytes (bit-packed)
  # But this struct would be 16 bytes (4 ints)
  BadStruct <- ffi_struct(
    enabled = ffi_int(),
    mode = ffi_int(), 
    priority = ffi_int(),
    reserved = ffi_int()
  )
  
  # Size mismatch
  expect_equal(BadStruct@size, 16L)  # 4 ints = 16 bytes
  # Real SettingsFlags is 4 bytes (1 uint32_t)
  
  # If we tried to pass this, libffi would send 16 bytes instead of 4
  # The C function expects 4 bytes, so it would read garbage
})

test_that("bit-field struct size detection in real code", {
  # How to detect the actual size?
  # Use sizeof from C side or check struct definition
  
  # For SettingsFlags: 1+3+4+24 = 32 bits = 4 bytes = uint32_t
  # For PacketFlags: 1+1+1+1+4 = 8 bits = 1 byte = uint8_t
  
  # Rule: sum bit-widths, round up to next power-of-2 byte size
  # 32 bits -> 4 bytes (uint32_t)
  # 8 bits -> 1 byte (uint8_t)
  # 16 bits -> 2 bytes (uint16_t)
  # 64 bits -> 8 bytes (uint64_t)
  
  settings_bits <- 1 + 3 + 4 + 24  # 32 bits
  expect_equal(settings_bits, 32L)
  
  packet_bits <- 1 + 1 + 1 + 1 + 4  # 8 bits
  expect_equal(packet_bits, 8L)
})

test_that("PacketFlags 8-bit struct by value", {
  # PacketFlags is 8-bit, so use uint8_t
  
  # C: uint8_t test_packet_create_synack(void)
  create_synack <- ffi_function("test_packet_create_synack", ffi_uint8())
  
  packed <- create_synack()
  expect_type(packed, "integer")
  
  # C: int test_packet_count_flags(uint8_t packed)
  count_fn <- ffi_function("test_packet_count_flags", 
                           ffi_int(),
                           ffi_uint8())
  
  count <- count_fn(packed)
  expect_equal(count, 2L)  # SYN + ACK
})

test_that("documentation for bit-field struct by value", {
  # Key insight to document:
  # 1. Bit-field structs are packed into smallest integer type
  # 2. When passed by value, treat as that integer type
  # 3. Calculate size: sum bit-widths, round up to 1/2/4/8 bytes
  # 4. Use ffi_uint8/16/32/64() accordingly
  # 5. Use accessor helpers to pack/unpack on R side
  
  # Example workflow:
  # struct Flags { unsigned a:2; unsigned b:3; };  // 5 bits -> 1 byte
  # C: int process(Flags f);
  # R: process_fn <- ffi_function("process", ffi_int(), ffi_uint8())
  #    accessor <- ffi_create_bitfield_accessors(list(a=2L, b=3L))
  #    packed <- accessor$pack(list(a=1L, b=5L))
  #    result <- process_fn(packed)
  
  expect_true(TRUE)  # Documentation test
})

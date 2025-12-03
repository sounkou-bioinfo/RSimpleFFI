# test_packed_structs.R - Test packed struct support in RSimpleFFI
#
# This script demonstrates that RSimpleFFI's pack parameter correctly
# matches C's #pragma pack behavior for struct alignment.
#
# Usage:
#   source(system.file("extdata", "test_packed_structs.R", package = "RSimpleFFI"))
#
# Or run from command line:
#   Rscript inst/extdata/test_packed_structs.R

library(RSimpleFFI)

cat("=== Testing Packed Struct Support ===\n\n")

# Compile the test C code
c_file <- system.file("extdata", "packed_structs.c", package = "RSimpleFFI")
if (c_file == "") {
    c_file <- "inst/extdata/packed_structs.c"
}

if (!file.exists(c_file)) {
    stop("Cannot find packed_structs.c")
}

cat("Compiling packed_structs.c...\n")
lib_path <- dll_compile_and_load(
    readLines(c_file) |> paste(collapse = "\n"),
    "packed_structs_test"
)
cat("Compiled successfully!\n\n")

# Cleanup on exit
on.exit(dll_unload(lib_path), add = TRUE)

# ============================================================
# Test 1: Verify C struct sizes match R struct sizes
# ============================================================
cat("--- Test 1: Struct Size Verification ---\n")

# Get sizes from C
c_natural_size <- dll_ffi_symbol("get_natural_struct_size", ffi_int())()
c_packed1_size <- dll_ffi_symbol("get_packed1_struct_size", ffi_int())()
c_packed2_size <- dll_ffi_symbol("get_packed2_struct_size", ffi_int())()
c_complex_size <- dll_ffi_symbol("get_complex_packed_size", ffi_int())()

cat(sprintf("C NaturalStruct size:  %d bytes\n", c_natural_size))
cat(sprintf("C PackedStruct1 size:  %d bytes (pack=1)\n", c_packed1_size))
cat(sprintf("C PackedStruct2 size:  %d bytes (pack=2)\n", c_packed2_size))
cat(sprintf("C ComplexPacked size:  %d bytes (pack=1)\n", c_complex_size))

# Create equivalent R structs
NaturalStruct <- ffi_struct(a = ffi_int32(), b = ffi_double())
PackedStruct1 <- ffi_struct(a = ffi_int32(), b = ffi_double(), pack = 1)
PackedStruct2 <- ffi_struct(a = ffi_int32(), b = ffi_double(), pack = 2)
ComplexPacked <- ffi_struct(
    flags = ffi_uint8(),
    id = ffi_uint32(),
    value = ffi_uint16(),
    data = ffi_double(),
    pack = 1
)

r_natural_size <- ffi_sizeof(NaturalStruct)
r_packed1_size <- ffi_sizeof(PackedStruct1)
r_packed2_size <- ffi_sizeof(PackedStruct2)
r_complex_size <- ffi_sizeof(ComplexPacked)

cat(sprintf("\nR NaturalStruct size:  %d bytes\n", r_natural_size))
cat(sprintf("R PackedStruct1 size:  %d bytes (pack=1)\n", r_packed1_size))
cat(sprintf("R PackedStruct2 size:  %d bytes (pack=2)\n", r_packed2_size))
cat(sprintf("R ComplexPacked size:  %d bytes (pack=1)\n", r_complex_size))

stopifnot(c_natural_size == r_natural_size)
stopifnot(c_packed1_size == r_packed1_size)
stopifnot(c_packed2_size == r_packed2_size)
stopifnot(c_complex_size == r_complex_size)

cat("\n✓ All struct sizes match!\n\n")

# ============================================================
# Test 2: Verify field offsets
# ============================================================
cat("--- Test 2: Field Offset Verification ---\n")

# Get offsets from C
c_packed1_offset_a <- dll_ffi_symbol("get_packed1_offset_a", ffi_int())()
c_packed1_offset_b <- dll_ffi_symbol("get_packed1_offset_b", ffi_int())()

cat(sprintf("C PackedStruct1 offset a: %d\n", c_packed1_offset_a))
cat(sprintf("C PackedStruct1 offset b: %d\n", c_packed1_offset_b))

r_packed1_offsets <- ffi_all_offsets(PackedStruct1)
cat(sprintf("R PackedStruct1 offset a: %d\n", r_packed1_offsets["a"]))
cat(sprintf("R PackedStruct1 offset b: %d\n", r_packed1_offsets["b"]))

stopifnot(c_packed1_offset_a == r_packed1_offsets["a"])
stopifnot(c_packed1_offset_b == r_packed1_offsets["b"])

# Complex struct offsets
c_complex_offset_flags <- dll_ffi_symbol("get_complex_offset_flags", ffi_int())()
c_complex_offset_id <- dll_ffi_symbol("get_complex_offset_id", ffi_int())()
c_complex_offset_value <- dll_ffi_symbol("get_complex_offset_value", ffi_int())()
c_complex_offset_data <- dll_ffi_symbol("get_complex_offset_data", ffi_int())()

cat(sprintf(
    "\nC ComplexPacked offsets: flags=%d, id=%d, value=%d, data=%d\n",
    c_complex_offset_flags, c_complex_offset_id,
    c_complex_offset_value, c_complex_offset_data
))

r_complex_offsets <- ffi_all_offsets(ComplexPacked)
cat(sprintf(
    "R ComplexPacked offsets: flags=%d, id=%d, value=%d, data=%d\n",
    r_complex_offsets["flags"], r_complex_offsets["id"],
    r_complex_offsets["value"], r_complex_offsets["data"]
))

stopifnot(c_complex_offset_flags == r_complex_offsets["flags"])
stopifnot(c_complex_offset_id == r_complex_offsets["id"])
stopifnot(c_complex_offset_value == r_complex_offsets["value"])
stopifnot(c_complex_offset_data == r_complex_offsets["data"])

cat("\n✓ All field offsets match!\n\n")

# ============================================================
# Test 3: Pass packed struct to C function
# ============================================================
cat("--- Test 3: Pass Packed Struct to C ---\n")

# Allocate and fill packed struct
packed_ptr <- ffi_alloc(PackedStruct1)
ffi_set_field(packed_ptr, "a", 42L, PackedStruct1)
ffi_set_field(packed_ptr, "b", 3.14, PackedStruct1)

# Call C functions to read values
get_a <- dll_ffi_symbol("get_packed1_a", ffi_int32(), ffi_pointer())
get_b <- dll_ffi_symbol("get_packed1_b", ffi_double(), ffi_pointer())
sum_fn <- dll_ffi_symbol("sum_packed1", ffi_double(), ffi_pointer())

c_a <- get_a(packed_ptr)
c_b <- get_b(packed_ptr)
c_sum <- sum_fn(packed_ptr)

cat(sprintf("Set in R:    a=%d, b=%.2f\n", 42L, 3.14))
cat(sprintf("Read from C: a=%d, b=%.2f\n", c_a, c_b))
cat(sprintf("Sum from C:  %.2f (expected %.2f)\n", c_sum, 42 + 3.14))

stopifnot(c_a == 42L)
stopifnot(abs(c_b - 3.14) < 0.001)
stopifnot(abs(c_sum - 45.14) < 0.001)

cat("\n✓ Packed struct passed correctly to C!\n\n")

# ============================================================
# Test 4: C function writes to packed struct
# ============================================================
cat("--- Test 4: C Writes to Packed Struct ---\n")

set_fn <- dll_ffi_symbol(
    "set_packed1", ffi_void(),
    ffi_pointer(), ffi_int32(), ffi_double()
)

# Have C set values
set_fn(packed_ptr, 100L, 99.99)

# Read back in R
r_a <- ffi_get_field(packed_ptr, "a", PackedStruct1)
r_b <- ffi_get_field(packed_ptr, "b", PackedStruct1)

cat(sprintf("C set:      a=%d, b=%.2f\n", 100L, 99.99))
cat(sprintf("R reads:    a=%d, b=%.2f\n", r_a, r_b))

stopifnot(r_a == 100L)
stopifnot(abs(r_b - 99.99) < 0.001)

cat("\n✓ C correctly wrote to packed struct!\n\n")

# ============================================================
# Test 5: Complex packed struct
# ============================================================
cat("--- Test 5: Complex Packed Struct ---\n")

complex_ptr <- ffi_alloc(ComplexPacked)

# Set from C
set_complex <- dll_ffi_symbol(
    "set_complex", ffi_void(),
    ffi_pointer(), ffi_uint8(), ffi_uint32(),
    ffi_uint16(), ffi_double()
)
set_complex(complex_ptr, 0xFF, 12345678L, 9999L, 2.71828)

# Read in R
r_flags <- ffi_get_field(complex_ptr, "flags", ComplexPacked)
r_id <- ffi_get_field(complex_ptr, "id", ComplexPacked)
r_value <- ffi_get_field(complex_ptr, "value", ComplexPacked)
r_data <- ffi_get_field(complex_ptr, "data", ComplexPacked)

cat(sprintf(
    "C set:   flags=0x%02X, id=%d, value=%d, data=%.5f\n",
    0xFF, 12345678L, 9999L, 2.71828
))
cat(sprintf(
    "R reads: flags=0x%02X, id=%d, value=%d, data=%.5f\n",
    r_flags, r_id, r_value, r_data
))

stopifnot(r_flags == 255L)
stopifnot(r_id == 12345678L)
stopifnot(r_value == 9999L)
stopifnot(abs(r_data - 2.71828) < 0.00001)

# Also verify via C getters
get_flags <- dll_ffi_symbol("get_complex_flags", ffi_uint8(), ffi_pointer())
get_id <- dll_ffi_symbol("get_complex_id", ffi_uint32(), ffi_pointer())
get_value <- dll_ffi_symbol("get_complex_value", ffi_uint16(), ffi_pointer())
get_data <- dll_ffi_symbol("get_complex_data", ffi_double(), ffi_pointer())

# Set from R, read from C
ffi_set_field(complex_ptr, "flags", 0x42L, ComplexPacked)
ffi_set_field(complex_ptr, "id", 87654321L, ComplexPacked)
ffi_set_field(complex_ptr, "value", 1234L, ComplexPacked)
ffi_set_field(complex_ptr, "data", 1.41421, ComplexPacked)

c_flags <- get_flags(complex_ptr)
c_id <- get_id(complex_ptr)
c_value <- get_value(complex_ptr)
c_data <- get_data(complex_ptr)

cat(sprintf(
    "\nR set:   flags=0x%02X, id=%d, value=%d, data=%.5f\n",
    0x42L, 87654321L, 1234L, 1.41421
))
cat(sprintf(
    "C reads: flags=0x%02X, id=%d, value=%d, data=%.5f\n",
    c_flags, c_id, c_value, c_data
))

stopifnot(c_flags == 0x42L)
stopifnot(c_id == 87654321L)
stopifnot(c_value == 1234L)
stopifnot(abs(c_data - 1.41421) < 0.00001)

cat("\n✓ Complex packed struct works correctly!\n\n")

# ============================================================
# Test 6: Modify packed struct (in -> out)
# ============================================================
cat("--- Test 6: Modify Packed Struct ---\n")

in_ptr <- ffi_alloc(PackedStruct1)
out_ptr <- ffi_alloc(PackedStruct1)

ffi_set_field(in_ptr, "a", 50L, PackedStruct1)
ffi_set_field(in_ptr, "b", 25.0, PackedStruct1)

modify_fn <- dll_ffi_symbol(
    "modify_packed1", ffi_void(),
    ffi_pointer(), ffi_pointer()
)
modify_fn(in_ptr, out_ptr)

out_a <- ffi_get_field(out_ptr, "a", PackedStruct1)
out_b <- ffi_get_field(out_ptr, "b", PackedStruct1)

cat(sprintf("Input:  a=%d, b=%.1f\n", 50L, 25.0))
cat(sprintf("Output: a=%d, b=%.1f (expected a=100, b=125.0)\n", out_a, out_b))

stopifnot(out_a == 100L)
stopifnot(abs(out_b - 125.0) < 0.001)

cat("\n✓ Struct modification works correctly!\n\n")

# ============================================================
# Summary
# ============================================================
cat("=" |> rep(50) |> paste(collapse = ""), "\n")
cat("ALL PACKED STRUCT TESTS PASSED!\n")
cat("=" |> rep(50) |> paste(collapse = ""), "\n\n")

cat("Summary:\n")
cat("- ffi_struct(..., pack=N) correctly computes struct sizes\
")
cat("- Field offsets match C's #pragma pack behavior\n")
cat("- Packed structs can be passed to C functions by pointer\n")
cat("- C can read/write fields in packed structs allocated by R\n")
cat("- R can read/write fields in packed structs modified by C\n")

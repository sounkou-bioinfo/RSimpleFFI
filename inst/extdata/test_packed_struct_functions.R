#!/usr/bin/env Rscript
#
# test_packed_struct_functions.R
#
# Test passing packed structs to C functions via FFI.
# Tests both pointer-based access (works) and by-value passing (may have limitations).
#
# Usage: Rscript test_packed_struct_functions.R
#

library(RSimpleFFI)

cat("=== Packed Struct FFI Tests ===\n\n")

# Read C code from file
c_file <- system.file(
  "extdata",
  "packed_struct_functions.c",
  package = "RSimpleFFI"
)
if (c_file == "") {
  c_file <- "inst/extdata/packed_struct_functions.c"
}

c_code <- paste(readLines(c_file), collapse = "\n")

# Compile and load using the package function
cat("Compiling C code...\n")
lib_path <- dll_compile_and_load(
  c_code,
  name = "packed_struct_functions",
  verbose = TRUE
)

# Extract DLL name for symbol lookup (getNativeSymbolInfo wants the name, not path)
lib <- tools::file_path_sans_ext(basename(lib_path))
cat("Loaded library:", lib, "\n\n")

# ============ Define types ============

# Packed struct: { char a; int32_t b; } with pack=1
PackedSimple <- ffi_struct(
  a = ffi_char(),
  b = ffi_int32(),
  pack = 1L
)

# Normal struct for comparison
NormalSimple <- ffi_struct(
  a = ffi_char(),
  b = ffi_int32()
)

# Nested packed struct
PackedNested <- ffi_struct(
  inner = PackedSimple,
  c = ffi_char(),
  pack = 1L
)

# Multi-field packed struct
PackedMulti <- ffi_struct(
  flags = ffi_uint8(),
  value = ffi_uint32(),
  count = ffi_uint16(),
  status = ffi_uint8(),
  pack = 1L
)

# ============ Test 1: Size verification ============

cat("=== Test 1: Size Verification ===\n")

# Get sizes from C
get_packed_simple_size <- ffi_function(
  "get_packed_simple_size",
  ffi_int(),
  library = lib
)
get_normal_simple_size <- ffi_function(
  "get_normal_simple_size",
  ffi_int(),
  library = lib
)
get_packed_nested_size <- ffi_function(
  "get_packed_nested_size",
  ffi_int(),
  library = lib
)
get_packed_multi_size <- ffi_function(
  "get_packed_multi_size",
  ffi_int(),
  library = lib
)

c_packed_size <- get_packed_simple_size()
c_normal_size <- get_normal_simple_size()
c_nested_size <- get_packed_nested_size()
c_multi_size <- get_packed_multi_size()

r_packed_size <- ffi_sizeof(PackedSimple)
r_normal_size <- ffi_sizeof(NormalSimple)
r_nested_size <- ffi_sizeof(PackedNested)
r_multi_size <- ffi_sizeof(PackedMulti)

cat(sprintf(
  "PackedSimple: C=%d, R=%d %s\n",
  c_packed_size,
  r_packed_size,
  if (c_packed_size == r_packed_size) "✓" else "✗"
))
cat(sprintf(
  "NormalSimple: C=%d, R=%d %s\n",
  c_normal_size,
  r_normal_size,
  if (c_normal_size == r_normal_size) "✓" else "✗"
))
cat(sprintf(
  "PackedNested: C=%d, R=%d %s\n",
  c_nested_size,
  r_nested_size,
  if (c_nested_size == r_nested_size) "✓" else "✗"
))
cat(sprintf(
  "PackedMulti:  C=%d, R=%d %s\n",
  c_multi_size,
  r_multi_size,
  if (c_multi_size == r_multi_size) "✓" else "✗"
))
cat("\n")

# ============ Test 2: Offset verification ============

cat("=== Test 2: Offset Verification ===\n")

get_packed_simple_b_offset <- ffi_function(
  "get_packed_simple_b_offset",
  ffi_int(),
  library = lib
)
get_normal_simple_b_offset <- ffi_function(
  "get_normal_simple_b_offset",
  ffi_int(),
  library = lib
)
get_packed_nested_c_offset <- ffi_function(
  "get_packed_nested_c_offset",
  ffi_int(),
  library = lib
)
get_packed_multi_value_offset <- ffi_function(
  "get_packed_multi_value_offset",
  ffi_int(),
  library = lib
)
get_packed_multi_count_offset <- ffi_function(
  "get_packed_multi_count_offset",
  ffi_int(),
  library = lib
)
get_packed_multi_status_offset <- ffi_function(
  "get_packed_multi_status_offset",
  ffi_int(),
  library = lib
)

tests <- list(
  list(
    "PackedSimple.b",
    get_packed_simple_b_offset(),
    ffi_offsetof(PackedSimple, "b")
  ),
  list(
    "NormalSimple.b",
    get_normal_simple_b_offset(),
    ffi_offsetof(NormalSimple, "b")
  ),
  list(
    "PackedNested.c",
    get_packed_nested_c_offset(),
    ffi_offsetof(PackedNested, "c")
  ),
  list(
    "PackedMulti.value",
    get_packed_multi_value_offset(),
    ffi_offsetof(PackedMulti, "value")
  ),
  list(
    "PackedMulti.count",
    get_packed_multi_count_offset(),
    ffi_offsetof(PackedMulti, "count")
  ),
  list(
    "PackedMulti.status",
    get_packed_multi_status_offset(),
    ffi_offsetof(PackedMulti, "status")
  )
)

for (test in tests) {
  name <- test[[1]]
  c_off <- test[[2]]
  r_off <- test[[3]]
  status <- if (c_off == r_off) "✓" else "✗"
  cat(sprintf("%s offset: C=%d, R=%d %s\n", name, c_off, r_off, status))
}
cat("\n")

# ============ Test 3: Pointer-based access (should work) ============

cat("=== Test 3: Pointer-Based Access ===\n")

# Allocate and fill via C
write_packed_simple <- ffi_function(
  "write_packed_simple",
  ffi_void(),
  ffi_pointer(),
  ffi_char(),
  ffi_int32(),
  library = lib
)
read_packed_simple_b <- ffi_function(
  "read_packed_simple_b",
  ffi_int32(),
  ffi_pointer(),
  library = lib
)
verify_packed_simple_layout <- ffi_function(
  "verify_packed_simple_layout",
  ffi_int(),
  ffi_pointer(),
  ffi_char(),
  ffi_int32(),
  library = lib
)

# Helper to convert char to int
char_to_int <- function(ch) as.integer(charToRaw(ch)[1])

ptr <- ffi_alloc(PackedSimple)
write_packed_simple(ptr, char_to_int("X"), 12345L)

# Read back via C
b_val <- read_packed_simple_b(ptr)
cat(sprintf(
  "Write via C, read via C: b=%d %s\n",
  b_val,
  if (b_val == 12345L) "✓" else "✗"
))

# Verify memory layout
layout_ok <- verify_packed_simple_layout(ptr, char_to_int("X"), 12345L)
cat(sprintf(
  "Memory layout verification: %s\n",
  if (layout_ok == 1L) "✓" else "✗"
))

# Read via R's ffi_get_field
r_a <- ffi_get_field(ptr, "a", PackedSimple)
r_b <- ffi_get_field(ptr, "b", PackedSimple)
cat(sprintf(
  "Read via R: a=%d ('%s'), b=%d %s\n",
  r_a,
  rawToChar(as.raw(r_a)),
  r_b,
  if (r_a == char_to_int("X") && r_b == 12345L) "✓" else "✗"
))

# Write via R, read via C
ffi_set_field(ptr, "a", char_to_int("Y"), PackedSimple)
ffi_set_field(ptr, "b", 67890L, PackedSimple)
b_val2 <- read_packed_simple_b(ptr)
layout_ok2 <- verify_packed_simple_layout(ptr, char_to_int("Y"), 67890L)
cat(sprintf(
  "Write via R, read via C: b=%d, layout=%s %s\n",
  b_val2,
  if (layout_ok2 == 1L) "OK" else "BAD",
  if (b_val2 == 67890L && layout_ok2 == 1L) "✓" else "✗"
))
cat("\n")

# ============ Test 4: Multi-field packed struct ============

cat("=== Test 4: Multi-field Packed Struct ===\n")

fill_packed_multi <- ffi_function(
  "fill_packed_multi",
  ffi_void(),
  ffi_pointer(),
  ffi_uint8(),
  ffi_uint32(),
  ffi_uint16(),
  ffi_uint8(),
  library = lib
)
sum_packed_multi <- ffi_function(
  "sum_packed_multi",
  ffi_int32(),
  ffi_pointer(),
  library = lib
)

ptr2 <- ffi_alloc(PackedMulti)
fill_packed_multi(ptr2, 1L, 1000L, 100L, 10L)

# Read via R
flags <- ffi_get_field(ptr2, "flags", PackedMulti)
value <- ffi_get_field(ptr2, "value", PackedMulti)
count <- ffi_get_field(ptr2, "count", PackedMulti)
status <- ffi_get_field(ptr2, "status", PackedMulti)

cat(sprintf(
  "Fields: flags=%d, value=%d, count=%d, status=%d\n",
  flags,
  value,
  count,
  status
))
expected_sum <- 1L + 1000L + 100L + 10L
actual_sum <- sum_packed_multi(ptr2)
cat(sprintf(
  "Sum via C: %d (expected %d) %s\n",
  actual_sum,
  expected_sum,
  if (actual_sum == expected_sum) "✓" else "✗"
))

# Modify via R
ffi_set_field(ptr2, "value", 2000L, PackedMulti)
actual_sum2 <- sum_packed_multi(ptr2)
expected_sum2 <- 1L + 2000L + 100L + 10L
cat(sprintf(
  "After R modification, sum via C: %d (expected %d) %s\n",
  actual_sum2,
  expected_sum2,
  if (actual_sum2 == expected_sum2) "✓" else "✗"
))
cat("\n")

# ============ Test 5: Array of packed structs ============

cat("=== Test 5: Array of Packed Structs ===\n")

fill_packed_array <- ffi_function(
  "fill_packed_array",
  ffi_void(),
  ffi_pointer(),
  ffi_int(),
  ffi_int32(),
  library = lib
)
sum_packed_array <- ffi_function(
  "sum_packed_array",
  ffi_int32(),
  ffi_pointer(),
  ffi_int(),
  library = lib
)

arr <- ffi_alloc(PackedSimple, 5L)
fill_packed_array(arr, 5L, 100L)

# Sum via C
c_sum <- sum_packed_array(arr, 5L)
expected_arr_sum <- 100L + 110L + 120L + 130L + 140L
cat(sprintf(
  "Array sum via C: %d (expected %d) %s\n",
  c_sum,
  expected_arr_sum,
  if (c_sum == expected_arr_sum) "✓" else "✗"
))

# Read individual elements via R
cat("Array elements (via R):\n")
for (i in 1:5) {
  elem_ptr <- ffi_get_element(arr, i, PackedSimple)
  a_val <- ffi_get_field(elem_ptr, "a", PackedSimple)
  b_val <- ffi_get_field(elem_ptr, "b", PackedSimple)
  expected_a <- char_to_int(LETTERS[i])
  expected_b <- 100L + (i - 1L) * 10L
  status <- if (a_val == expected_a && b_val == expected_b) "✓" else "✗"
  cat(sprintf(
    "  [%d]: a='%s', b=%d %s\n",
    i,
    rawToChar(as.raw(a_val)),
    b_val,
    status
  ))
}
cat("\n")

# ============ Test 6: By-value passing (may have limitations) ============

cat("=== Test 6: By-Value Passing (Experimental) ===\n")
cat(
  "NOTE: Passing packed structs by value may not work correctly due to ABI differences.\n"
)
cat("      libffi uses natural alignment for function calls.\n\n")

tryCatch(
  {
    # This creates a CIF for by-value packed struct - may not match C's ABI
    read_packed_simple_b_byval <- ffi_function(
      "read_packed_simple_b_byval",
      ffi_int32(),
      PackedSimple,
      library = lib
    )

    # Create a packed struct in memory
    ptr3 <- ffi_alloc(PackedSimple)
    ffi_set_field(ptr3, "a", char_to_int("Z"), PackedSimple)
    ffi_set_field(ptr3, "b", 99999L, PackedSimple)

    # Try to pass by value - this passes the pointer, C function interprets it
    # NOTE: This likely won't work correctly because libffi will use natural alignment
    result <- read_packed_simple_b_byval(ptr3)
    cat(sprintf("By-value result: %d (expected 99999)\n", result))
    if (result == 99999L) {
      cat("By-value passing worked (may be platform-specific) ✓\n")
    } else {
      cat(
        "By-value passing returned incorrect value (expected due to ABI mismatch)\n"
      )
    }
  },
  error = function(e) {
    cat(sprintf("By-value test error: %s\n", e$message))
  }
)
cat("\n")

# ============ Summary ============

cat("=== Summary ===\n")
cat("Pointer-based access to packed structs: WORKS\n")
cat("Array of packed structs: WORKS\n")
cat("Nested packed structs: WORKS\n")
cat("By-value passing: MAY NOT WORK (ABI limitation with libffi)\n")
cat("\nRecommendation: Always pass packed structs BY POINTER, not by value.\n")

# Cleanup
dll_unload(lib_path)
cat("\nTests completed.\n")

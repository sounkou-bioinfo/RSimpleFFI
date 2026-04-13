# Hybrid ABI/API Mode Proposal for RSimpleFFI

## Executive Summary

**Problem**: Current RSimpleFFI uses pure ABI mode (like Python’s
ctypes) which has fundamental limitations with: 1. **Bitfield
structs** - Mixed structs with bitfields and regular members (e.g.,
htslib’s `hFILE`) 2. **Constructor functions** - No way to safely
initialize complex structs 3. **Compiler-specific layouts** -
Platform/compiler differences in struct packing 4. **Opaque types** -
Cannot construct types defined in headers without knowing internals

**Proposed Solution**: Hybrid ABI/API mode similar to Python’s CFFI,
using TinyCC or R CMD SHLIB for JIT compilation of helper functions.

## Current Status & Limitations

### What Works (Pure ABI Mode)

``` r
# Simple structs work fine
Point <- ffi_struct(x = ffi_int(), y = ffi_int())
ptr <- ffi_alloc(Point)

# Bitfields as packed integers work
settings <- ffi_uint32()  # Represents bitfield struct
packed <- pack_fn(1L, 2L, 3L)  # Call C function to pack
```

### What Doesn’t Work

#### 1. Mixed Bitfield/Regular Member Structs

``` c
// htslib's hFILE - CANNOT be modeled with ffi_struct()
typedef struct hFILE {
    char *buffer, *begin, *end, *limit;      // Regular pointers
    const struct hFILE_backend *backend;     // Regular pointer
    off_t offset;                             // Regular member
    unsigned at_eof:1, mobile:1, readonly:1; // BITFIELDS!
    int has_errno;                            // Regular member
} hFILE;
```

**Problem**:
[`ffi_struct()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_struct.md)
models each field as full-sized type, but bitfields are compiler-packed.
The layout doesn’t match reality.

#### 2. No Safe Constructors

``` r
# Current: Manual allocation + field-by-field setting (error-prone!)
ptr <- ffi_alloc(ComplexStruct)
ffi_set_field(ptr, "field1", value1)  # Tedious
ffi_set_field(ptr, "field2", value2)  # Error-prone
ffi_set_field(ptr, "field3", value3)  # No validation

# Desired: Constructor function
obj <- create_ComplexStruct(value1, value2, value3)
```

#### 3. Opaque Types

``` c
// Library header declares but doesn't define:
typedef struct Database Database;  // Opaque

// Library provides constructor:
Database* db_create(const char* path);
void db_destroy(Database* db);
```

**Current solution**: Use
[`ffi_pointer()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_pointer.md)
and call constructor - works but no struct field access.

## Proposed Solution: Hybrid ABI/API Mode

### Architecture Overview

    ┌─────────────────────────────────────────────────────────┐
    │                    R User Code                          │
    └────────────────┬────────────────────────────────────────┘
                     │
        ┌────────────┴────────────┐
        │                         │
        ▼                         ▼
    ┌─────────┐            ┌──────────────┐
    │ ABI Mode│            │  API Mode    │
    │ (libffi)│            │  (JIT C)     │
    └────┬────┘            └──────┬───────┘
         │                        │
         ▼                        ▼
    ┌─────────────┐      ┌────────────────┐
    │ Direct calls│      │ Generated .so  │
    │ to C ABI    │      │ with helpers   │
    └─────────────┘      └────────────────┘

### Strategy A: TinyCC JIT (Fast Development)

**Advantages**: - Fast compilation (TinyCC compiles in milliseconds) -
Already vendored in package - No external dependencies - Perfect for
interactive development - Can reload code on-the-fly

**Disadvantages**: - TinyCC doesn’t support all GCC extensions
(`__attribute__((packed))`, some builtins) - May have platform-specific
issues (as we’ve seen in CI) - Less optimized code

### Strategy B: R CMD SHLIB (Production Ready)

**Advantages**: - Uses system compiler (gcc/clang/MSVC) - full language
support - Reliable across platforms - Optimized code - Standard R
package mechanism - Can compile complex code with all extensions

**Disadvantages**: - Slower compilation (seconds vs milliseconds) -
Requires development tools (Rtools on Windows) - Less suitable for
interactive/iterative development

### Recommended Approach: Hybrid Strategy

Use **TinyCC for development**, **R CMD SHLIB for production**:

``` r
# Development mode - fast iteration
ffi_compile_helpers(header, mode = "tinycc")  # Fast, immediate feedback

# Production mode - robust, optimized
ffi_compile_helpers(header, mode = "shlib")   # Slower but bulletproof

# Auto mode - try TinyCC, fallback to SHLIB
ffi_compile_helpers(header, mode = "auto")    # Best of both worlds
```

## Implementation Design

### 1. Code Generation Pipeline

``` r
ffi_generate_helpers <- function(header_file, functions = c("new", "free", "getters", "setters")) {
  # Parse header to extract struct definitions
  parsed <- ffi_parse_header(header_file)
  
  # Generate C helper code
  c_code <- generate_helper_code(parsed, functions)
  
  # Write to temporary .c file
  tmp_c <- tempfile(fileext = ".c")
  writeLines(c_code, tmp_c)
  
  return(tmp_c)
}
```

### 2. Generated Helper Types

#### Constructor/Destructor

``` c
// Auto-generated from struct definition
// Use rffi_ prefix to avoid naming collisions with library functions
// Returns SEXP external pointer with automatic finalization

#include <R.h>
#include <Rinternals.h>

// Internal destructor for finalizer
static void rffi_hFILE_finalizer(SEXP ext_ptr) {
    hFILE* obj = (hFILE*)R_ExternalPtrAddr(ext_ptr);
    if (obj) {
        // Add any cleanup logic here (close files, free nested pointers, etc.)
        free(obj);
        R_ClearExternalPtr(ext_ptr);  // Clear the pointer
    }
}

// Constructor returns SEXP external pointer
SEXP rffi_hFILE_new(void) {
    hFILE* obj = (hFILE*)calloc(1, sizeof(hFILE));
    if (!obj) {
        Rf_error("Failed to allocate memory for hFILE");
    }
    
    // Initialize with safe defaults
    obj->buffer = NULL;
    obj->begin = NULL;
    obj->end = NULL;
    obj->limit = NULL;
    obj->backend = NULL;
    obj->offset = 0;
    obj->at_eof = 0;
    obj->mobile = 0;
    obj->readonly = 0;
    obj->has_errno = 0;
    
    // Wrap in external pointer with finalizer
    SEXP ext_ptr = PROTECT(R_MakeExternalPtr(obj, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ext_ptr, rffi_hFILE_finalizer, TRUE);
    
    // Preserve to prevent premature GC, will be released when R object is GC'd
    R_PreserveObject(ext_ptr);
    
    UNPROTECT(1);
    return ext_ptr;
}

// Manual free function (optional, for explicit cleanup)
void rffi_hFILE_free(SEXP ext_ptr) {
    if (TYPEOF(ext_ptr) != EXTPTRSXP) {
        Rf_error("Expected external pointer");
    }
    
    hFILE* obj = (hFILE*)R_ExternalPtrAddr(ext_ptr);
    if (obj) {
        free(obj);
        R_ClearExternalPtr(ext_ptr);
    }
    
    // Release preservation
    R_ReleaseObject(ext_ptr);
}
```

#### Generic Field Accessors (Type-Aware)

``` c
// Generic field accessor - returns SEXP external pointer to field location
// The field pointer is TAGGED with the FFI type for later conversion
// This leverages RSimpleFFI's existing type system instead of generating N×M functions
SEXP rffi_struct_get_field_ptr(SEXP struct_ptr, size_t offset, SEXP field_type) {
    // Validate struct pointer
    if (TYPEOF(struct_ptr) != EXTPTRSXP) {
        Rf_error("Expected external pointer for struct");
    }
    void* struct_addr = R_ExternalPtrAddr(struct_ptr);
    if (!struct_addr) {
        Rf_error("NULL struct pointer");
    }
    
    // Calculate field address
    void* field_addr = (char*)struct_addr + offset;
    
    // Return external pointer TAGGED with FFI type information
    // Tag stores the field's FFI type for later type-aware operations
    // Prot stores the parent struct to prevent premature GC
    SEXP field_ptr = PROTECT(R_MakeExternalPtr(field_addr, field_type, struct_ptr));
    UNPROTECT(1);
    return field_ptr;
}

// Generic field setter - uses FFI type system to write value
// This is called from R with type information already attached
void rffi_struct_set_field(SEXP struct_ptr, size_t offset, SEXP field_type, SEXP value) {
    // Get field pointer (tagged with type)
    SEXP field_ptr = rffi_struct_get_field_ptr(struct_ptr, offset, field_type);
    void* field_addr = R_ExternalPtrAddr(field_ptr);
    
    // Use RSimpleFFI's existing ffi_write_typed_value() function
    // This handles all types including bitfields based on field_type
    ffi_write_typed_value(field_addr, field_type, value);
}

// Helper to convert field pointer to R value
// Extracts type information from tag and uses FFI type system
SEXP rffi_field_to_r(SEXP field_ptr) {
    if (TYPEOF(field_ptr) != EXTPTRSXP) {
        Rf_error("Expected external pointer for field");
    }
    
    void* field_addr = R_ExternalPtrAddr(field_ptr);
    if (!field_addr) {
        Rf_error("NULL field pointer");
    }
    
    // Extract FFI type from tag - this is the field's type info!
    SEXP field_type = R_ExternalPtrTag(field_ptr);
    
    // Use RSimpleFFI's existing ffi_read_typed_value() function
    // Handles all types including bitfields, pointers, structs, etc.
    return ffi_read_typed_value(field_addr, field_type);
}

// Struct-specific helper: return all field offsets as R list
// This is the ONLY struct-specific code needed!
// Compiler computes offsets using offsetof() - handles alignment, padding, bitfields!
SEXP rffi_hFILE_offsets(void) {
    // Return named list of field offsets
    const char* names[] = {
        "buffer", "begin", "end", "limit", "backend",
        "offset", "at_eof", "mobile", "readonly", "has_errno"
    };
    
    size_t offsets[] = {
        offsetof(hFILE, buffer),
        offsetof(hFILE, begin),
        offsetof(hFILE, end),
        offsetof(hFILE, limit),
        offsetof(hFILE, backend),
        offsetof(hFILE, offset),
        offsetof(hFILE, at_eof),    // Bitfield - compiler knows the offset!
        offsetof(hFILE, mobile),    // Bitfield - compiler handles packing!
        offsetof(hFILE, readonly),  // Bitfield - no manual calculation!
        offsetof(hFILE, has_errno)
    };
    
    int n_fields = sizeof(offsets) / sizeof(size_t);
    SEXP result = PROTECT(Rf_allocVector(VECSXP, n_fields));
    SEXP result_names = PROTECT(Rf_allocVector(STRSXP, n_fields));
    
    for (int i = 0; i < n_fields; i++) {
        SET_STRING_ELT(result_names, i, Rf_mkChar(names[i]));
        SET_VECTOR_ELT(result, i, Rf_ScalarReal((double)offsets[i]));
    }
    
    Rf_setAttrib(result, R_NamesSymbol, result_names);
    UNPROTECT(2);
    return result;
}
```

#### Initialization Functions

``` c
// Full initialization with all parameters (rffi_ prefix to avoid collisions)
// Returns SEXP external pointer with automatic finalization
SEXP rffi_hFILE_init(char* buffer, char* begin, char* end, char* limit,
                     const hFILE_backend* backend, off_t offset,
                     int at_eof, int mobile, int readonly, int has_errno) {
    // First create the object
    SEXP ext_ptr = PROTECT(rffi_hFILE_new());
    
    // Extract the C pointer and initialize fields
    hFILE* obj = (hFILE*)R_ExternalPtrAddr(ext_ptr);
    if (!obj) {
        UNPROTECT(1);
        Rf_error("Failed to create hFILE object");
    }
    
    obj->buffer = buffer;
    obj->begin = begin;
    obj->end = end;
    obj->limit = limit;
    obj->backend = backend;
    obj->offset = offset;
    obj->at_eof = at_eof ? 1 : 0;
    obj->mobile = mobile ? 1 : 0;
    obj->readonly = readonly ? 1 : 0;
    obj->has_errno = has_errno;
    
    UNPROTECT(1);
    return ext_ptr;
}

// Helper to extract raw C pointer (use with caution!)
// This is for passing to library functions that expect raw pointers
hFILE* rffi_hFILE_get_ptr(SEXP ext_ptr) {
    if (TYPEOF(ext_ptr) != EXTPTRSXP) {
        Rf_error("Expected external pointer");
    }
    hFILE* obj = (hFILE*)R_ExternalPtrAddr(ext_ptr);
    if (!obj) {
        Rf_error("NULL pointer");
    }
    return obj;
}
```

### 3. TinyCC JIT Implementation

``` r
ffi_compile_tinycc <- function(c_source, include_dirs = NULL) {
  # Load libtcc (platform-specific)
  if (.Platform$OS.type == "windows") {
    tcc_lib <- system.file("tinycc", "libtcc.dll", package = "RSimpleFFI")
  } else if (Sys.info()["sysname"] == "Darwin") {
    tcc_lib <- system.file("tinycc", "lib", "libtcc.dylib", package = "RSimpleFFI")
  } else {
    tcc_lib <- system.file("tinycc", "lib", "libtcc.so", package = "RSimpleFFI")
  }
  
  if (!file.exists(tcc_lib)) {
    stop("TinyCC library not found at: ", tcc_lib)
  }
  
  # Load the library
  dll_load(tcc_lib)
  
  # Define libtcc API
  tcc_new <- ffi_function("tcc_new", ffi_pointer())
  tcc_delete <- ffi_function("tcc_delete", ffi_void(), ffi_pointer())
  tcc_set_output_type <- ffi_function("tcc_set_output_type", ffi_int(), 
                                      ffi_pointer(), ffi_int())
  tcc_add_include_path <- ffi_function("tcc_add_include_path", ffi_int(),
                                       ffi_pointer(), ffi_pointer())
  tcc_compile_string <- ffi_function("tcc_compile_string", ffi_int(),
                                     ffi_pointer(), ffi_pointer())
  tcc_relocate <- ffi_function("tcc_relocate", ffi_int(),
                               ffi_pointer(), ffi_pointer())
  tcc_get_symbol <- ffi_function("tcc_get_symbol", ffi_pointer(),
                                 ffi_pointer(), ffi_pointer())
  
  # Create TCC state
  state <- tcc_new()
  if (is.null(state)) {
    stop("Failed to create TCC state")
  }
  
  on.exit(tcc_delete(state))
  
  # Configure TCC
  TCC_OUTPUT_MEMORY <- 1L
  tcc_set_output_type(state, TCC_OUTPUT_MEMORY)
  
  # Add include paths
  if (!is.null(include_dirs)) {
    for (dir in include_dirs) {
      tcc_add_include_path(state, dir)
    }
  }
  
  # Compile source
  ret <- tcc_compile_string(state, c_source)
  if (ret < 0) {
    stop("TCC compilation failed")
  }
  
  # Relocate (JIT)
  # First call gets required size
  size <- tcc_relocate(state, NULL)
  if (size < 0) {
    stop("TCC relocation failed")
  }
  
  # Allocate memory and relocate
  mem <- ffi_alloc(ffi_uint8(), size)
  ret <- tcc_relocate(state, mem)
  if (ret < 0) {
    stop("TCC relocation failed")
  }
  
  # Return state and memory (prevent GC)
  structure(list(state = state, memory = mem), class = "tcc_program")
}

ffi_get_symbol_tcc <- function(program, symbol_name) {
  tcc_get_symbol <- ffi_function("tcc_get_symbol", ffi_pointer(),
                                 ffi_pointer(), ffi_pointer())
  
  sym <- tcc_get_symbol(program$state, symbol_name)
  if (is.null(sym)) {
    stop("Symbol not found: ", symbol_name)
  }
  
  ffi_symbol(sym)
}
```

### 4. R CMD SHLIB Implementation

``` r
ffi_compile_shlib <- function(c_source, include_dirs = NULL) {
  # Write C source to temp file
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  
  c_file <- file.path(tmp_dir, "helpers.c")
  writeLines(c_source, c_file)
  
  # Build R CMD SHLIB command
  cmd <- c("SHLIB", c_file)
  
  if (!is.null(include_dirs)) {
    includes <- paste0("-I", include_dirs, collapse = " ")
    cmd <- c(cmd, paste0("PKG_CPPFLAGS=\"", includes, "\""))
  }
  
  # Compile
  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd))
  
  result <- system2(file.path(R.home("bin"), "R"), c("CMD", cmd),
                   stdout = TRUE, stderr = TRUE)
  
  # Check for .so/.dll
  so_file <- list.files(tmp_dir, pattern = "\\.(so|dll)$", full.names = TRUE)
  if (length(so_file) == 0) {
    stop("Compilation failed:\n", paste(result, collapse = "\n"))
  }
  
  # Load library
  dll <- dll_load(so_file[1])
  
  # Keep temp dir alive for the DLL lifetime
  attr(dll, "tmpdir") <- tmp_dir
  
  dll
}

ffi_get_symbol_shlib <- function(dll, symbol_name) {
  dll_symbol(dll, symbol_name)
}
```

### 5. High-Level User API

``` r
# Parse header and generate helper library
helpers <- ffi_create_helpers(
  header = "htslib/hfile.h",
  structs = "hFILE",
  mode = "auto",  # Try TinyCC, fallback to SHLIB
  include_dirs = c("/usr/include", "inst/include"),
  prefix = "rffi_"  # Prefix to avoid naming collisions
)

# Use generated constructor (returns SEXP external pointer with finalizer)
# Automatic memory management - freed when R object is GC'd
hfile <- helpers$rffi_hFILE_new()
class(hfile)  # "externalptr"

# Generic field access - type information flows from R!
# Field offsets computed by COMPILER using offsetof() - handles all edge cases!
hFILE_type <- helpers$hFILE_type  # Contains field metadata
# e.g., hFILE_type$fields$mobile = list(type = ffi_bitfield(1), offset = 48)

# Get field pointer (tagged with type) - lazy, doesn't convert yet
mobile_ptr <- ffi_get_field_ptr(hfile, hFILE_type$fields$mobile$offset, 
                                hFILE_type$fields$mobile$type)

# Convert to R value when needed (uses tag to determine type)
mobile_value <- ffi_field_to_r(mobile_ptr)  # Returns R logical/integer

# Or use convenience wrapper for direct get
mobile_value <- ffi_get_field(hfile, "mobile", hFILE_type)

# Generic set - works for ANY type (bitfields, pointers, structs, etc.)
ffi_set_field(hfile, "mobile", hFILE_type, 1L)
ffi_set_field(hfile, "readonly", hFILE_type, TRUE)
ffi_set_field(hfile, "offset", hFILE_type, 1024)

# Use generated initializer (also returns external pointer)
hfile2 <- helpers$rffi_hFILE_init(
  buffer = NULL,
  begin = NULL,
  end = NULL,
  limit = NULL,
  backend = NULL,
  offset = 0,
  at_eof = 0,
  mobile = 1,
  readonly = 0,
  has_errno = 0
)

# Pass to library functions - need raw C pointer
hfile_ptr <- helpers$rffi_hFILE_get_ptr(hfile)
hclose_fn <- ffi_function("hclose", ffi_int(), ffi_pointer())
result <- hclose_fn(hfile_ptr)

# Clean up (optional - automatic with finalizer, but can be explicit)
helpers$rffi_hFILE_free(hfile)
helpers$rffi_hFILE_free(hfile2)

# Or let R's GC handle it automatically
rm(hfile, hfile2)
gc()  # Finalizers will be called
```

### 6. Code Generation Functions

``` r
generate_constructor <- function(struct_info, prefix = "rffi_") {
  struct_name <- struct_info$name
  func_prefix <- paste0(prefix, struct_name)
  
  c_code <- sprintf("
%s* %s_new(void) {
    %s* obj = (%s*)calloc(1, sizeof(%s));
    if (!obj) return NULL;
    
    /* Initialize fields with safe defaults */
    %s
    
    return obj;
}

void %s_free(%s* obj) {
    if (obj) {
        free(obj);
    }
}
", 
    struct_name, func_prefix,  # Function declaration with prefix
    struct_name, struct_name, struct_name,  # Allocation
    generate_field_initializers(struct_info),  # Field init
    func_prefix, struct_name  # Free function with prefix
  )
  
  return(c_code)
}

generate_field_initializers <- function(struct_info) {
  inits <- character(0)
  
  for (field in struct_info$fields) {
    default_val <- get_default_value(field$type)
    inits <- c(inits, sprintf("    obj->%s = %s;", field$name, default_val))
  }
  
  paste(inits, collapse = "\n")
}

generate_offset_extractor <- function(struct_info, prefix = "rffi_") {
  struct_name <- struct_info$name
  func_prefix <- paste0(prefix, struct_name)
  
  # Generate function that returns ALL offsets at once
  # Compiler computes offsets using offsetof() - no manual calculation!
  field_names <- sapply(struct_info$fields, function(f) f$name)
  
  # Build C arrays
  names_array <- paste0('        "', field_names, '"', collapse = ", ")
  offsets_array <- paste0(
    '        offsetof(', struct_name, ', ', field_names, ')',
    collapse = ",\n"
  )
  
  c_code <- sprintf('
// Returns named list of field offsets (compiler-computed with offsetof())
SEXP %s_offsets(void) {
    const char* names[] = {
%s
    };
    
    size_t offsets[] = {
%s
    };
    
    int n_fields = sizeof(offsets) / sizeof(size_t);
    SEXP result = PROTECT(Rf_allocVector(VECSXP, n_fields));
    SEXP result_names = PROTECT(Rf_allocVector(STRSXP, n_fields));
    
    for (int i = 0; i < n_fields; i++) {
        SET_STRING_ELT(result_names, i, Rf_mkChar(names[i]));
        SET_VECTOR_ELT(result, i, Rf_ScalarReal((double)offsets[i]));
    }
    
    Rf_setAttrib(result, R_NamesSymbol, result_names);
    UNPROTECT(2);
    return result;
}
', func_prefix, names_array, offsets_array)
  
  return(c_code)
}

generate_field_type_info <- function(struct_info, helpers) {
  # Get field offsets from compiled C code (compiler-computed with offsetof()!)
  struct_name <- struct_info$name
  offset_fn_name <- paste0("rffi_", struct_name, "_offsets")
  
  # Call compiled function - returns named list of offsets
  offsets <- .Call(offset_fn_name)
  
  # Build field metadata list
  field_list <- list()
  for (field in struct_info$fields) {
    field_name <- field$name
    ffi_type <- get_ffi_type(field$type)  # Returns FFIType object
    
    field_list[[field_name]] <- list(
      type = ffi_type,
      offset = as.integer(offsets[[field_name]])  # Compiler-computed!
    )
  }
  
  return(field_list)
}

get_ffi_type_expr <- function(type_info) {
  # Map C types to FFI type constructors
  switch(type_info$base,
    "int" = "ffi_int()",
    "unsigned" = "ffi_uint()",
    "char*" = "ffi_pointer()",
    "off_t" = "ffi_long()",
    "bitfield" = sprintf("ffi_bitfield(%d)", type_info$width),
    sprintf("ffi_type('%s')", type_info$base)
  )
}
```

## Generic Accessors vs Per-Field Functions

### Why Generic Accessors Win

**Traditional Approach (Per-Field Functions)**: - Generate N getters + N
setters for struct with N fields - 1000-line struct → 2000+ generated
functions - Type information duplicated in C code - Doesn’t scale to
complex codebases

**Generic Approach (Type-Aware Accessors)**: - 3 generic functions work
for ANY struct/field combination - Type information stored in R (FFI
types) - Leverages existing `ffi_read_typed_value()` /
`ffi_write_typed_value()` - Only struct-specific code: `offsetof()`
lookup (1 function per struct)

**Key Benefits**: 1. **Scalability**: O(1) generated code per struct
instead of O(N) per field 2. **Reusability**: Uses RSimpleFFI’s existing
FFI type system 3. **Flexibility**: Type information in R enables
dynamic field access 4. **Maintainability**: Changes to type handling
happen once, not per field 5. **Tagged Pointers**: Field pointers carry
type info for lazy conversion 6. **Compiler-Computed Offsets**: Uses
`offsetof()` - handles alignment, padding, bitfields, compiler-specific
layout automatically!

**Example Comparison**:

``` r
# Old approach: 20 fields = 40 generated C functions
helpers$rffi_hFILE_get_buffer(ptr)
helpers$rffi_hFILE_set_buffer(ptr, val)
helpers$rffi_hFILE_get_begin(ptr)
helpers$rffi_hFILE_set_begin(ptr, val)
# ... 36 more functions ...

# New approach: 1 generic function + type metadata
ffi_get_field(ptr, "buffer", hFILE_type)
ffi_set_field(ptr, "buffer", hFILE_type, val)
ffi_get_field(ptr, "begin", hFILE_type)
ffi_set_field(ptr, "begin", hFILE_type, val)
# Same code works for ALL fields!
```

## Comparison: TinyCC vs R CMD SHLIB

| Feature                 | TinyCC JIT      | R CMD SHLIB        |
|-------------------------|-----------------|--------------------|
| **Compilation Speed**   | \<50ms          | 1-5 seconds        |
| **Runtime Performance** | Good            | Excellent          |
| **GCC Extensions**      | Limited         | Full support       |
| **Platform Support**    | Some issues     | Rock solid         |
| **Dependencies**        | None (vendored) | Rtools/build tools |
| **Use Case**            | Interactive dev | Production         |
| **Reload Code**         | Yes (fast)      | No (slow)          |
| **Debugging**           | Difficult       | Standard tools     |

## Migration Path

### Phase 1: R CMD SHLIB Implementation (Weeks 1-2)

- Implement
  [`ffi_compile_shlib()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_compile_shlib.md)
- Implement code generation functions
- Test with simple structs
- Test with bitfield structs
- Test with mixed structs (hFILE)

### Phase 2: TinyCC JIT Implementation (Weeks 3-4)

- Implement `ffi_compile_tinycc()` using libtcc API
- Handle TinyCC limitations (no `__attribute__`)
- Create fallback mechanisms
- Test on all platforms

### Phase 3: High-Level API (Week 5)

- Implement
  [`ffi_create_helpers()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_create_helpers.md)
- Auto-detect compilation mode
- Cache compiled helpers
- Documentation and examples

### Phase 4: Integration (Week 6)

- Integrate with existing
  [`ffi_parse_header()`](https://sounkou-bioinfo.github.io/RSimpleFFI/reference/ffi_parse_header.md)
- Update code generation to emit helper calls
- Update tests
- Update documentation

## Example Use Cases

### Use Case 1: htslib Integration

``` r
# Current: Cannot model hFILE struct properly
# Proposed:
helpers <- ffi_create_helpers("htslib/hfile.h", structs = "hFILE", prefix = "rffi_")

# rffi_ prefix avoids collision with htslib's own functions
hfile <- helpers$rffi_hFILE_new()

# Generic field access - works with bitfields!
hFILE_type <- helpers$hFILE_type
ffi_set_field(hfile, "mobile", hFILE_type, 1L)
ffi_set_field(hfile, "readonly", hFILE_type, 0L)
ffi_set_field(hfile, "offset", hFILE_type, 1024)

# Read fields back
mobile <- ffi_get_field(hfile, "mobile", hFILE_type)  # Returns 1L
offset <- ffi_get_field(hfile, "offset", hFILE_type)  # Returns 1024

# Pass to htslib functions (original library functions, no prefix)
hfile_ptr <- helpers$rffi_hFILE_get_ptr(hfile)
hclose_fn <- ffi_function("hclose", ffi_int(), ffi_pointer())
result <- hclose_fn(hfile_ptr)
```

### Use Case 2: GTK+ Widget Creation

``` r
# Current: Manual struct manipulation
# Proposed:
helpers <- ffi_create_helpers("gtk/gtk.h", structs = "GtkWidget", prefix = "rffi_")

# rffi_ prefix avoids collision with GTK's constructor functions
widget <- helpers$rffi_GtkWidget_new()

# Generic field access
GtkWidget_type <- helpers$GtkWidget_type
ffi_set_field(widget, "visible", GtkWidget_type, 1L)
ffi_set_field(widget, "sensitive", GtkWidget_type, 1L)

# Pass to GTK functions (original library functions)
widget_ptr <- helpers$rffi_GtkWidget_get_ptr(widget)
gtk_show_fn <- ffi_function("gtk_widget_show", ffi_void(), ffi_pointer())
gtk_show_fn(widget_ptr)
```

### Use Case 3: Protocol Buffer Structs

``` r
# Complex nested structs with bitfields
helpers <- ffi_create_helpers("protocol.h", structs = c("Header", "Packet"), prefix = "rffi_")

# rffi_ prefix ensures no collision with protocol library functions
header <- helpers$rffi_Header_init(
  version = 1L,
  flags = 0L,
  length = 256L
)

packet <- helpers$rffi_Packet_new()

# Generic field access for nested structs
Packet_type <- helpers$Packet_type
ffi_set_field(packet, "header", Packet_type, header)  # Nested struct
ffi_set_field(packet, "payload", Packet_type, payload_ptr)  # Pointer

# Read nested field
header_back <- ffi_get_field(packet, "header", Packet_type)
version <- ffi_get_field(header_back, "version", helpers$Header_type)
```

## Conclusion

**Recommendation**: Implement R CMD SHLIB first (solid foundation), then
add TinyCC JIT as optional fast mode.

This hybrid approach: 1. ✅ Solves the bitfield problem completely 2. ✅
Provides safe constructors 3. ✅ Works with all compiler extensions 4.
✅ Maintains ABI mode for simple cases 5. ✅ Adds API mode for complex
cases 6. ✅ Similar to proven CFFI design 7. ✅ Uses standard R tools (R
CMD SHLIB) 8. ✅ Optional TinyCC for speed

**Next Steps**: 1. Prototype R CMD SHLIB implementation 2. Test with
hFILE struct from htslib 3. Benchmark compilation speed 4. Create
user-facing API 5. Write documentation and examples

#' Code Generation for API Mode
#'
#' Generate C helper code for struct field access using compiler-computed offsets.
#'
#' @name code_generation
#' @keywords internal
NULL

#' Generate offset extractor function for a struct (API mode)
#'
#' Creates C code that returns all field offsets for a struct using offsetof().
#' The compiler computes the offsets, handling alignment, padding, and bitfields
#' correctly for the target platform.
#'
#' @param struct_name Name of the C struct (e.g., "Point2D", "hFILE")
#' @param field_names Character vector of field names
#' @param prefix Prefix for generated function names (default "rffi_")
#' @return Character string containing C code
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' code <- generate_api_offset_extractor("Point2D", c("x", "y"))
#' cat(code)
#' }
generate_api_offset_extractor <- function(struct_name, field_names, prefix = "rffi_") {
  if (length(field_names) == 0) {
    stop("field_names cannot be empty")
  }
  
  func_name <- paste0(prefix, struct_name, "_offsets")
  
  # Strip bitfield syntax (e.g., "enabled : 1" -> "enabled")
  # offsetof() only needs the field name, not the bitfield width
  clean_field_names <- gsub("\\s*:.*$", "", field_names)
  
  # Build field names array (use original names for R)
  names_array <- paste0(
    '        "', clean_field_names, '"',
    collapse = ",\n"
  )
  
  # Build offsetof() calls array (use clean names without bitfield syntax)
  offsets_array <- paste0(
    '        offsetof(', struct_name, ', ', clean_field_names, ')',
    collapse = ",\n"
  )
  
  # Generate C function
  c_code <- sprintf('
#include <R.h>
#include <Rinternals.h>
#include <stddef.h>

// Returns named list of field offsets (compiler-computed with offsetof())
SEXP %s(void) {
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
', func_name, names_array, offsets_array)
  
  return(c_code)
}

#' Generate compile-time bitfield offset/size detection macros
#' 
#' Uses the Reddit r/cpp technique with __builtin_ctzll/__builtin_clzll
#' Works on GCC/Clang (which R always uses)
#' 
#' @keywords internal
generate_bitfield_detection_macros <- function() {
  '
/* Compile-time bitfield offset and size detection
 * Based on: https://www.reddit.com/r/cpp/comments/rz1m1w/sizeofoffsetof_for_bitfields/
 * Works with GCC/Clang (R always uses these compilers)
 */
#define BITOFFSETOF(type, field) \\
  ({ union { unsigned long long raw; type typ; } _x = {0}; \\
     ++_x.typ.field; __builtin_ctzll(_x.raw); })

#define BITSIZEOF(type, field) \\
  ({ union { unsigned long long raw; type typ; } _x = {0}; \\
     --_x.typ.field; \\
     8*sizeof(_x.raw) - __builtin_clzll(_x.raw) - __builtin_ctzll(_x.raw); })
'
}

#' Generate bitfield accessor functions for a struct (API mode)
#'
#' Creates C code for getting/setting bitfield values using compile-time
#' offset detection and bit operations.
#'
#' @param struct_name Name of the C struct
#' @param bitfield_info List with elements: name, clean_name, width
#' @param prefix Prefix for generated function names
#' @return Character string containing C code
#' @keywords internal
generate_api_bitfield_accessors <- function(struct_name, bitfield_info, prefix = "rffi_") {
  if (length(bitfield_info) == 0) {
    return("")
  }
  
  func_base <- paste0(prefix, struct_name)
  accessors <- character()
  
  for (info in bitfield_info) {
    field_name <- info$clean_name
    
    # Generate getter using compile-time offset detection
    getter <- sprintf('
// Get bitfield %s from %s (detected at compile time)
SEXP %s_get_%s(SEXP ptr) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        Rf_error("ptr must be an external pointer");
    }
    
    %s *s = (%s *)R_ExternalPtrAddr(ptr);
    if (s == NULL) {
        Rf_error("NULL pointer");
    }
    
    // Compile-time bitfield detection
    const int bit_offset = BITOFFSETOF(%s, %s);
    const int bit_size = BITSIZEOF(%s, %s);
    const int byte_offset = bit_offset / 8;
    const int bit_shift = bit_offset %% 8;
    
    // Read container
    unsigned char *base = (unsigned char *)s + byte_offset;
    unsigned long long container = 0;
    memcpy(&container, base, sizeof(unsigned int));
    
    // Extract bitfield value
    unsigned int value = (container >> bit_shift) & ((1ULL << bit_size) - 1);
    
    return Rf_ScalarInteger((int)value);
}
', field_name, struct_name,
      func_base, field_name,
      struct_name, struct_name,
      struct_name, field_name,
      struct_name, field_name)
    
    # Generate setter using compile-time offset detection
    setter <- sprintf('
// Set bitfield %s in %s (detected at compile time)
SEXP %s_set_%s(SEXP ptr, SEXP value) {
    if (TYPEOF(ptr) != EXTPTRSXP) {
        Rf_error("ptr must be an external pointer");
    }
    if (!Rf_isInteger(value) && !Rf_isReal(value)) {
        Rf_error("value must be numeric");
    }
    
    %s *s = (%s *)R_ExternalPtrAddr(ptr);
    if (s == NULL) {
        Rf_error("NULL pointer");
    }
    
    // Compile-time bitfield detection
    const int bit_offset = BITOFFSETOF(%s, %s);
    const int bit_size = BITSIZEOF(%s, %s);
    const int byte_offset = bit_offset / 8;
    const int bit_shift = bit_offset %% 8;
    
    unsigned int new_val = (unsigned int)Rf_asInteger(value);
    unsigned long long mask = (1ULL << bit_size) - 1;
    
    if (new_val > mask) {
        Rf_error("Value %%u exceeds bitfield width of %%d bits", new_val, bit_size);
    }
    
    // Read-modify-write container
    unsigned char *base = (unsigned char *)s + byte_offset;
    unsigned long long container = 0;
    memcpy(&container, base, sizeof(unsigned int));
    
    container = (container & ~(mask << bit_shift)) | ((unsigned long long)(new_val & mask) << bit_shift);
    
    memcpy(base, &container, sizeof(unsigned int));
    
    return R_NilValue;
}
', field_name, struct_name,
      func_base, field_name,
      struct_name, struct_name,
      struct_name, field_name,
      struct_name, field_name)
    
    accessors <- c(accessors, getter, setter)
  }
  
  return(paste(accessors, collapse = "\n"))
}

#' Constructor function for a struct (API mode)
#'
#' Creates C code for allocating and initializing a struct with default values.
#'
#' @param struct_name Name of the C struct
#' @param prefix Prefix for generated function names (default "rffi_")
#' @return Character string containing C code
#' @export
#' @keywords internal
generate_api_constructor <- function(struct_name, prefix = "rffi_") {
  func_name <- paste0(prefix, struct_name, "_new")
  
  c_code <- sprintf('
// Constructor returns SEXP external pointer with automatic finalization
static void %s_finalizer(SEXP ext_ptr) {
    %s* obj = (%s*)R_ExternalPtrAddr(ext_ptr);
    if (obj) {
        free(obj);
        R_ClearExternalPtr(ext_ptr);
    }
}

SEXP %s(void) {
    %s* obj = (%s*)calloc(1, sizeof(%s));
    if (!obj) {
        Rf_error("Failed to allocate memory for %s");
    }
    
    // Wrap in external pointer with finalizer
    SEXP ext_ptr = PROTECT(R_MakeExternalPtr(obj, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ext_ptr, %s_finalizer, TRUE);
    
    UNPROTECT(1);
    return ext_ptr;
}
', struct_name, struct_name, struct_name, func_name, struct_name, 
   struct_name, struct_name, struct_name, struct_name)
  
  return(c_code)
}

#' Generate struct typedef from field types
#'
#' @param struct_name Name of the C struct
#' @param field_names Character vector of field names
#' @param field_types Character vector of C type names (e.g., "int", "double")
#' @return Character string containing typedef
#' @export
#' @keywords internal
generate_api_struct_typedef <- function(struct_name, field_names, field_types = NULL) {
  if (is.null(field_types)) {
    # Default to int for all fields
    field_types <- rep("int", length(field_names))
  }
  
  if (length(field_types) != length(field_names)) {
    stop("field_types must have same length as field_names")
  }
  
  # Build field declarations
  field_decls <- paste0("    ", field_types, " ", field_names, ";", collapse = "\n")
  
  typedef_code <- sprintf(
    "typedef struct {\n%s\n} %s;\n",
    field_decls,
    struct_name
  )
  
  return(typedef_code)
}

#' Generate complete helper module for a struct (API mode)
#'
#' Combines constructor and offset extractor into a complete C file.
#'
#' @param struct_name Name of the C struct
#' @param field_names Character vector of field names
#' @param field_types Character vector of C type names (defaults to "int" for all)
#' @param header_includes Character vector of header files to include
#' @param prefix Prefix for generated function names (default "rffi_")
#' @return Character string containing complete C code
#' @export
#' @keywords internal
generate_api_struct_helpers <- function(struct_name, field_names, field_types = NULL,
                                   header_includes = NULL, prefix = "rffi_") {
  # Build includes
  includes <- c(
    "#include <R.h>",
    "#include <Rinternals.h>",
    "#include <stddef.h>",
    "#include <stdlib.h>"
  )
  
  if (!is.null(header_includes)) {
    includes <- c(includes, paste0("#include <", header_includes, ">"))
  }
  
  includes_code <- paste(includes, collapse = "\n")
  
  # Generate struct typedef if not using external header
  typedef_code <- if (is.null(header_includes)) {
    generate_api_struct_typedef(struct_name, field_names, field_types)
  } else {
    ""
  }
  
  # Generate functions
  constructor_code <- generate_api_constructor(struct_name, prefix)
  offset_code <- generate_api_offset_extractor(struct_name, field_names, prefix)
  
  # Combine
  complete_code <- paste(
    includes_code,
    "",
    typedef_code,
    constructor_code,
    "",
    offset_code,
    sep = "\n"
  )
  
  return(complete_code)
}

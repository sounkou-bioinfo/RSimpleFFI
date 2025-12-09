#' API Mode Package Generation Functions
#'
#' Functions for generating R packages with compiled struct helpers (API mode)
#'
#' @name package_api_mode
#' @keywords internal
NULL

#' Generate R wrapper code that calls compiled C accessors
#'
#' Creates R functions that use .Call() to invoke compiled struct helpers.
#'
#' @param struct_name Name of the struct
#' @param field_names Character vector of field names  
#' @param prefix Prefix for C function names (default "rffi_")
#' @param package_name Name of the package (for .Call interface)
#' @return Character string containing R code
#' @export
#' @keywords internal
generate_api_r_wrappers <- function(struct_name, field_names, prefix = "rffi_", 
                                     package_name = NULL) {
  
  escaped_name <- gsub("[^a-zA-Z0-9_]", "_", struct_name)
  offsets_func <- paste0(prefix, struct_name, "_offsets")
  new_func <- paste0(prefix, struct_name, "_new")
  
  # Use native symbol objects (no quotes) when package_name provided
  # This is required when R_forceSymbols(dll, TRUE) is used
  call_format <- if (!is.null(package_name)) {
    # Native symbol: .Call(symbol_name, PACKAGE = "pkg")
    function(sym) sprintf('.Call(%s, PACKAGE = "%s")', sym, package_name)
  } else {
    # String lookup: .Call("symbol_name")
    function(sym) sprintf('.Call("%s")', sym)
  }
  
  r_code <- sprintf('
#\' @title Struct helper functions for %s
#\' @description Compiled struct accessors (API mode) for %s
#\' @name %s_helpers
NULL

#\' Create new %s instance
#\' @return External pointer to allocated %s
#\' @export
%s_new <- function() {
  %s
}

#\' Get field offsets for %s (compiler-computed)
#\' @return Named list of field offsets
#\' @export
%s_offsets <- function() {
  %s
}

#\' Get field from %s struct
#\' @param ptr External pointer to struct
#\' @param field Field name (character) or index (integer)
#\' @return Field value
#\' @export
%s_get <- function(ptr, field) {
  if (!inherits(ptr, "externalptr")) {
    stop("ptr must be an external pointer")
  }
  
  offsets <- %s_offsets()
  
  if (is.character(field)) {
    if (!field %%in%% names(offsets)) {
      stop("Unknown field: ", field)
    }
    offset <- offsets[[field]]
  } else if (is.numeric(field)) {
    field_idx <- as.integer(field)
    if (field_idx < 1 || field_idx > length(offsets)) {
      stop("Field index out of range: ", field_idx)
    }
    offset <- offsets[[field_idx]]
  } else {
    stop("field must be character (name) or integer (index)")
  }
  
  # Use RSimpleFFI::R_field_to_r for extraction
  .Call("R_field_to_r", ptr, as.numeric(offset), PACKAGE = "RSimpleFFI")
}

#\' Set field in %s struct  
#\' @param ptr External pointer to struct
#\' @param field Field name (character) or index (integer)
#\' @param value Value to set
#\' @export
%s_set <- function(ptr, field, value) {
  if (!inherits(ptr, "externalptr")) {
    stop("ptr must be an external pointer")
  }
  
  offsets <- %s_offsets()
  
  if (is.character(field)) {
    if (!field %%in%% names(offsets)) {
      stop("Unknown field: ", field)
    }
    offset <- offsets[[field]]
    field_name <- field
  } else if (is.numeric(field)) {
    field_idx <- as.integer(field)
    if (field_idx < 1 || field_idx > length(offsets)) {
      stop("Field index out of range: ", field_idx)
    }
    offset <- offsets[[field_idx]]
    field_name <- names(offsets)[field_idx]
  } else {
    stop("field must be character (name) or integer (index)")
  }
  
  # Determine FFI type from field name or use generic pointer handling
  # For now, we use RSimpleFFI generic setter
  .Call("R_struct_set_field_generic", ptr, as.numeric(offset), value, 
        PACKAGE = "RSimpleFFI")
  
  invisible(NULL)
}
',
    struct_name, struct_name, struct_name,
    struct_name, struct_name,
    struct_name, call_format(new_func),
    struct_name,
    struct_name, call_format(offsets_func),
    struct_name,
    struct_name, escaped_name,
    struct_name,
    struct_name, escaped_name
  )
  
  return(r_code)
}

#' Generate src/init.c for R package
#'
#' Creates the C function registration code for R CMD INSTALL
#'
#' @param struct_names Character vector of struct names
#' @param prefix Prefix for C function names (default "rffi_")
#' @return Character string containing src/init.c code
#' @export
#' @keywords internal
generate_package_init_c <- function(struct_names, bitfield_funcs = list(), prefix = "rffi_") {
  
  if (length(struct_names) == 0) {
    # Empty init.c
    return('
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void R_init_PACKAGENAME(DllInfo *dll) {
  // No structs to register
}
')
  }
  
  # Generate declarations for all functions
  declarations <- character()
  callmethods <- character()
  
  for (struct_name in struct_names) {
    new_func <- paste0(prefix, struct_name, "_new")
    offsets_func <- paste0(prefix, struct_name, "_offsets")
    
    declarations <- c(
      declarations,
      sprintf("SEXP %s(void);", new_func),
      sprintf("SEXP %s(void);", offsets_func)
    )
    
    callmethods <- c(
      callmethods,
      sprintf('  {"%s", (DL_FUNC) &%s, 0},', new_func, new_func),
      sprintf('  {"%s", (DL_FUNC) &%s, 0},', offsets_func, offsets_func)
    )
    
    # Add bitfield accessors if present
    if (!is.null(bitfield_funcs[[struct_name]])) {
      for (field_name in bitfield_funcs[[struct_name]]) {
        get_func <- paste0(prefix, struct_name, "_get_", field_name)
        set_func <- paste0(prefix, struct_name, "_set_", field_name)
        
        declarations <- c(
          declarations,
          sprintf("SEXP %s(SEXP);", get_func),
          sprintf("SEXP %s(SEXP, SEXP);", set_func)
        )
        
        callmethods <- c(
          callmethods,
          sprintf('  {"%s", (DL_FUNC) &%s, 1},', get_func, get_func),
          sprintf('  {"%s", (DL_FUNC) &%s, 2},', set_func, set_func)
        )
      }
    }
  }
  
  declarations_code <- paste(declarations, collapse = "\n")
  callmethods_code <- paste(callmethods, collapse = "\n")
  
  init_c <- sprintf('
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Forward declarations
%s

static const R_CallMethodDef CallEntries[] = {
%s
  {NULL, NULL, 0}
};

void R_init_PACKAGENAME(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, FALSE);
}
', declarations_code, callmethods_code)
  
  return(init_c)
}

#' Generate src/struct_helpers.c for R package
#'
#' Creates all struct accessor functions for compilation into the package
#'
#' @param parsed_headers List of parsed header structures
#' @param header_includes Character vector of header files to include
#' @param prefix Prefix for C function names (default "rffi_")
#' @return Character string containing src/struct_helpers.c code
#' @export  
#' @keywords internal
generate_package_struct_helpers_c <- function(parsed_headers, header_includes = NULL, 
                                               prefix = "rffi_") {
  
  if (!is.list(parsed_headers) || length(parsed_headers) == 0) {
    stop("parsed_headers must be a non-empty list")
  }
  
  # Collect all struct definitions
  all_structs <- list()
  
  # Handle both single parsed header and list of parsed headers
  if ("structs" %in% names(parsed_headers)) {
    # Single parsed header
    all_structs <- parsed_headers$structs
  } else {
    # Multiple parsed headers
    for (parsed in parsed_headers) {
      if (!is.null(parsed$structs)) {
        all_structs <- c(all_structs, parsed$structs)
      }
    }
  }
  
  # Deduplicate structs by name (keep first occurrence)
  # This handles system headers being included multiple times
  if (length(all_structs) > 0) {
    struct_names <- names(all_structs)
    unique_names <- unique(struct_names)
    if (length(unique_names) < length(struct_names)) {
      # Keep first occurrence of each struct
      first_occurrences <- match(unique_names, struct_names)
      all_structs <- all_structs[first_occurrences]
      
      n_removed <- length(struct_names) - length(unique_names)
      message(sprintf("Removed %d duplicate struct definition%s", 
                     n_removed, if (n_removed != 1) "s" else ""))
    }
    
    # Filter out implementation-reserved structs (names starting with __)
    # and problematic system structs (stat, etc.)
    # Per C standard, __ prefixes are reserved for implementation use
    original_count <- length(all_structs)
    # Filter: __ prefix or exact match for problematic system structs
    all_structs <- all_structs[!grepl("^__", names(all_structs)) & !names(all_structs) %in% c("stat")]
    
    n_filtered <- original_count - length(all_structs)
    if (n_filtered > 0) {
      message(sprintf("Filtered out %d system/reserved struct%s", 
                     n_filtered, if (n_filtered != 1) "s" else ""))
    }
  }
  
  if (length(all_structs) == 0) {
    return('
#include <R.h>
#include <Rinternals.h>

// No structs found
')
  }
  
  # Build includes
  includes <- c(
    "#include <R.h>",
    "#include <Rinternals.h>",
    "#include <stddef.h>",
    "#include <stdlib.h>",
    "#include <string.h>",
    "",
    generate_bitfield_detection_macros()
  )
  
  if (!is.null(header_includes)) {
    includes <- c(includes, paste0('#include "', header_includes, '"'))
  }
  
  includes_code <- paste(includes, collapse = "\n")
  
  # Generate typedef + helpers for each struct
  struct_codes <- character()
  
  for (struct_name in names(all_structs)) {
    struct_def <- all_structs[[struct_name]]
    field_names <- vapply(struct_def, function(f) f$name, character(1))
    field_types <- vapply(struct_def, function(f) {
      # Map parsed type to C type
      map_ffi_type_to_c(f$type)
    }, character(1))
    
    # Separate bitfield and non-bitfield fields
    is_bitfield <- grepl("\\s*:\\s*\\d+", field_names)
    bitfield_names <- field_names[is_bitfield]
    non_bitfield_names <- field_names[!is_bitfield]
    non_bitfield_types <- field_types[!is_bitfield]
    
    # Parse bitfield information for compile-time detection
    bitfield_info <- list()
    if (length(bitfield_names) > 0) {
      for (bf_name in bitfield_names) {
        # Parse "field_name : width"
        parts <- strsplit(bf_name, "\\s*:\\s*")[[1]]
        clean_name <- trimws(parts[1])
        width <- as.integer(parts[2])
        
        bitfield_info[[length(bitfield_info) + 1]] <- list(
          name = bf_name,
          clean_name = clean_name,
          width = width
        )
      }
    }
    
    # Generate code for this struct
    # Skip typedef generation if using real headers (they define the structs)
    typedef_code <- if (is.null(header_includes)) {
      generate_api_struct_typedef(struct_name, field_names, field_types)
    } else {
      # Headers provide struct definitions - no typedef needed
      ""
    }
    constructor_code <- generate_api_constructor(struct_name, prefix)
    
    # Generate offset extractor for non-bitfield fields only
    if (length(non_bitfield_names) > 0) {
      offsets_code <- generate_api_offset_extractor(struct_name, non_bitfield_names, prefix)
    } else {
      # Struct has only bitfields - create empty offsets function
      func_name <- paste0(prefix, struct_name, "_offsets")
      offsets_code <- sprintf('
// All fields are bitfields - cannot use offsetof()
SEXP %s(void) {
    SEXP result = PROTECT(Rf_allocVector(VECSXP, 0));
    UNPROTECT(1);
    return result;
}
', func_name)
    }
    
    # Generate bitfield accessors if present
    bitfield_code <- ""
    if (length(bitfield_info) > 0) {
      bitfield_code <- generate_api_bitfield_accessors(struct_name, bitfield_info, prefix)
    }
    
    struct_codes <- c(
      struct_codes,
      sprintf("// Struct: %s", struct_name),
      typedef_code,
      constructor_code,
      offsets_code,
      bitfield_code,
      ""
    )
  }
  
  complete_code <- paste(
    includes_code,
    "",
    paste(struct_codes, collapse = "\n"),
    sep = "\n"
  )
  
  return(complete_code)
}

#' Map FFI type to C type string
#' @keywords internal
map_ffi_type_to_c <- function(type_info) {
  if (is.character(type_info)) {
    type_name <- type_info
  } else if (is.list(type_info)) {
    # Handle nested types like struct Point
    type_name <- type_info$type %||% type_info$name %||% "int"
    
    # If it's a struct type, return the struct name
    if (!is.null(type_info$struct_type)) {
      return(sprintf("struct %s", type_info$struct_type))
    }
  } else {
    type_name <- "int"
  }
  
  # Clean up type name (remove any pointer indicators)
  type_name <- gsub("\\*$", "", trimws(type_name))
  
  # Map common types
  type_map <- c(
    "int" = "int",
    "uint" = "unsigned int",
    "unsigned int" = "unsigned int",
    "long" = "long",
    "ulong" = "unsigned long",
    "unsigned long" = "unsigned long",
    "short" = "short",
    "ushort" = "unsigned short",
    "unsigned short" = "unsigned short",
    "char" = "char",
    "uchar" = "unsigned char",
    "unsigned char" = "unsigned char",
    "int8" = "int8_t",
    "uint8" = "uint8_t",
    "int16" = "int16_t",
    "uint16" = "uint16_t",
    "int32" = "int32_t",
    "uint32" = "uint32_t",
    "int64" = "int64_t",
    "uint64" = "uint64_t",
    "float" = "float",
    "double" = "double",
    "pointer" = "void*",
    "string" = "char*",
    "char*" = "char*",
    "size_t" = "size_t",
    "ssize_t" = "ssize_t",
    "unsigned" = "unsigned int"
  )
  
  # Return mapped type or original if not found
  if (type_name %in% names(type_map)) {
    return(type_map[[type_name]])
  }
  
  # Check if it starts with "struct " already
  if (grepl("^struct ", type_name)) {
    return(type_name)
  }
  
  # Otherwise return as-is (might be a custom typedef)
  return(type_name)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

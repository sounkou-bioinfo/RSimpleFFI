# Header Parsing and Code Generation

#' Get the master C-to-FFI type mapping
#'
#' Returns a named character vector mapping C type names to FFI type constructor calls.
#' This is the single source of truth for type mappings used across all code generation
#' functions (struct, union, typedef, function wrappers).
#'
#' Includes:
#' - Standard C types (char, int, long, etc.)
#' - Fixed-width types (int8_t, uint32_t, etc.)
#' - POSIX types (size_t, ssize_t, off_t, pid_t, etc.)
#' - glibc/musl internal types (__ssize_t, __pid_t, etc.)
#' - macOS/Darwin types (__darwin_size_t, etc.)
#' - BSD types (u_int32_t, register_t, etc.)
#' - Windows/MSYS2/MinGW types (DWORD, HANDLE, __int64, etc.)
#' - Clang/LLVM builtin types (__SIZE_TYPE__, etc.)
#'
#' @return Named character vector: names are C types, values are FFI constructor calls
#' @keywords internal
get_ffi_type_map <- function() {
  c(
    # ============================================
    # Standard C types
    # ============================================
    "char" = "ffi_char()",
    "signed char" = "ffi_char()",
    "unsigned char" = "ffi_uchar()",
    "short" = "ffi_short()",
    "short int" = "ffi_short()",
    "signed short" = "ffi_short()",
    "signed short int" = "ffi_short()",
    "unsigned short" = "ffi_ushort()",
    "unsigned short int" = "ffi_ushort()",
    "int" = "ffi_int()",
    "signed" = "ffi_int()",
    "signed int" = "ffi_int()",
    "unsigned" = "ffi_uint()",
    "unsigned int" = "ffi_uint()",
    "long" = "ffi_long()",
    "long int" = "ffi_long()",
    "signed long" = "ffi_long()",
    "signed long int" = "ffi_long()",
    "unsigned long" = "ffi_ulong()",
    "unsigned long int" = "ffi_ulong()",
    "long long" = "ffi_longlong()",
    "long long int" = "ffi_longlong()",
    "signed long long" = "ffi_longlong()",
    "signed long long int" = "ffi_longlong()",
    "unsigned long long" = "ffi_ulonglong()",
    "unsigned long long int" = "ffi_ulonglong()",
    "float" = "ffi_float()",
    "double" = "ffi_double()",
    "long double" = "ffi_longdouble()",
    "void" = "ffi_void()",
    "void*" = "ffi_pointer()",
    "void *" = "ffi_pointer()",
    "bool" = "ffi_bool()",
    "_Bool" = "ffi_bool()",
    "wchar_t" = "ffi_wchar_t()",
    "char*" = "ffi_pointer()",
    "char *" = "ffi_pointer()",
    "const char*" = "ffi_pointer()",
    "const char *" = "ffi_pointer()",

    # ============================================
    # C99/C11 fixed-width integer types
    # ============================================
    "int8_t" = "ffi_int8()",
    "int16_t" = "ffi_int16()",
    "int32_t" = "ffi_int32()",
    "int64_t" = "ffi_int64()",
    "uint8_t" = "ffi_uint8()",
    "uint16_t" = "ffi_uint16()",
    "uint32_t" = "ffi_uint32()",
    "uint64_t" = "ffi_uint64()",
    "intmax_t" = "ffi_int64()",
    "uintmax_t" = "ffi_uint64()",
    "intptr_t" = "ffi_ssize_t()",
    "uintptr_t" = "ffi_size_t()",

    # ============================================
    # C11/C++11/C++20 character types
    # (tree-sitter treats these as primitive_type)
    # ============================================
    "char8_t" = "ffi_uint8()", # C++20 UTF-8 character
    "char16_t" = "ffi_uint16()", # C11/C++11 UTF-16 character
    "char32_t" = "ffi_uint32()", # C11/C++11 UTF-32 character
    "char64_t" = "ffi_uint64()", # Non-standard extension

    # ============================================
    # Other tree-sitter primitive types
    # ============================================
    "charptr_t" = "ffi_pointer()", # char* typedef
    "nullptr_t" = "ffi_pointer()", # C23/C++11 null pointer type
    "max_align_t" = "ffi_longdouble()", # Maximum alignment type

    # ============================================
    # POSIX types
    # ============================================
    "size_t" = "ffi_size_t()",
    "ssize_t" = "ffi_ssize_t()",
    "ptrdiff_t" = "ffi_ssize_t()",
    "off_t" = "ffi_long()",
    "off64_t" = "ffi_int64()",
    "pid_t" = "ffi_int()",
    "uid_t" = "ffi_uint()",
    "gid_t" = "ffi_uint()",
    "mode_t" = "ffi_uint()",
    "dev_t" = "ffi_ulong()",
    "ino_t" = "ffi_ulong()",
    "nlink_t" = "ffi_ulong()",
    "blksize_t" = "ffi_long()",
    "blkcnt_t" = "ffi_long()",
    "time_t" = "ffi_long()",
    "clock_t" = "ffi_long()",
    "suseconds_t" = "ffi_long()",
    "socklen_t" = "ffi_uint()",
    "sig_atomic_t" = "ffi_int()",
    "fsblkcnt_t" = "ffi_ulong()",
    "fsfilcnt_t" = "ffi_ulong()",

    # ============================================
    # glibc/musl internal types (Linux)
    # ============================================
    "__ssize_t" = "ffi_ssize_t()",
    "__size_t" = "ffi_size_t()",
    "__ptrdiff_t" = "ffi_ssize_t()",
    "__intptr_t" = "ffi_ssize_t()",
    "__uintptr_t" = "ffi_size_t()",
    "__int8_t" = "ffi_int8()",
    "__int16_t" = "ffi_int16()",
    "__int32_t" = "ffi_int32()",
    "__int64_t" = "ffi_int64()",
    "__uint8_t" = "ffi_uint8()",
    "__uint16_t" = "ffi_uint16()",
    "__uint32_t" = "ffi_uint32()",
    "__uint64_t" = "ffi_uint64()",
    "__off_t" = "ffi_long()",
    "__off64_t" = "ffi_int64()",
    "__pid_t" = "ffi_int()",
    "__uid_t" = "ffi_uint()",
    "__gid_t" = "ffi_uint()",
    "__clock_t" = "ffi_long()",
    "__time_t" = "ffi_long()",
    "__suseconds_t" = "ffi_long()",
    "__dev_t" = "ffi_ulong()",
    "__ino_t" = "ffi_ulong()",
    "__ino64_t" = "ffi_uint64()",
    "__mode_t" = "ffi_uint()",
    "__nlink_t" = "ffi_ulong()",
    "__blksize_t" = "ffi_long()",
    "__blkcnt_t" = "ffi_long()",
    "__blkcnt64_t" = "ffi_int64()",
    "__fsblkcnt_t" = "ffi_ulong()",
    "__fsblkcnt64_t" = "ffi_uint64()",
    "__fsfilcnt_t" = "ffi_ulong()",
    "__fsfilcnt64_t" = "ffi_uint64()",
    "__socklen_t" = "ffi_uint()",
    "__sig_atomic_t" = "ffi_int()",
    "__wchar_t" = "ffi_wchar_t()",
    "__wint_t" = "ffi_int()",
    "__caddr_t" = "ffi_pointer()",
    "__quad_t" = "ffi_int64()",
    "__u_quad_t" = "ffi_uint64()",
    "__loff_t" = "ffi_int64()",
    "__syscall_slong_t" = "ffi_long()",
    "__syscall_ulong_t" = "ffi_ulong()",
    "__fsword_t" = "ffi_long()",
    "__kernel_long_t" = "ffi_long()",
    "__kernel_ulong_t" = "ffi_ulong()",

    # ============================================
    # macOS/Darwin types
    # ============================================
    "__darwin_size_t" = "ffi_size_t()",
    "__darwin_ssize_t" = "ffi_ssize_t()",
    "__darwin_ptrdiff_t" = "ffi_ssize_t()",
    "__darwin_intptr_t" = "ffi_ssize_t()",
    "__darwin_uintptr_t" = "ffi_size_t()",
    "__darwin_off_t" = "ffi_int64()",
    "__darwin_pid_t" = "ffi_int()",
    "__darwin_uid_t" = "ffi_uint()",
    "__darwin_gid_t" = "ffi_uint()",
    "__darwin_time_t" = "ffi_long()",
    "__darwin_clock_t" = "ffi_ulong()",
    "__darwin_suseconds_t" = "ffi_int()",
    "__darwin_dev_t" = "ffi_int()",
    "__darwin_ino_t" = "ffi_uint64()",
    "__darwin_ino64_t" = "ffi_uint64()",
    "__darwin_mode_t" = "ffi_uint16()",
    "__darwin_socklen_t" = "ffi_uint()",
    "__darwin_wchar_t" = "ffi_int()",
    "__darwin_wint_t" = "ffi_int()",
    "__darwin_blkcnt_t" = "ffi_int64()",
    "__darwin_blksize_t" = "ffi_int()",
    "__darwin_uuid_t" = "ffi_pointer()",
    "__darwin_uuid_string_t" = "ffi_pointer()",
    "__darwin_natural_t" = "ffi_uint()",
    "__darwin_mach_port_t" = "ffi_uint()",
    "__darwin_mach_port_name_t" = "ffi_uint()",
    "__darwin_sigset_t" = "ffi_uint()",
    "__darwin_pthread_t" = "ffi_pointer()",
    "__darwin_pthread_key_t" = "ffi_ulong()",

    # ============================================
    # BSD types (FreeBSD, OpenBSD, NetBSD)
    # ============================================
    "__int_least8_t" = "ffi_int8()",
    "__int_least16_t" = "ffi_int16()",
    "__int_least32_t" = "ffi_int32()",
    "__int_least64_t" = "ffi_int64()",
    "__uint_least8_t" = "ffi_uint8()",
    "__uint_least16_t" = "ffi_uint16()",
    "__uint_least32_t" = "ffi_uint32()",
    "__uint_least64_t" = "ffi_uint64()",
    "__int_fast8_t" = "ffi_int8()",
    "__int_fast16_t" = "ffi_int()",
    "__int_fast32_t" = "ffi_int()",
    "__int_fast64_t" = "ffi_int64()",
    "__uint_fast8_t" = "ffi_uint8()",
    "__uint_fast16_t" = "ffi_uint()",
    "__uint_fast32_t" = "ffi_uint()",
    "__uint_fast64_t" = "ffi_uint64()",
    "register_t" = "ffi_ssize_t()",
    "u_char" = "ffi_uchar()",
    "u_short" = "ffi_ushort()",
    "u_int" = "ffi_uint()",
    "u_long" = "ffi_ulong()",
    "u_int8_t" = "ffi_uint8()",
    "u_int16_t" = "ffi_uint16()",
    "u_int32_t" = "ffi_uint32()",
    "u_int64_t" = "ffi_uint64()",
    "quad_t" = "ffi_int64()",
    "u_quad_t" = "ffi_uint64()",
    "caddr_t" = "ffi_pointer()",
    "daddr_t" = "ffi_int()",
    "segsz_t" = "ffi_int()",
    "fixpt_t" = "ffi_uint()",

    # ============================================
    # MSYS2/MinGW/Windows types
    # ============================================
    "__int8" = "ffi_int8()",
    "__int16" = "ffi_int16()",
    "__int32" = "ffi_int32()",
    "__int64" = "ffi_int64()",
    "__mingw_off_t" = "ffi_long()",
    "__mingw_off64_t" = "ffi_int64()",
    "_off_t" = "ffi_long()",
    "_off64_t" = "ffi_int64()",
    "_dev_t" = "ffi_uint()",
    "_ino_t" = "ffi_ushort()",
    "_pid_t" = "ffi_int()",
    "_time32_t" = "ffi_int32()",
    "_time64_t" = "ffi_int64()",
    "__time32_t" = "ffi_int32()",
    "__time64_t" = "ffi_int64()",
    # Windows SDK types (uppercase)
    "BOOL" = "ffi_int()",
    "BOOLEAN" = "ffi_uchar()",
    "BYTE" = "ffi_uint8()",
    "WORD" = "ffi_uint16()",
    "DWORD" = "ffi_uint32()",
    "QWORD" = "ffi_uint64()",
    "INT" = "ffi_int()",
    "UINT" = "ffi_uint()",
    "INT8" = "ffi_int8()",
    "INT16" = "ffi_int16()",
    "INT32" = "ffi_int32()",
    "INT64" = "ffi_int64()",
    "UINT8" = "ffi_uint8()",
    "UINT16" = "ffi_uint16()",
    "UINT32" = "ffi_uint32()",
    "UINT64" = "ffi_uint64()",
    "CHAR" = "ffi_char()",
    "WCHAR" = "ffi_wchar_t()",
    "SHORT" = "ffi_short()",
    "LONG" = "ffi_long()",
    "LONGLONG" = "ffi_longlong()",
    "UCHAR" = "ffi_uchar()",
    "USHORT" = "ffi_ushort()",
    "ULONG" = "ffi_ulong()",
    "ULONGLONG" = "ffi_ulonglong()",
    "FLOAT" = "ffi_float()",
    "DOUBLE" = "ffi_double()",
    "SIZE_T" = "ffi_size_t()",
    "SSIZE_T" = "ffi_ssize_t()",
    "INT_PTR" = "ffi_ssize_t()",
    "UINT_PTR" = "ffi_size_t()",
    "LONG_PTR" = "ffi_ssize_t()",
    "ULONG_PTR" = "ffi_size_t()",
    "DWORD_PTR" = "ffi_size_t()",
    "HANDLE" = "ffi_pointer()",
    "HMODULE" = "ffi_pointer()",
    "HINSTANCE" = "ffi_pointer()",
    "HWND" = "ffi_pointer()",
    "HDC" = "ffi_pointer()",
    "HBITMAP" = "ffi_pointer()",
    "HBRUSH" = "ffi_pointer()",
    "HFONT" = "ffi_pointer()",
    "HICON" = "ffi_pointer()",
    "HCURSOR" = "ffi_pointer()",
    "HMENU" = "ffi_pointer()",
    "HKEY" = "ffi_pointer()",
    "HRESULT" = "ffi_long()",
    "PVOID" = "ffi_pointer()",
    "LPVOID" = "ffi_pointer()",
    "LPCVOID" = "ffi_pointer()",
    "LPSTR" = "ffi_pointer()",
    "LPCSTR" = "ffi_pointer()",
    "LPWSTR" = "ffi_pointer()",
    "LPCWSTR" = "ffi_pointer()",
    "LPTSTR" = "ffi_pointer()",
    "LPCTSTR" = "ffi_pointer()",
    "BSTR" = "ffi_pointer()",
    "WPARAM" = "ffi_size_t()",
    "LPARAM" = "ffi_ssize_t()",
    "LRESULT" = "ffi_ssize_t()",
    "ATOM" = "ffi_uint16()",
    "COLORREF" = "ffi_uint32()",

    # ============================================
    # Clang/LLVM/GCC builtin type macros
    # ============================================
    "__INTPTR_TYPE__" = "ffi_ssize_t()",
    "__UINTPTR_TYPE__" = "ffi_size_t()",
    "__SIZE_TYPE__" = "ffi_size_t()",
    "__PTRDIFF_TYPE__" = "ffi_ssize_t()",
    "__WCHAR_TYPE__" = "ffi_wchar_t()",
    "__WINT_TYPE__" = "ffi_int()",
    "__INT8_TYPE__" = "ffi_int8()",
    "__INT16_TYPE__" = "ffi_int16()",
    "__INT32_TYPE__" = "ffi_int32()",
    "__INT64_TYPE__" = "ffi_int64()",
    "__UINT8_TYPE__" = "ffi_uint8()",
    "__UINT16_TYPE__" = "ffi_uint16()",
    "__UINT32_TYPE__" = "ffi_uint32()",
    "__UINT64_TYPE__" = "ffi_uint64()",
    "__INTMAX_TYPE__" = "ffi_int64()",
    "__UINTMAX_TYPE__" = "ffi_uint64()",
    "__SIG_ATOMIC_TYPE__" = "ffi_int()"
  )
}

#' Get list of resolvable type names
#'
#' Returns a character vector of all type names that can be directly resolved
#' to FFI types, for use in can_resolve_typedef().
#'
#' @return Character vector of resolvable type names
#' @keywords internal
get_resolvable_types <- function() {
  names(get_ffi_type_map())
}

#' Generate bit-field accessor code
#' @param struct_name Name of the struct
#' @param bitfield_specs Character vector of "'name : width'" strings
#' @return Character string with accessor code
#' @keywords internal
generate_bitfield_accessor_code <- function(struct_name, bitfield_specs) {
  # Parse field specs: 'enabled : 1' -> list(name='enabled', width=1)
  fields <- list()
  for (spec in bitfield_specs) {
    # Remove quotes and split by :
    clean_spec <- gsub("'", "", spec)
    parts <- strsplit(clean_spec, ":")[[1]]
    if (length(parts) == 2) {
      field_name <- trimws(parts[1])
      field_width <- as.integer(trimws(parts[2]))
      fields[[field_name]] <- field_width
    }
  }

  if (length(fields) == 0) {
    return(sprintf(
      "# %s - bit-field struct (see ?ffi_create_bitfield_accessors)",
      struct_name
    ))
  }

  # Generate code
  field_list <- paste(
    sprintf("  %s = %dL", names(fields), unlist(fields)),
    collapse = ",\n"
  )

  code <- sprintf(
    "# Bit-field accessor for %s\n# C struct has bit-fields: %s\n%s <- ffi_create_bitfield_accessors(\n  list(\n%s\n  )\n)\n# Usage:\n#  packed <- %s$pack(list(%s))\n#  %s$get(packed, \"%s\")\n#  packed <- %s$set(packed, \"%s\", new_value)",
    struct_name,
    paste(names(fields), collapse = ", "),
    struct_name,
    field_list,
    struct_name,
    paste(
      sprintf("%s = 0L", names(fields)[1:min(2, length(fields))]),
      collapse = ", "
    ),
    struct_name,
    names(fields)[1],
    struct_name,
    names(fields)[1]
  )

  code
}

#' Parse C header file and create structured result
#'
#' Uses tree-sitter for robust AST-based parsing of C headers.
#'
#' @param header_file Path to C header file
#' @param includes Additional include directories
#' @return List with parsed components (file, defines, structs, unions, enums, functions, typedefs)
#' @export
ffi_parse_header <- function(header_file, includes = NULL) {
  ffi_parse_header_ts(header_file, includes)
}

#' Generate R struct definition from parsed struct
#' @param struct_name Name of the struct
#' @param struct_def Struct definition from parsed header
#' @param typedefs Optional data frame of typedefs to resolve type aliases
#' @return Character vector with R code
#' @export
generate_struct_definition <- function(
  struct_name,
  struct_def,
  typedefs = NULL
) {
  if (length(struct_def) == 0) {
    return(NULL)
  }

  # Check for packed attribute
  is_packed <- isTRUE(attr(struct_def, "packed"))

  if (is_packed) {
    warning(
      sprintf(
        "Struct '%s' is packed (__attribute__((packed)) or #pragma pack).\n  Packed structs cannot be passed by value to C functions (libffi limitation).\n  Use pointers instead. Generated code includes pack=1.",
        struct_name
      ),
      call. = FALSE
    )
  }

  # Check for bit-field warning
  bitfield_warning <- attr(struct_def, "bitfield_warning")
  has_bitfields <- !is.null(bitfield_warning) && bitfield_warning$has_bitfields

  if (has_bitfields) {
    warning(
      sprintf(
        "Struct '%s' contains bit-fields which are not supported by libffi.\n  Fields: %s\n  See README section 'Bit-fields and Struct Packing' for workarounds.",
        struct_name,
        paste(bitfield_warning$fields, collapse = ", ")
      ),
      call. = FALSE
    )

    # Generate bit-field accessor code instead of struct
    return(generate_bitfield_accessor_code(
      struct_name,
      bitfield_warning$fields
    ))
  }

  # Get centralized type map
  type_map <- get_ffi_type_map()

  # Generate field definitions
  field_defs <- character()
  for (field in struct_def) {
    field_type <- trimws(field$type)
    field_name <- field$name

    # Handle arrays: type[N] or type[N][M] -> extract base type and sizes
    # Match patterns like int[10] or double[3][3]
    if (grepl("\\[", field_type)) {
      # Extract base type (everything before first [)
      base_type_str <- sub("\\[.*", "", field_type)

      # Extract all array dimensions from type
      dimensions <- regmatches(
        field_type,
        gregexpr("\\[([0-9]+)\\]", field_type, perl = TRUE)
      )[[1]]
      dimensions <- as.integer(gsub("\\[|\\]", "", dimensions))

      if (length(dimensions) > 0) {
        # Get FFI type for base type
        base_comment <- NULL
        if (base_type_str %in% names(type_map)) {
          base_type <- type_map[[base_type_str]]
        } else if (grepl("\\*", base_type_str)) {
          base_type <- "ffi_pointer()"
        } else {
          base_type <- "ffi_pointer()" # Unknown type, use pointer
          base_comment <- base_type_str
        }

        # Build nested array types for multi-dimensional arrays
        # For double[3][3], we want ffi_array_type(ffi_array_type(ffi_double(), 3L), 3L)
        ffi_type <- base_type
        for (dim in rev(dimensions)) {
          ffi_type <- sprintf("ffi_array_type(%s, %dL)", ffi_type, dim)
        }

        # Escape field name if needed
        escaped_field_name <- escape_r_name(field_name)
        if (!is.null(base_comment)) {
          field_defs <- c(
            field_defs,
            sprintf("  %s = %s  # %s", escaped_field_name, ffi_type, field_type)
          )
        } else {
          field_defs <- c(
            field_defs,
            sprintf("  %s = %s  # %s", escaped_field_name, ffi_type, field_type)
          )
        }
      }
    } else {
      # Regular field (no array)
      # Handle pointers
      field_comment <- NULL
      if (grepl("\\*", field_type)) {
        ffi_type <- "ffi_pointer()"
      } else if (field_type %in% names(type_map)) {
        ffi_type <- type_map[[field_type]]
      } else if (!is.null(typedefs) && field_type %in% names(typedefs)) {
        # Resolve typedef to underlying type
        base_type <- typedefs[[field_type]]
        if (grepl("\\*", base_type)) {
          ffi_type <- "ffi_pointer()"
        } else if (base_type %in% names(type_map)) {
          ffi_type <- type_map[[base_type]]
        } else {
          ffi_type <- "ffi_pointer()"
          field_comment <- field_type
        }
      } else {
        # Unknown type - could be nested struct, use pointer
        ffi_type <- "ffi_pointer()"
        field_comment <- field_type
      }

      # Escape field name if needed
      escaped_field_name <- escape_r_name(field_name)
      if (!is.null(field_comment)) {
        field_defs <- c(
          field_defs,
          sprintf(
            "  %s = %s  # %s",
            escaped_field_name,
            ffi_type,
            field_comment
          )
        )
      } else {
        field_defs <- c(
          field_defs,
          sprintf("  %s = %s", escaped_field_name, ffi_type)
        )
      }
    }
  }

  # Add commas to all fields except the last one (or all if we're adding pack)
  # Need to insert comma BEFORE any comment (# ...)
  if (length(field_defs) > 0) {
    last_field_idx <- if (is_packed) length(field_defs) else length(field_defs)
    for (i in seq_along(field_defs)) {
      needs_comma <- (i < length(field_defs)) || is_packed
      if (needs_comma) {
        if (grepl("#", field_defs[i])) {
          # Has a comment - insert comma before the comment
          field_defs[i] <- sub("  #", ",  #", field_defs[i])
        } else {
          # No comment - just append comma
          field_defs[i] <- paste0(field_defs[i], ",")
        }
      }
    }
  }

  # Generate struct code
  if (is_packed) {
    code <- c(
      sprintf(
        "# WARNING: Packed struct - cannot be passed by value, use pointers"
      ),
      sprintf("%s <- ffi_struct(", struct_name),
      paste(field_defs, collapse = "\n"),
      "  .pack = 1L",
      ")"
    )
  } else {
    code <- c(
      sprintf("%s <- ffi_struct(", struct_name),
      paste(field_defs, collapse = "\n"),
      ")"
    )
  }

  paste(code, collapse = "\n")
}

#' Generate R enum definition from parsed enum
#' @param enum_name Name of the enum
#' @param enum_values Named integer vector of enum values
#' @return Character vector with R code
#' @export
generate_enum_definition <- function(enum_name, enum_values) {
  if (length(enum_values) == 0) {
    return(NULL)
  }

  # Create named value pairs
  value_defs <- character()
  for (i in seq_along(enum_values)) {
    name <- names(enum_values)[i]
    value <- enum_values[[i]]
    escaped_name <- escape_r_name(name)
    value_defs <- c(value_defs, sprintf("  %s = %dL", escaped_name, value))
  }

  # Add commas to all except last
  if (length(value_defs) > 1) {
    value_defs[-length(value_defs)] <- paste0(
      value_defs[-length(value_defs)],
      ","
    )
  }

  # Generate enum code
  code <- c(
    sprintf("%s <- ffi_enum(", enum_name),
    paste(value_defs, collapse = "\n"),
    ")"
  )

  paste(code, collapse = "\n")
}

#' Generate R union definition from parsed union
#' @param union_name Name of the union
#' @param union_def Union definition (list of fields)
#' @return Character vector with R code
#' @export
generate_union_definition <- function(union_name, union_def) {
  if (length(union_def) == 0) {
    return(NULL)
  }

  # Get centralized type map
  type_map <- get_ffi_type_map()

  # Generate field definitions
  field_defs <- character()
  for (field in union_def) {
    field_type <- trimws(field$type)
    field_name <- field$name

    # Get FFI type
    if (field_type %in% names(type_map)) {
      ffi_type <- type_map[[field_type]]
    } else if (grepl("\\*", field_type)) {
      ffi_type <- "ffi_pointer()"
    } else {
      # Unknown type - add comment
      ffi_type <- sprintf("ffi_pointer()  # %s", field_type)
    }

    escaped_field_name <- escape_r_name(field_name)
    field_defs <- c(
      field_defs,
      sprintf("  %s = %s", escaped_field_name, ffi_type)
    )
  }

  # Add commas
  if (length(field_defs) > 1) {
    # Insert comma before comment if present, otherwise append
    for (i in seq_along(field_defs)[-length(field_defs)]) {
      if (grepl("#", field_defs[i])) {
        field_defs[i] <- sub("  #", ",  #", field_defs[i])
      } else {
        field_defs[i] <- paste0(field_defs[i], ",")
      }
    }
  }

  # Generate union code
  code <- c(
    sprintf("%s <- ffi_union(", union_name),
    paste(field_defs, collapse = "\n"),
    ")"
  )

  paste(code, collapse = "\n")
}

#' Generate struct helper functions (allocator, from_list, to_list)
#'
#' Creates convenience functions for working with a struct type:
#' - `new_<struct>()`: Allocate a new struct, optionally initialize from values
#' - `<struct>_to_list()`: Convert struct pointer to R list
#'
#' @param struct_name Name of the struct (should match the ffi_struct variable name)
#' @param field_names Character vector of field names
#' @return Character string with R code for helper functions
#' @export
generate_struct_helpers <- function(struct_name, field_names) {
  # Clean name for function prefix (remove backticks if present)
  clean_name <- gsub("`", "", struct_name)

  # Escape function names if they would start with _ (invalid R identifiers)
  # new_<name> is fine if name starts with letter, but new___foo needs escaping
  new_func_name <- paste0("new_", clean_name)
  to_list_func_name <- paste0(clean_name, "_to_list")

  # If the resulting function name starts with _ or the clean_name starts with _,
  # we need backticks
  if (grepl("^_", clean_name)) {
    new_func_name <- paste0("`", new_func_name, "`")
    to_list_func_name <- paste0("`", to_list_func_name, "`")
  }

  # Clean field names for use as R parameter names
  # Remove array brackets like [2], replace invalid chars with underscore
  clean_field_names <- gsub("\\[.*\\]", "", field_names) # Remove [n] suffixes
  clean_field_names <- gsub("[^a-zA-Z0-9_.]", "_", clean_field_names) # Replace invalid chars

  # Escape field names using the existing escape_r_name function
  # This handles R reserved words (next, if, else, etc.) and names starting with _
  escaped_field_names <- vapply(
    clean_field_names,
    escape_r_name,
    character(1),
    USE.NAMES = FALSE
  )

  # Generate parameter documentation (use clean names without backticks for docs)
  field_params <- paste(
    sprintf(
      "#' @param %s Value for %s field (optional)",
      clean_field_names,
      field_names
    ),
    collapse = "\n"
  )

  # Build the args list string mapping clean param names to original field names
  args_list_items <- sprintf('"%s" = %s', field_names, escaped_field_names)

  code <- c(
    sprintf("#' Create a new %s struct", clean_name),
    "#'",
    "#' Allocates and optionally initializes a new struct.",
    "#'",
    field_params,
    sprintf("#' @return External pointer to allocated %s struct", clean_name),
    "#' @export",
    sprintf(
      "%s <- function(%s) {",
      new_func_name,
      paste(paste0(escaped_field_names, " = NULL"), collapse = ", ")
    ),
    sprintf(
      "
  ptr <- ffi_alloc(%s)
  args <- list(%s)
  args <- args[!vapply(args, is.null, logical(1))]
  if (length(args) > 0) {
    for (nm in names(args)) {
      ffi_set_field(ptr, nm, args[[nm]], %s)
    }
  }
  ptr
}",
      struct_name,
      paste(args_list_items, collapse = ", "),
      struct_name
    ),
    "",
    sprintf("#' Convert %s pointer to R list", clean_name),
    "#'",
    sprintf("#' @param ptr External pointer to %s struct", clean_name),
    "#' @return Named list with field values",
    "#' @export",
    sprintf("%s <- function(ptr) {", to_list_func_name),
    sprintf("  ffi_struct_to_list(ptr, %s)", struct_name),
    "}",
    ""
  )

  paste(code, collapse = "\n")
}

#' Check if a typedef can be fully resolved to a real type
#'
#' Recursively follows typedef chains to see if the final base type
#' can be mapped to an FFI type. Returns FALSE for unresolvable types
#' like `__builtin_va_list`.
#'
#' @param base_type The underlying C type string
#' @param known_typedefs Named character vector of typedefs
#' @param known_structs Character vector of known struct names
#' @param visited Set of already-visited typedefs (to detect cycles)
#' @return TRUE if resolvable, FALSE otherwise
#' @keywords internal
can_resolve_typedef <- function(
  base_type,
  known_typedefs,
  known_structs = character(),
  visited = character()
) {
  # Avoid infinite loops from cycles

  if (base_type %in% visited) {
    return(FALSE)
  }
  visited <- c(visited, base_type)

  # Get resolvable types from centralized type map
  resolvable_types <- get_resolvable_types()

  # Pointer types are always resolvable
  if (grepl("\\*", base_type)) {
    return(TRUE)
  }

  # Known types are resolvable (from centralized type map)
  if (base_type %in% resolvable_types) {
    return(TRUE)
  }

  # Struct types
  if (grepl("^struct\\s+", base_type)) {
    struct_name <- sub("^struct\\s+", "", base_type)
    return(struct_name %in% known_structs)
  }

  # If it's another typedef, recursively check
  if (base_type %in% names(known_typedefs)) {
    underlying <- known_typedefs[[base_type]]
    return(can_resolve_typedef(
      underlying,
      known_typedefs,
      known_structs,
      visited
    ))
  }

  # Unknown/unresolvable type
  FALSE
}

#' Generate R typedef alias from parsed typedef
#'
#' Creates R code that maps a typedef alias to its underlying FFI type.
#' Handles simple type aliases (e.g., typedef int my_int) and struct typedefs.
#'
#' @param alias_name Name of the typedef alias
#' @param base_type The underlying C type string
#' @param known_structs Character vector of known struct names (for struct type resolution)
#' @param known_typedefs Named character vector of already-processed typedefs (for chained resolution)
#' @return Character string with R code, or NULL if type cannot be mapped
#' @export
generate_typedef_definition <- function(
  alias_name,
  base_type,
  known_structs = character(),
  known_typedefs = character()
) {
  # Get centralized type map
  type_map <- get_ffi_type_map()

  escaped_alias <- escape_r_name(alias_name)

  # Check for pointer types
  if (grepl("\\*", base_type)) {
    # char* or const char* -> ffi_string()
    if (grepl("char\\s*\\*", base_type)) {
      return(sprintf(
        "%s <- ffi_string()  # typedef %s",
        escaped_alias,
        base_type
      ))
    }
    # Other pointers -> ffi_pointer()
    return(sprintf(
      "%s <- ffi_pointer()  # typedef %s",
      escaped_alias,
      base_type
    ))
  }

  # Check exact match in type_map
  if (base_type %in% names(type_map)) {
    return(sprintf(
      "%s <- %s  # typedef %s",
      escaped_alias,
      type_map[[base_type]],
      base_type
    ))
  }

  # Check for struct type: "struct Name"
  if (grepl("^struct\\s+", base_type)) {
    struct_name <- sub("^struct\\s+", "", base_type)
    escaped_struct <- escape_r_name(struct_name)
    if (struct_name %in% known_structs) {
      # Reference to a known struct - alias points to the struct
      return(sprintf(
        "%s <- %s  # typedef struct %s",
        escaped_alias,
        escaped_struct,
        struct_name
      ))
    } else {
      # Forward declaration or unknown struct - comment only
      return(sprintf(
        "# %s: forward declaration of struct %s (define struct first)",
        escaped_alias,
        struct_name
      ))
    }
  }

  # Check for typedef-of-typedef (base_type is another typedef)
  # Only chain if the underlying typedef can actually be resolved
  if (base_type %in% names(known_typedefs)) {
    if (can_resolve_typedef(base_type, known_typedefs, known_structs)) {
      # Chain to the underlying typedef
      return(sprintf(
        "%s <- %s  # typedef %s",
        escaped_alias,
        escape_r_name(base_type),
        base_type
      ))
    }
    # Otherwise fall through to unknown type handling
  }

  # Unknown type - add as comment
  sprintf(
    "# %s: unknown type '%s' - manual mapping required",
    escaped_alias,
    base_type
  )
}

#' Escape R name with backticks if needed
#' @param name Variable name to escape
#' @return Escaped name if needed, original otherwise
escape_r_name <- function(name) {
  # R reserved words and special constants that must be escaped
  reserved_words <- c(
    # Control flow keywords
    "if",
    "else",
    "repeat",
    "while",
    "function",
    "for",
    "in",
    "next",
    "break",
    # Logical constants
    "TRUE",
    "FALSE",
    # Special values
    "NULL",
    "Inf",
    "NaN",
    # NA variants
    "NA",
    "NA_integer_",
    "NA_real_",
    "NA_complex_",
    "NA_character_",
    # Additional R internal names that could conflict
    "..."
  )

  # Check if name is a reserved word or invalid R identifier
  # Single underscore or names starting with underscore need backticks
  if (
    name %in%
      reserved_words ||
      name == "_" ||
      !grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", name) ||
      grepl("^_", name)
  ) {
    return(paste0("`", name, "`"))
  }
  name
}

#' Generate R function wrapper from parsed function
#' @param func_def Function definition (row from functions data.frame)
#' @param typedefs Named character vector of typedefs (optional). Used to resolve
#'   typedef'd types like SEXPTYPE to their underlying FFI types.
#' @return Character vector with R code
#' @export
generate_function_wrapper <- function(func_def, typedefs = NULL) {
  func_name <- func_def$name
  return_type <- func_def$return_type

  # Get centralized type map
  type_map <- get_ffi_type_map()

  # Build a typedef resolution map: typedef name -> FFI type string
  # This resolves types like SEXPTYPE -> ffi_uint() based on parsed typedefs
  typedef_ffi_map <- list()
  if (!is.null(typedefs) && length(typedefs) > 0) {
    for (td_name in names(typedefs)) {
      base_type <- typedefs[[td_name]]
      # Check if base type is a known FFI type
      if (base_type %in% names(type_map)) {
        typedef_ffi_map[[td_name]] <- type_map[[base_type]]
      } else if (grepl("\\*", base_type)) {
        typedef_ffi_map[[td_name]] <- "ffi_pointer()"
      }
    }
  }

  # Verify we have structured parameter list from tree-sitter
  if (!("param_list" %in% names(func_def)) || is.null(func_def$param_list)) {
    stop("Function definition must include param_list from tree-sitter parser")
  }

  # Use tree-sitter parsed parameters (clean, no attributes, no regex parsing)
  param_names <- character()
  param_types_c <- character()
  param_types_ffi <- character()
  is_variadic <- FALSE

  # Extract the actual parameter list (it's a named list keyed by function name)
  params <- func_def$param_list[[1]]

  # Process each parameter from AST
  for (param in params) {
    if (isTRUE(param$is_variadic)) {
      is_variadic <- TRUE
      next
    }

    param_type <- param$type
    param_name <- param$name

    # Generate name if missing
    if (is.null(param_name) || param_name == "") {
      param_name <- paste0("arg", length(param_names) + 1)
    } else {
      # Check for duplicates
      if (param_name %in% param_names) {
        param_name <- paste0("arg", length(param_names) + 1)
      } else {
        param_name <- escape_r_name(param_name)
      }
    }

    param_names <- c(param_names, param_name)
    param_types_c <- c(param_types_c, param_type)

    # Map to FFI type
    ffi_type <- map_type_to_ffi(param_type, type_map, typedef_ffi_map)
    param_types_ffi <- c(param_types_ffi, ffi_type)
  }

  # Generate wrapper code
  return_type_ffi <- map_type_to_ffi(
    return_type,
    type_map,
    typedef_ffi_map,
    is_return = TRUE
  )

  generate_wrapper_code(
    func_name,
    return_type_ffi,
    param_names,
    param_types_c,
    param_types_ffi,
    is_variadic
  )
}

#' Map C type to FFI type string
#' @keywords internal
map_type_to_ffi <- function(
  type_string,
  type_map,
  typedef_ffi_map,
  is_return = FALSE
) {
  type_string <- trimws(type_string)

  # Handle empty or NULL type
  if (is.null(type_string) || length(type_string) == 0 || type_string == "") {
    return("ffi_pointer()") # Default to pointer for unknown/empty types
  }

  # Check for pointer types
  if (grepl("\\*", type_string)) {
    return("ffi_pointer()")
  }
  # Check exact match in type_map (built-in types)
  if (type_string %in% names(type_map)) {
    return(type_map[[type_string]])
  }
  # Check if it's a resolved typedef
  if (type_string %in% names(typedef_ffi_map)) {
    return(typedef_ffi_map[[type_string]])
  }
  # Default to pointer for unknown types (likely structs)
  return("ffi_pointer()")
}

#' Generate wrapper code from parameter information
#' @keywords internal
generate_wrapper_code <- function(
  func_name,
  return_type_ffi,
  param_names,
  param_types_c,
  param_types_ffi,
  is_variadic
) {
  r_func_name <- paste0("r_", func_name)

  # Handle variadic functions specially
  if (is_variadic) {
    n_fixed <- length(param_names)
    code <- c(
      sprintf(
        "# Variadic function: %s(%s, ...)",
        func_name,
        paste(param_types_c, collapse = ", ")
      ),
      sprintf(
        "# This function has variable arguments (...) which require ffi_cif_var()."
      ),
      sprintf("# Fixed args: %d, then variadic arguments follow.", n_fixed),
      "#",
      "# Example usage:",
      sprintf("# sym <- ffi_symbol(\"%s\")", func_name),
      if (n_fixed > 0) {
        c(
          sprintf("# # With %d fixed arg(s) + variadic args:", n_fixed),
          sprintf(
            "# cif <- ffi_cif_var(%s, %dL, %s, <variadic_arg_types...>)",
            return_type_ffi,
            n_fixed,
            paste(param_types_ffi, collapse = ", ")
          ),
          sprintf(
            "# ffi_call(cif, sym, %s, <variadic_args...>)",
            paste(param_names, collapse = ", ")
          )
        )
      } else {
        c(
          "# # With only variadic args:",
          sprintf(
            "# cif <- ffi_cif_var(%s, 0L, <variadic_arg_types...>)",
            return_type_ffi
          ),
          "# ffi_call(cif, sym, <variadic_args...>)"
        )
      },
      ""
    )
    return(paste(code, collapse = "\n"))
  }

  # Build ffi_function call for non-variadic functions
  if (length(param_types_ffi) == 0) {
    ffi_call <- sprintf(
      '  .fn <- ffi_function("%s", %s)',
      func_name,
      return_type_ffi
    )
    signature <- paste0(r_func_name, " <- function()")
    call_line <- "  .fn()"
  } else {
    ffi_params <- paste(param_types_ffi, collapse = ", ")
    ffi_call <- sprintf(
      '  .fn <- ffi_function("%s", %s, %s)',
      func_name,
      return_type_ffi,
      ffi_params
    )
    signature <- sprintf(
      "%s <- function(%s)",
      r_func_name,
      paste(param_names, collapse = ", ")
    )
    call_line <- sprintf("  .fn(%s)", paste(param_names, collapse = ", "))
  }

  # Generate parameter documentation
  param_docs <- character()
  if (length(param_names) > 0) {
    for (i in seq_along(param_names)) {
      param_docs <- c(
        param_docs,
        sprintf(
          "#' @param %s (%s) %s",
          param_names[i],
          param_types_ffi[i],
          param_types_c[i]
        )
      )
    }
  }

  # Full function code
  code <- c(
    sprintf("#' Wrapper for C function: %s", func_name),
    if (length(param_docs) > 0) param_docs else NULL,
    sprintf("#' @return (%s)", return_type_ffi),
    "#' @export",
    signature,
    " {",
    ffi_call,
    call_line,
    "}",
    ""
  )

  paste(code, collapse = "\n")
}

#' Sort typedefs by dependency order (topological sort)
#'
#' Ensures that typedef-of-typedef chains are ordered correctly,
#' with base types defined before types that depend on them.
#'
#' @param typedefs Named character vector where names are aliases and values are base types
#' @return Character vector of typedef names in dependency order
#' @keywords internal
sort_typedefs_by_dependency <- function(typedefs) {
  if (length(typedefs) == 0) {
    return(character())
  }

  typedef_names <- names(typedefs)
  n <- length(typedef_names)

  # Build dependency graph
  # deps[[name]] = names that 'name' depends on (its base type if also a typedef)
  deps <- vector("list", n)
  names(deps) <- typedef_names

  for (alias_name in typedef_names) {
    base_type <- typedefs[[alias_name]]
    # Check if base_type is another typedef in our list
    if (base_type %in% typedef_names) {
      deps[[alias_name]] <- base_type
    } else {
      deps[[alias_name]] <- character(0)
    }
  }

  # Kahn's algorithm for topological sort
  # Count incoming edges (how many typedefs depend on each)
  in_degree <- stats::setNames(rep(0L, n), typedef_names)
  for (alias_name in typedef_names) {
    for (dep in deps[[alias_name]]) {
      in_degree[[dep]] <- in_degree[[dep]] + 1L
    }
  }

  # Start with nodes that have no dependencies pointing to them
  # BUT we want base types first, so we want types with no deps first
  result <- character(0)
  processed <- stats::setNames(rep(FALSE, n), typedef_names)

  # Simple DFS-based topological sort
  visit <- function(name) {
    if (processed[[name]]) {
      return()
    }
    # Visit dependencies first
    for (dep in deps[[name]]) {
      visit(dep)
    }
    result <<- c(result, name)
    processed[[name]] <- TRUE
  }

  for (name in typedef_names) {
    visit(name)
  }

  result
}

#' Generate R bindings from parsed header
#' @param parsed_header Parsed header object from ffi_parse_header()
#' @param output_file Optional file to write code to
#' @param verbose If TRUE, print progress messages
#' @return Character vector with all generated R code
#' @export
generate_r_bindings <- function(
  parsed_header,
  output_file = NULL,
  verbose = FALSE
) {
  code_sections <- list()

  # Header comment with source file hash
  source_hash <- if (file.exists(parsed_header$file)) {
    tools::md5sum(parsed_header$file)
  } else {
    "unknown"
  }

  code_sections$header <- c(
    sprintf("# Auto-generated R bindings for %s", basename(parsed_header$file)),
    sprintf("# Generated on: %s", Sys.time()),
    sprintf("# Source hash: %s", source_hash),
    "#",
    "# NOTE: These functions expect symbols to be available in the current process.",
    "# For external libraries, load them first with dll_load() or use dll_ffi_symbol().",
    "#",
    "# Type handling:",
    "#  - Primitives (int, double, etc.): passed by value, auto-converted",
    "#  - char*: use ffi_pointer(), use pointer_to_string() for conversion to string",
    "#  - struct Foo*: use ffi_pointer(), allocate with ffi_struct() + ffi_alloc()",
    "#  - Struct fields: access with ffi_get_field() and ffi_set_field()",
    "#  - Union fields: same as structs, all fields share memory at offset 0",
    "#  - Enums: convert with ffi_enum_to_int() and ffi_int_to_enum()",
    ""
  )

  # Defines as R constants (escape invalid names with backticks)
  if (length(parsed_header$defines) > 0) {
    code_sections$defines <- c(
      "# Constants from #define",
      ""
    )
    for (name in names(parsed_header$defines)) {
      value <- parsed_header$defines[[name]]
      if (value != "") {
        escaped_name <- escape_r_name(name)

        # Clean up C-specific patterns that R doesn't understand

        # 0. Skip wide string literals (L"...") - can't represent in R
        if (grepl('^L"', value) || grepl("^L'", value)) {
          next
        }

        # 1. String concatenation: "hello" "world" -> "helloworld"
        if (grepl('^"[^"]*"\\s+"[^"]*"', value)) {
          # Extract all string literals and concatenate
          strings <- regmatches(value, gregexpr('"[^"]*"', value))[[1]]
          # Remove quotes, concatenate, re-quote
          value <- paste0(
            '"',
            paste(gsub('"', "", strings), collapse = ""),
            '"'
          )
        }

        # 2. Character literals: '\n' -> "\n" (R uses strings not chars)
        # Need to handle escape sequences carefully
        if (grepl("^'.*'$", value)) {
          # Extract the content between quotes (allow empty with .*)
          content <- gsub("^'(.*)'$", "\\1", value)
          # R string: just re-quote (escapes like \n, \t work the same)
          # But \' becomes ' in R strings (no need to escape single quote)
          content <- gsub("\\\\'", "'", content)
          # And \" is not needed in single-quote context, but we're using double quotes
          value <- paste0('"', content, '"')
        }

        # 3. Binary literals: 0b11111111 -> convert to decimal or hex
        if (grepl("^0[bB][01]+$", value)) {
          # Convert binary to decimal
          binary_str <- sub("^0[bB]", "", value)
          decimal_val <- sum(
            as.integer(strsplit(binary_str, "")[[1]]) *
              2^(nchar(binary_str):1 - 1)
          )
          value <- as.character(decimal_val)
        }

        # 4. Digit separators: 1'000'000 -> 1000000
        if (grepl("'", value)) {
          value <- gsub("'", "", value)
        }

        # 5. Negative hex: -0x80000000 -> keep as is, will be quoted later

        # 6. Strip C literal suffixes from numeric values
        # Handle hex values: 0x123ULL, 0xabcLL, etc. - strip U and L suffixes only
        if (grepl("^0[xX][0-9a-fA-F]+[UuLl]+$", value)) {
          value <- sub("[UuLl]+$", "", value)
        } else if (grepl("^[0-9]+[LlUu]+$", value)) {
          # Handle decimal integers: 123LL, 456ULL, etc.
          value <- sub("[LlUu]+$", "", value)
          # Add single L for R integer (if not too large)
          if (as.numeric(value) <= 2147483647) {
            value <- paste0(value, "L")
          }
        } else if (grepl("^[0-9.+-]+[Ff]$", value) && !grepl("^0[xX]", value)) {
          # Handle floats: 3.14f, 1.0F (but NOT hex like 0xF)
          value <- sub("[Ff]$", "", value)
        } else if (grepl("^[0-9.+-]+[eE][+-]?[0-9]+[LlUuFf]*$", value)) {
          # Handle exponential notation: 1.0e10f
          value <- sub("[LlUuFf]+$", "", value)
        }

        # 7. String values from character literals are already properly escaped
        # Skip re-escaping to avoid breaking escape sequences like \n

        # Quote value if it's not a number or already quoted
        if (
          !grepl(
            "^(0[xX][0-9a-fA-F]+|[0-9.+-]+([eE][+-]?[0-9]+)?L?)$",
            value
          ) &&
            !grepl("^['\"]", value)
        ) {
          # Escape any internal quotes and backslashes before wrapping
          value <- gsub("\\\\", "\\\\\\\\", value) # Escape backslashes first
          value <- gsub('"', '\\\\"', value) # Escape double quotes
          value <- paste0('"', value, '"')
        }

        code_sections$defines <- c(
          code_sections$defines,
          sprintf("%s <- %s", escaped_name, value)
        )
      }
    }
    code_sections$defines <- c(code_sections$defines, "")
  }

  # Struct definitions (escape invalid names with backticks)
  if (length(parsed_header$structs) > 0) {
    code_sections$structs <- c(
      "# Struct definitions",
      ""
    )
    code_sections$struct_helpers <- c(
      "# Struct helper functions",
      ""
    )
    for (struct_name in names(parsed_header$structs)) {
      struct_def <- parsed_header$structs[[struct_name]]
      escaped_name <- escape_r_name(struct_name)
      struct_code <- generate_struct_definition(
        escaped_name,
        struct_def,
        typedefs = parsed_header$typedefs
      )
      if (!is.null(struct_code)) {
        code_sections$structs <- c(
          code_sections$structs,
          struct_code,
          ""
        )
        # Generate helper functions for this struct
        field_names <- vapply(struct_def, function(f) f$name, character(1))
        if (length(field_names) > 0) {
          helper_code <- generate_struct_helpers(escaped_name, field_names)
          code_sections$struct_helpers <- c(
            code_sections$struct_helpers,
            helper_code
          )
        }
      }
    }
  }

  # Enum definitions
  if (length(parsed_header$enums) > 0) {
    code_sections$enums <- c(
      "# Enum definitions",
      ""
    )
    for (enum_name in names(parsed_header$enums)) {
      enum_values <- parsed_header$enums[[enum_name]]
      escaped_name <- escape_r_name(enum_name)
      enum_code <- generate_enum_definition(escaped_name, enum_values)
      if (!is.null(enum_code)) {
        code_sections$enums <- c(
          code_sections$enums,
          enum_code,
          ""
        )
      }
    }
  }

  # Union definitions
  if (length(parsed_header$unions) > 0) {
    code_sections$unions <- c(
      "# Union definitions",
      ""
    )
    for (union_name in names(parsed_header$unions)) {
      union_def <- parsed_header$unions[[union_name]]
      escaped_name <- escape_r_name(union_name)
      union_code <- generate_union_definition(escaped_name, union_def)
      if (!is.null(union_code)) {
        code_sections$unions <- c(
          code_sections$unions,
          union_code,
          ""
        )
      }
    }
  }

  # Typedef aliases
  if (length(parsed_header$typedefs) > 0) {
    code_sections$typedefs <- c(
      "# Type aliases (typedefs)",
      ""
    )

    # Get known struct names for resolution
    known_structs <- names(parsed_header$structs)
    typedefs <- parsed_header$typedefs

    # Sort typedefs by dependency order (topological sort)
    # A typedef depends on another if its base_type is another typedef name
    sorted_names <- sort_typedefs_by_dependency(typedefs)

    for (alias_name in sorted_names) {
      base_type <- typedefs[[alias_name]]
      typedef_code <- generate_typedef_definition(
        alias_name,
        base_type,
        known_structs = known_structs,
        known_typedefs = typedefs
      )
      if (!is.null(typedef_code)) {
        code_sections$typedefs <- c(
          code_sections$typedefs,
          typedef_code
        )
      }
    }
    code_sections$typedefs <- c(code_sections$typedefs, "")
  }

  # Function wrappers
  if (nrow(parsed_header$functions) > 0) {
    code_sections$functions <- c(
      "# Function wrappers",
      ""
    )

    # Filter out system functions (those starting with extern)
    user_funcs <- parsed_header$functions[
      !grepl("^extern", parsed_header$functions$return_type),
    ]

    # Pre-generate all function wrappers (avoid O(n²) concatenation)
    n_funcs <- nrow(user_funcs)
    if (n_funcs > 0) {
      if (verbose) {
        message(sprintf("  Generating %d function wrappers...", n_funcs))
      }
      func_codes <- vapply(
        seq_len(n_funcs),
        function(i) {
          if (verbose && i %% 500 == 0) {
            message(sprintf("    Progress: %d/%d functions", i, n_funcs))
          }
          paste0(
            generate_function_wrapper(
              user_funcs[i, ],
              typedefs = parsed_header$typedefs
            ),
            "\n"
          )
        },
        character(1)
      )
      code_sections$functions <- c(code_sections$functions, func_codes)
    }
  }

  if (verbose) {
    message("  Combining code sections...")
  }
  all_code <- unlist(code_sections)

  # Combine into single string for easier use
  code_string <- paste(all_code, collapse = "\n")

  # Write to file if requested
  if (!is.null(output_file)) {
    writeLines(all_code, output_file)
    message("Generated R bindings written to: ", output_file)
  }

  code_string
}

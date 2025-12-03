# Header Parsing and Code Generation (Functional Style)

#' Strip C type qualifiers from a type string
#'
#' Removes const, volatile, restrict, _Atomic, and related qualifiers
#' from a C type string, normalizing whitespace.
#'
#' @param type_str Character string containing a C type
#' @return Cleaned type string with qualifiers removed
#' @keywords internal
strip_type_qualifiers <- function(type_str) {
  # Remove qualifiers (word boundaries to avoid partial matches)
  type_str <- gsub("\\bconst\\b", "", type_str)
  type_str <- gsub("\\bvolatile\\b", "", type_str)
  type_str <- gsub("\\brestrict\\b", "", type_str)
  type_str <- gsub("\\b__restrict\\b", "", type_str)
  type_str <- gsub("\\b__restrict__\\b", "", type_str)
  type_str <- gsub("\\b_Atomic\\b", "", type_str)
  type_str <- gsub("\\bregister\\b", "", type_str)
  type_str <- gsub("\\binline\\b", "", type_str)
  type_str <- gsub("\\b__inline\\b", "", type_str)
  type_str <- gsub("\\b__inline__\\b", "", type_str)
  type_str <- gsub("\\bextern\\b", "", type_str)
  type_str <- gsub("\\bstatic\\b", "", type_str)
  # Normalize whitespace

  type_str <- gsub("\\s+", " ", type_str)
  trimws(type_str)
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
    return(sprintf("# %s - bit-field struct (see ?ffi_create_bitfield_accessors)", struct_name))
  }

  # Generate code
  field_list <- paste(sprintf("  %s = %dL", names(fields), unlist(fields)), collapse = ",\n")

  code <- sprintf(
    "# Bit-field accessor for %s\n# C struct has bit-fields: %s\n%s <- ffi_create_bitfield_accessors(\n  list(\n%s\n  )\n)\n# Usage:\n#  packed <- %s$pack(list(%s))\n#  %s$get(packed, \"%s\")\n#  packed <- %s$set(packed, \"%s\", new_value)",
    struct_name,
    paste(names(fields), collapse = ", "),
    struct_name,
    field_list,
    struct_name,
    paste(sprintf("%s = 0L", names(fields)[1:min(2, length(fields))]), collapse = ", "),
    struct_name,
    names(fields)[1],
    struct_name,
    names(fields)[1]
  )

  code
}

#' Parse C header file and create structured result
#' @param header_file Path to C header file
#' @param includes Additional include directories
#' @return List with parsed components (file, defines, structs, unions, enums, functions, typedefs)
#' @export
ffi_parse_header <- function(header_file, includes = NULL) {
  if (!tcc_available()) {
    stop("TinyCC not available. Package may not be installed correctly.")
  }

  if (!file.exists(header_file)) {
    stop("Header file not found: ", header_file)
  }

  # Use TCC to parse header
  preprocessed <- tcc_preprocess(header_file, includes = includes)

  # Extract components
  defines <- tcc_extract_defines(
    header_file = header_file,
    preprocessed_lines = preprocessed
  )
  structs <- tcc_extract_structs(preprocessed)
  unions <- tcc_extract_unions(preprocessed)
  enums <- tcc_extract_enums(preprocessed)
  functions <- tcc_extract_functions(preprocessed)
  typedefs <- tcc_extract_typedefs(preprocessed)

  # Create structured result
  structure(
    list(
      file = header_file,
      defines = defines,
      structs = structs,
      unions = unions,
      enums = enums,
      functions = functions,
      typedefs = typedefs
    ),
    class = "parsed_header"
  )
}

#' Generate R struct definition from parsed struct
#' @param struct_def Struct definition from parsed header
#' @param struct_name Name of the struct
#' @return Character vector with R code
#' @export
generate_struct_definition <- function(struct_name, struct_def) {
  if (length(struct_def) == 0) {
    return(NULL)
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
    return(generate_bitfield_accessor_code(struct_name, bitfield_warning$fields))
  }

  # Map C types to FFI types
  type_map <- c(
    "int" = "ffi_int()",
    "long" = "ffi_long()",
    "short" = "ffi_short()",
    "char" = "ffi_char()",
    "float" = "ffi_float()",
    "double" = "ffi_double()",
    "void*" = "ffi_pointer()",
    "void *" = "ffi_pointer()",
    "size_t" = "ffi_size_t()",
    "uint8_t" = "ffi_uint8()",
    "uint16_t" = "ffi_uint16()",
    "uint32_t" = "ffi_uint32()",
    "uint64_t" = "ffi_uint64()",
    "int8_t" = "ffi_int8()",
    "int16_t" = "ffi_int16()",
    "int32_t" = "ffi_int32()",
    "int64_t" = "ffi_int64()",
    "unsigned char" = "ffi_uint8()",
    "unsigned short" = "ffi_uint16()",
    "unsigned int" = "ffi_uint32()",
    "unsigned long" = "ffi_ulong()",
    "long long" = "ffi_longlong()",
    "long long int" = "ffi_longlong()",
    "signed long long" = "ffi_longlong()",
    "signed long long int" = "ffi_longlong()",
    "unsigned long long" = "ffi_ulonglong()",
    "unsigned long long int" = "ffi_ulonglong()",
    "ssize_t" = "ffi_ssize_t()",
    "ptrdiff_t" = "ffi_ssize_t()",
    "bool" = "ffi_bool()",
    "_Bool" = "ffi_bool()"
  )

  # Generate field definitions
  field_defs <- character()
  for (field in struct_def) {
    field_type <- strip_type_qualifiers(trimws(field$type))
    field_name <- field$name

    # Handle arrays: name[N] or name[N][M] -> extract name and sizes
    # Match patterns like name[10] or matrix[3][3]
    if (grepl("\\[", field_name)) {
      # Extract base name and array dimensions
      base_name <- sub("\\[.*", "", field_name)

      # Extract all array dimensions
      dimensions <- regmatches(field_name, gregexpr("\\[([0-9]+)\\]", field_name, perl = TRUE))[[1]]
      dimensions <- as.integer(gsub("\\[|\\]", "", dimensions))

      if (length(dimensions) > 0) {
        # Get base type
        if (field_type %in% names(type_map)) {
          base_type <- type_map[[field_type]]
        } else if (grepl("\\*", field_type)) {
          base_type <- "ffi_pointer()"
        } else {
          base_type <- paste0("ffi_pointer()") # Will add comment separately
          base_comment <- field_type
        }

        # Build nested array types for multi-dimensional arrays
        ffi_type <- base_type
        for (dim in rev(dimensions)) {
          ffi_type <- sprintf("ffi_array_type(%s, %dL)", ffi_type, dim)
        }

        # Escape field name if needed
        escaped_field_name <- escape_r_name(base_name)
        if (exists("base_comment", inherits = FALSE)) {
          field_defs <- c(field_defs, sprintf("  %s = %s  # %s", escaped_field_name, ffi_type, base_comment))
          rm(base_comment)
        } else {
          field_defs <- c(field_defs, sprintf("  %s = %s", escaped_field_name, ffi_type))
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
      } else {
        # Unknown type - could be typedef or struct, use pointer
        ffi_type <- "ffi_pointer()"
        field_comment <- field_type
      }

      # Escape field name if needed
      escaped_field_name <- escape_r_name(field_name)
      if (!is.null(field_comment)) {
        field_defs <- c(field_defs, sprintf("  %s = %s  # %s", escaped_field_name, ffi_type, field_comment))
      } else {
        field_defs <- c(field_defs, sprintf("  %s = %s", escaped_field_name, ffi_type))
      }
    }
  }

  # Add commas to all fields except the last one
  # Need to insert comma BEFORE any comment (# ...)
  if (length(field_defs) > 0) {
    for (i in seq_along(field_defs)[-length(field_defs)]) {
      if (grepl("#", field_defs[i])) {
        # Has a comment - insert comma before the comment
        field_defs[i] <- sub("  #", ",  #", field_defs[i])
      } else {
        # No comment - just append comma
        field_defs[i] <- paste0(field_defs[i], ",")
      }
    }
  }

  # Generate struct code
  code <- c(
    sprintf("%s <- ffi_struct(", struct_name),
    paste(field_defs, collapse = "\n"),
    ")"
  )

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
    value_defs[-length(value_defs)] <- paste0(value_defs[-length(value_defs)], ",")
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

  # Map C types to FFI types (reuse logic from struct generation)
  type_map <- c(
    "int" = "ffi_int()",
    "long" = "ffi_long()",
    "short" = "ffi_short()",
    "char" = "ffi_char()",
    "float" = "ffi_float()",
    "double" = "ffi_double()",
    "void*" = "ffi_pointer()",
    "void *" = "ffi_pointer()",
    "size_t" = "ffi_size_t()",
    "uint8_t" = "ffi_uint8()",
    "uint16_t" = "ffi_uint16()",
    "uint32_t" = "ffi_uint32()",
    "uint64_t" = "ffi_uint64()",
    "int8_t" = "ffi_int8()",
    "int16_t" = "ffi_int16()",
    "int32_t" = "ffi_int32()",
    "int64_t" = "ffi_int64()",
    "unsigned char" = "ffi_uint8()",
    "unsigned short" = "ffi_uint16()",
    "unsigned int" = "ffi_uint32()",
    "unsigned long" = "ffi_uint64()"
  )

  # Generate field definitions
  field_defs <- character()
  for (field in union_def) {
    field_type <- strip_type_qualifiers(trimws(field$type))
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
    field_defs <- c(field_defs, sprintf("  %s = %s", escaped_field_name, ffi_type))
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
can_resolve_typedef <- function(base_type, known_typedefs, known_structs = character(), visited = character()) {
  # Strip qualifiers
  base_type <- strip_type_qualifiers(base_type)

  # Avoid infinite loops from cycles

  if (base_type %in% visited) {
    return(FALSE)
  }
  visited <- c(visited, base_type)

  # Basic type map (subset for checking)
  # Includes standard types AND common system-level aliases (glibc internals)
  basic_types <- c(
    # Standard C types
    "char", "signed char", "unsigned char",
    "short", "short int", "signed short", "signed short int",
    "unsigned short", "unsigned short int",
    "int", "signed", "signed int", "unsigned", "unsigned int",
    "long", "long int", "signed long", "signed long int",
    "unsigned long", "unsigned long int",
    "long long", "long long int", "signed long long", "signed long long int",
    "unsigned long long", "unsigned long long int",
    "float", "double", "long double", "void",
    "size_t", "ssize_t", "ptrdiff_t",
    "int8_t", "int16_t", "int32_t", "int64_t",
    "uint8_t", "uint16_t", "uint32_t", "uint64_t",
    "bool", "_Bool",
    # Common system-level aliases (glibc, musl, etc.)
    "__ssize_t", "__size_t", "__ptrdiff_t", "__intptr_t", "__uintptr_t",
    "__int8_t", "__int16_t", "__int32_t", "__int64_t",
    "__uint8_t", "__uint16_t", "__uint32_t", "__uint64_t",
    "__off_t", "__off64_t", "__pid_t", "__uid_t", "__gid_t",
    "__clock_t", "__time_t", "__suseconds_t",
    "__dev_t", "__ino_t", "__ino64_t", "__mode_t", "__nlink_t",
    "__blksize_t", "__blkcnt_t", "__blkcnt64_t",
    "__fsblkcnt_t", "__fsblkcnt64_t", "__fsfilcnt_t", "__fsfilcnt64_t",
    "__socklen_t", "__sig_atomic_t"
  )

  # Pointer types are always resolvable
  if (grepl("\\*", base_type)) {
    return(TRUE)
  }

  # Basic types are resolvable
  if (base_type %in% basic_types) {
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
    return(can_resolve_typedef(underlying, known_typedefs, known_structs, visited))
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
generate_typedef_definition <- function(alias_name, base_type, known_structs = character(), known_typedefs = character()) {
  # Strip qualifiers from base type
  base_type <- strip_type_qualifiers(base_type)


  # Type map for basic C types AND common system-level aliases
  # This allows direct resolution without typedef chains
  type_map <- c(
    # Standard C types
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
    "size_t" = "ffi_size_t()",
    "ssize_t" = "ffi_ssize_t()",
    "ptrdiff_t" = "ffi_ssize_t()",
    "int8_t" = "ffi_int8()",
    "int16_t" = "ffi_int16()",
    "int32_t" = "ffi_int32()",
    "int64_t" = "ffi_int64()",
    "uint8_t" = "ffi_uint8()",
    "uint16_t" = "ffi_uint16()",
    "uint32_t" = "ffi_uint32()",
    "uint64_t" = "ffi_uint64()",
    "bool" = "ffi_bool()",
    "_Bool" = "ffi_bool()",
    # System-level aliases (glibc internal types) - platform-appropriate mappings
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
    # POSIX types (typically long or int depending on platform)
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
    "__sig_atomic_t" = "ffi_int()"
  )

  escaped_alias <- escape_r_name(alias_name)

  # Check for pointer types
  if (grepl("\\*", base_type)) {
    # char* or const char* -> ffi_string()
    if (grepl("char\\s*\\*", base_type)) {
      return(sprintf("%s <- ffi_string()  # typedef %s", escaped_alias, base_type))
    }
    # Other pointers -> ffi_pointer()
    return(sprintf("%s <- ffi_pointer()  # typedef %s", escaped_alias, base_type))
  }

  # Check exact match in type_map
  if (base_type %in% names(type_map)) {
    return(sprintf("%s <- %s  # typedef %s", escaped_alias, type_map[[base_type]], base_type))
  }

  # Check for struct type: "struct Name"
  if (grepl("^struct\\s+", base_type)) {
    struct_name <- sub("^struct\\s+", "", base_type)
    escaped_struct <- escape_r_name(struct_name)
    if (struct_name %in% known_structs) {
      # Reference to a known struct - alias points to the struct
      return(sprintf("%s <- %s  # typedef struct %s", escaped_alias, escaped_struct, struct_name))
    } else {
      # Forward declaration or unknown struct - comment only
      return(sprintf("# %s: forward declaration of struct %s (define struct first)", escaped_alias, struct_name))
    }
  }

  # Check for typedef-of-typedef (base_type is another typedef)
  # Only chain if the underlying typedef can actually be resolved
  if (base_type %in% names(known_typedefs)) {
    if (can_resolve_typedef(base_type, known_typedefs, known_structs)) {
      # Chain to the underlying typedef
      return(sprintf("%s <- %s  # typedef %s", escaped_alias, escape_r_name(base_type), base_type))
    }
    # Otherwise fall through to unknown type handling
  }

  # Unknown type - add as comment
  sprintf("# %s: unknown type '%s' - manual mapping required", escaped_alias, base_type)
}

#' Escape R name with backticks if needed
#' @param name Variable name to escape
#' @return Escaped name if needed, original otherwise
escape_r_name <- function(name) {
  # R reserved words that must be escaped
  reserved_words <- c(
    "if", "else", "repeat", "while", "function", "for", "in", "next", "break",
    "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_",
    "NA_complex_", "NA_character_"
  )

  # Check if name is a reserved word or invalid R identifier
  # Single underscore or names starting with underscore need backticks
  if (name %in% reserved_words ||
    name == "_" ||
    !grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", name) ||
    grepl("^_", name)) {
    return(paste0("`", name, "`"))
  }
  name
}

#' Generate R function wrapper from parsed function
#' @param func_def Function definition (row from functions data.frame)
#' @return Character vector with R code
#' @export
generate_function_wrapper <- function(func_def) {
  func_name <- func_def$name
  return_type <- func_def$return_type
  params <- func_def$params

  # Map C type to FFI type call
  type_map <- c(
    "int" = "ffi_int()",
    "double" = "ffi_double()",
    "float" = "ffi_float()",
    "char" = "ffi_char()",
    "void" = "ffi_void()",
    "short" = "ffi_short()",
    "long" = "ffi_long()",
    "unsigned int" = "ffi_uint()",
    "unsigned char" = "ffi_uchar()",
    "unsigned short" = "ffi_ushort()",
    "unsigned long" = "ffi_ulong()",
    "long long" = "ffi_longlong()",
    "long long int" = "ffi_longlong()",
    "signed long long" = "ffi_longlong()",
    "signed long long int" = "ffi_longlong()",
    "unsigned long long" = "ffi_ulonglong()",
    "unsigned long long int" = "ffi_ulonglong()",
    "int8_t" = "ffi_int8()",
    "int16_t" = "ffi_int16()",
    "int32_t" = "ffi_int32()",
    "int64_t" = "ffi_int64()",
    "uint8_t" = "ffi_uint8()",
    "uint16_t" = "ffi_uint16()",
    "uint32_t" = "ffi_uint32()",
    "uint64_t" = "ffi_uint64()",
    "size_t" = "ffi_size_t()",
    "ssize_t" = "ffi_ssize_t()",
    "ptrdiff_t" = "ffi_ssize_t()",
    "bool" = "ffi_bool()",
    "_Bool" = "ffi_bool()",
    "char*" = "ffi_string()",
    "const char*" = "ffi_string()"
  )

  # Function to map a C type to FFI type (handles both "type name" and "type")
  map_type_from_string <- function(type_string) {
    type_string <- strip_type_qualifiers(trimws(type_string))

    # Check for pointer types (treat as ffi_pointer())
    if (grepl("\\*", type_string) && !grepl("char\\s*\\*", type_string)) {
      return("ffi_pointer()")
    }
    # Check exact match in type_map
    if (type_string %in% names(type_map)) {
      return(type_map[[type_string]])
    }
    # Default to pointer for unknown types (likely structs)
    return("ffi_pointer()")
  }

  # Function to extract type from "type varname" parameter
  extract_type <- function(param_decl) {
    param_decl <- trimws(param_decl)
    tokens <- strsplit(param_decl, "\\s+")[[1]]
    if (length(tokens) < 2) {
      return(strip_type_qualifiers(param_decl)) # No variable name, just type
    }
    # Type is all tokens except the last (which is the variable name)
    type_part <- paste(tokens[-length(tokens)], collapse = " ")
    return(strip_type_qualifiers(trimws(type_part)))
  }

  # Parse parameters
  param_parts <- strsplit(params, ",")[[1]]
  param_names <- character()
  param_types_c <- character()
  param_types_ffi <- character()

  # Common C type keywords (to detect unnamed parameters)
  c_types <- c(
    "void", "char", "short", "int", "long", "float", "double",
    "signed", "unsigned", "const", "volatile", "struct", "union",
    "enum", "size_t", "ssize_t", "ptrdiff_t", "wchar_t",
    "int8_t", "int16_t", "int32_t", "int64_t",
    "uint8_t", "uint16_t", "uint32_t", "uint64_t",
    "FILE", "_Bool", "bool"
  )

  for (part in param_parts) {
    part <- trimws(part)
    if (part == "" || part == "void") next

    # Extract parameter name (last word, remove * and [])
    tokens <- strsplit(part, "\\s+")[[1]]
    if (length(tokens) > 0) {
      param_name <- tokens[length(tokens)]
      # Remove pointers, arrays, and clean up
      param_name <- gsub("\\*+", "", param_name) # Remove *
      param_name <- gsub("\\[.*?\\]", "", param_name) # Remove []
      param_name <- trimws(param_name)

      # Check if param_name is actually a C type (meaning no variable name was provided)
      is_type_keyword <- param_name %in% c_types

      if (param_name != "" && !grepl("^[0-9]", param_name) && !is_type_keyword) {
        # Check for duplicate names and make unique
        if (param_name %in% param_names) {
          # Generate unique name if duplicate
          param_name <- paste0("arg", length(param_names) + 1)
        } else {
          # Escape invalid R names (underscore, reserved words, etc.)
          param_name <- escape_r_name(param_name)
        }
        param_names <- c(param_names, param_name)
      } else {
        # Generate a name for unnamed parameters or type keywords
        param_names <- c(param_names, paste0("arg", length(param_names) + 1))
      }

      # Type is everything except last token
      param_type_c <- extract_type(part)
      param_types_c <- c(param_types_c, param_type_c)
      param_types_ffi <- c(param_types_ffi, map_type_from_string(param_type_c))
    }
  }

  # Map return type (it's just a type, no variable name)
  return_ffi <- map_type_from_string(return_type)

  # Generate R function
  r_func_name <- paste0("r_", func_name)

  # Build ffi_function call
  if (length(param_types_ffi) == 0) {
    ffi_call <- sprintf('  .fn <- ffi_function("%s", %s)', func_name, return_ffi)
    signature <- paste0(r_func_name, " <- function()")
    call_line <- "  .fn()"
  } else {
    ffi_params <- paste(param_types_ffi, collapse = ", ")
    ffi_call <- sprintf('  .fn <- ffi_function("%s", %s, %s)', func_name, return_ffi, ffi_params)
    signature <- paste0(
      r_func_name, " <- function(",
      paste(param_names, collapse = ", "),
      ")"
    )
    call_line <- sprintf("  .fn(%s)", paste(param_names, collapse = ", "))
  }

  # Add parameter documentation with C types
  param_docs <- character()
  if (length(param_names) > 0) {
    for (i in seq_along(param_names)) {
      param_docs <- c(
        param_docs,
        sprintf("#' @param %s %s", param_names[i], param_types_c[i])
      )
    }
  }

  code <- c(
    sprintf("#' Wrapper for C function: %s %s(%s)", return_type, func_name, params),
    "#'",
    param_docs,
    sprintf("#' @return %s", return_type),
    sprintf("#' @export"),
    signature,
    "{",
    ffi_call,
    call_line,
    "}"
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
  in_degree <- setNames(rep(0L, n), typedef_names)
  for (alias_name in typedef_names) {
    for (dep in deps[[alias_name]]) {
      in_degree[[dep]] <- in_degree[[dep]] + 1L
    }
  }

  # Start with nodes that have no dependencies pointing to them
  # BUT we want base types first, so we want types with no deps first
  result <- character(0)
  processed <- setNames(rep(FALSE, n), typedef_names)

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
#' @return Character vector with all generated R code
#' @export
generate_r_bindings <- function(parsed_header, output_file = NULL) {
  code_sections <- list()

  # Header comment
  code_sections$header <- c(
    sprintf("# Auto-generated R bindings for %s", basename(parsed_header$file)),
    sprintf("# Generated on: %s", Sys.time()),
    "#",
    "# NOTE: These functions expect symbols to be available in the current process.",
    "# For external libraries, load them first with dll_load() or use dll_ffi_symbol().",
    "#",
    "# Type handling:",
    "#  - Primitives (int, double, etc.): passed by value, auto-converted",
    "#  - char*: use ffi_string(), automatically converts to/from R character",
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

        # 1. String concatenation: "hello" "world" -> "helloworld"
        if (grepl('^"[^"]*"\\s+"[^"]*"', value)) {
          # Extract all string literals and concatenate
          strings <- regmatches(value, gregexpr('"[^"]*"', value))[[1]]
          # Remove quotes, concatenate, re-quote
          value <- paste0('"', paste(gsub('"', "", strings), collapse = ""), '"')
        }

        # 2. Character literals: '\n' -> "\n" (R uses strings not chars)
        # Need to handle escape sequences carefully
        if (grepl("^'.*'$", value)) {
          # Extract the content between quotes
          content <- gsub("^'(.+)'$", "\\1", value)
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
          decimal_val <- sum(as.integer(strsplit(binary_str, "")[[1]]) * 2^(nchar(binary_str):1 - 1))
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
        }
        # Handle decimal integers: 123LL, 456ULL, etc.
        else if (grepl("^[0-9]+[LlUu]+$", value)) {
          value <- sub("[LlUu]+$", "", value)
          # Add single L for R integer (if not too large)
          if (as.numeric(value) <= 2147483647) {
            value <- paste0(value, "L")
          }
        }
        # Handle floats: 3.14f, 1.0F (but NOT hex like 0xF)
        else if (grepl("^[0-9.+-]+[Ff]$", value) && !grepl("^0[xX]", value)) {
          value <- sub("[Ff]$", "", value)
        }
        # Handle exponential notation: 1.0e10f
        else if (grepl("^[0-9.+-]+[eE][+-]?[0-9]+[LlUuFf]*$", value)) {
          value <- sub("[LlUuFf]+$", "", value)
        }

        # Quote value if it's not a number or already quoted
        if (!grepl("^(0[xX][0-9a-fA-F]+|[0-9.+-]+([eE][+-]?[0-9]+)?L?)$", value) && !grepl("^['\"]", value)) {
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
    for (struct_name in names(parsed_header$structs)) {
      struct_def <- parsed_header$structs[[struct_name]]
      escaped_name <- escape_r_name(struct_name)
      struct_code <- generate_struct_definition(escaped_name, struct_def)
      if (!is.null(struct_code)) {
        code_sections$structs <- c(
          code_sections$structs,
          struct_code,
          ""
        )
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

    for (i in seq_len(nrow(user_funcs))) {
      func_code <- generate_function_wrapper(user_funcs[i, ])
      code_sections$functions <- c(
        code_sections$functions,
        func_code,
        ""
      )
    }
  }

  # Combine all sections
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

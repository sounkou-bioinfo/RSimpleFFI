# Header Parsing and Code Generation (Functional Style)

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
#' @return List with parsed components (file, defines, structs, unions, enums, functions)
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
  defines <- tcc_extract_defines(header_file = header_file, 
                                   preprocessed_lines = preprocessed)
  structs <- tcc_extract_structs(preprocessed)
  unions <- tcc_extract_unions(preprocessed)
  enums <- tcc_extract_enums(preprocessed)
  functions <- tcc_extract_functions(preprocessed)
  
  # Create structured result
  structure(
    list(
      file = header_file,
      defines = defines,
      structs = structs,
      unions = unions,
      enums = enums,
      functions = functions
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
    "unsigned long" = "ffi_uint64()"
  )
  
  # Generate field definitions
  field_defs <- character()
  for (field in struct_def) {
    field_type <- trimws(field$type)
    field_name <- field$name
    
    # Handle arrays: name[N] or name[N][M] -> extract name and sizes
    # Match patterns like name[10] or matrix[3][3]
    if (grepl("\\[", field_name)) {
      # Extract base name and array dimensions
      base_name <- sub("\\[.*", "", field_name)
      
      # Extract all array dimensions
      dimensions <- regmatches(field_name, gregexpr("\\[([0-9]+)\\]", field_name, perl=TRUE))[[1]]
      dimensions <- as.integer(gsub("\\[|\\]", "", dimensions))
      
      if (length(dimensions) > 0) {
        # Get base type
        if (field_type %in% names(type_map)) {
          base_type <- type_map[[field_type]]
        } else if (grepl("\\*", field_type)) {
          base_type <- "ffi_pointer()"
        } else {
          base_type <- paste0("ffi_pointer()")  # Will add comment separately
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
    "int8_t" = "ffi_int8()",
    "int16_t" = "ffi_int16()",
    "int32_t" = "ffi_int32()",
    "int64_t" = "ffi_int64()",
    "uint8_t" = "ffi_uint8()",
    "uint16_t" = "ffi_uint16()",
    "uint32_t" = "ffi_uint32()",
    "uint64_t" = "ffi_uint64()",
    "size_t" = "ffi_size_t()",
    "bool" = "ffi_bool()",
    "_Bool" = "ffi_bool()",
    "char*" = "ffi_string()",
    "const char*" = "ffi_string()"
  )
  
  # Function to map a C type to FFI type (handles both "type name" and "type")
  map_type_from_string <- function(type_string) {
    type_string <- trimws(type_string)
    
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
      return(param_decl)  # No variable name, just type
    }
    # Type is all tokens except the last (which is the variable name)
    type_part <- paste(tokens[-length(tokens)], collapse = " ")
    return(trimws(type_part))
  }
  
  # Parse parameters
  param_parts <- strsplit(params, ",")[[1]]
  param_names <- character()
  param_types_c <- character()
  param_types_ffi <- character()
  
  # Common C type keywords (to detect unnamed parameters)
  c_types <- c("void", "char", "short", "int", "long", "float", "double", 
               "signed", "unsigned", "const", "volatile", "struct", "union", 
               "enum", "size_t", "ssize_t", "ptrdiff_t", "wchar_t",
               "int8_t", "int16_t", "int32_t", "int64_t",
               "uint8_t", "uint16_t", "uint32_t", "uint64_t",
               "FILE", "_Bool", "bool")
  
  for (part in param_parts) {
    part <- trimws(part)
    if (part == "" || part == "void") next
    
    # Extract parameter name (last word, remove * and [])
    tokens <- strsplit(part, "\\s+")[[1]]
    if (length(tokens) > 0) {
      param_name <- tokens[length(tokens)]
      # Remove pointers, arrays, and clean up
      param_name <- gsub("\\*+", "", param_name)  # Remove *
      param_name <- gsub("\\[.*?\\]", "", param_name)  # Remove []
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
    signature <- paste0(r_func_name, " <- function(", 
                        paste(param_names, collapse = ", "), 
                        ")")
    call_line <- sprintf("  .fn(%s)", paste(param_names, collapse = ", "))
  }
  
  # Add parameter documentation with C types
  param_docs <- character()
  if (length(param_names) > 0) {
    for (i in seq_along(param_names)) {
      param_docs <- c(param_docs, 
                      sprintf("#' @param %s %s", param_names[i], param_types_c[i]))
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
          value <- paste0('"', paste(gsub('"', '', strings), collapse = ''), '"')
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

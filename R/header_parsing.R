# Header Parsing and Code Generation (Functional Style)

#' Parse C header file and create structured result
#' @param header_file Path to C header file
#' @param includes Additional include directories
#' @return List with parsed components (file, defines, structs, functions)
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
  functions <- tcc_extract_functions(preprocessed)
  
  # Create structured result
  structure(
    list(
      file = header_file,
      defines = defines,
      structs = structs,
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
      if (param_name != "" && !grepl("^[0-9]", param_name)) {
        param_names <- c(param_names, param_name)
      } else {
        # Generate a name for unnamed parameters
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
  if (name %in% reserved_words ||
      !grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", name) ||
      grepl("^_", name)) {
    return(paste0("`", name, "`"))
  }
  name
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
        # Strip C literal suffixes (LL, ULL, L, U, etc.) from numeric values
        # R doesn't recognize these and they cause parse errors
        if (grepl("^[0-9.+-]+[LlUu]+$", value)) {
          value <- sub("[LlUu]+$", "", value)
          # If it's a large integer, add L suffix for R (but only one L)
          if (grepl("^[0-9]+$", value) && !grepl("\\.", value)) {
            value <- paste0(value, "L")
          }
        }
        # Quote value if it's not a number or already quoted
        if (!grepl("^[0-9.+-]+L?$", value) && !grepl("^['\"]", value)) {
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

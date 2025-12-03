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
          base_type <- paste0("ffi_pointer()  # ", field_type)
        }
        
        # Build nested array types for multi-dimensional arrays
        ffi_type <- base_type
        for (dim in rev(dimensions)) {
          ffi_type <- sprintf("ffi_array_type(%s, %dL)", ffi_type, dim)
        }
        
        field_defs <- c(field_defs, sprintf("  %s = %s", base_name, ffi_type))
      }
    } else {
      # Regular field (no array)
      # Handle pointers
      if (grepl("\\*", field_type)) {
        ffi_type <- "ffi_pointer()"
      } else if (field_type %in% names(type_map)) {
        ffi_type <- type_map[[field_type]]
      } else {
        # Unknown type - could be typedef or struct, use pointer
        ffi_type <- paste0("ffi_pointer()  # ", field_type)
      }
      
      field_defs <- c(field_defs, sprintf("  %s = %s", field_name, ffi_type))
    }
  }
  
  # Generate struct code
  code <- c(
    sprintf("%s <- ffi_struct(", struct_name),
    paste(field_defs, collapse = ",\n"),
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
  
  # Parse parameters
  param_parts <- strsplit(params, ",")[[1]]
  param_names <- character()
  param_types <- character()
  
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
      param_type <- paste(tokens[-length(tokens)], collapse = " ")
      param_types <- c(param_types, param_type)
    }
  }
  
  # Generate R function
  r_func_name <- paste0("r_", func_name)
  
  if (length(param_names) == 0) {
    signature <- paste0(r_func_name, " <- function(lib = NULL)")
    params_r <- "  # No parameters"
  } else {
    signature <- paste0(r_func_name, " <- function(", 
                        paste(param_names, collapse = ", "), 
                        ", lib = NULL)")
    params_r <- paste0("  # Parameters: ", paste(param_types, param_names, sep = " ", collapse = ", "))
  }
  
  code <- c(
    sprintf("#' Wrapper for %s", func_name),
    sprintf("#' @export"),
    signature,
    "{",
    params_r,
    "  # TODO: Implement FFI call",
    sprintf('  stop("Not yet implemented: %s")', func_name),
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
    ""
  )
  
  # Defines as R constants
  if (length(parsed_header$defines) > 0) {
    code_sections$defines <- c(
      "# Constants from #define",
      ""
    )
    for (name in names(parsed_header$defines)) {
      value <- parsed_header$defines[[name]]
      if (value != "") {
        code_sections$defines <- c(
          code_sections$defines,
          sprintf("%s <- %s", name, value)
        )
      }
    }
    code_sections$defines <- c(code_sections$defines, "")
  }
  
  # Struct definitions
  if (length(parsed_header$structs) > 0) {
    code_sections$structs <- c(
      "# Struct definitions",
      ""
    )
    for (struct_name in names(parsed_header$structs)) {
      struct_def <- parsed_header$structs[[struct_name]]
      struct_code <- generate_struct_definition(struct_name, struct_def)
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
  
  # Write to file if requested
  if (!is.null(output_file)) {
    writeLines(all_code, output_file)
    message("Generated R bindings written to: ", output_file)
  }
  
  all_code
}

#' Get path to embedded TCC binary
#' @return Path to tcc executable in installed package
#' @export
tcc_binary_path <- function() {
    if(.Platform$OS.type == "windows") {
        tcc_path <- system.file("tinycc", "bin", "tcc.exe", package = "RSimpleFFI")
        tcc_path <- normalizePath(tcc_path)
        message("TCC path (Windows): ", tcc_path)
    } else {
        tcc_path <- system.file("tinycc", "bin", "tcc", package = "RSimpleFFI")
        tcc_path <- normalizePath(tcc_path)
        message("TCC path (Unix): ", tcc_path)
    }

    if (!file.exists(tcc_path) || tcc_path == "") {
        stop("TCC binary not found. Package may not be installed correctly.")
    }
  
  tcc_path
}

#' Preprocess C header file using embedded TCC
#' @param header_file Path to C header file
#' @param includes Additional include directories
#' @param keep_defines Keep #define directives (not supported by -E alone)
#' @return Character vector of preprocessed lines
#' @export
tcc_preprocess <- function(header_file, includes = NULL, keep_defines = FALSE) {
  if (!file.exists(header_file)) {
    stop("Header file not found: ", header_file)
  }
  
  tcc <- tcc_binary_path()
  tmp_out <- tempfile(fileext = ".i")
  on.exit(unlink(tmp_out), add = TRUE)
  
  # Build arguments
  args <- c("-E")
  
  if (!is.null(includes)) {
    args <- c(args, paste0("-I", includes))
  }
  
  args <- c(args, header_file, "-o", tmp_out)
  
  # Run TCC preprocessor
  result <- system2(tcc, args, stdout = TRUE, stderr = TRUE)
  
  if (!file.exists(tmp_out)) {
    stop("TCC preprocessing failed:\n", paste(result, collapse = "\n"))
  }
  
  readLines(tmp_out)
}

#' Extract #define macros from C header file or preprocessed lines
#' @param header_file Path to C header file (optional if preprocessed_lines provided)
#' @param preprocessed_lines Character vector from tcc_preprocess() (optional)
#' @return Named list of macro definitions
#' @export
tcc_extract_defines <- function(header_file = NULL, preprocessed_lines = NULL) {
  defines <- list()
  
  # If preprocessed lines provided, extract from preprocessing markers
  if (!is.null(preprocessed_lines)) {
    # TCC preprocessor doesn't preserve #define in output, so we need to parse the original file
    # Extract file paths from line markers like: # 1 "/path/to/file.h"
    file_markers <- preprocessed_lines[grepl("^#\\s+\\d+\\s+\"", preprocessed_lines)]
    file_pattern <- '^#\\s+\\d+\\s+"([^"]+)"'
    
    files_to_scan <- unique(unlist(lapply(file_markers, function(marker) {
      m <- regexec(file_pattern, marker, perl = TRUE)
      parts <- regmatches(marker, m)[[1]]
      if (length(parts) >= 2) parts[2] else NULL
    })))
    
    files_to_scan <- Filter(function(f) !is.null(f) && file.exists(f), files_to_scan)
    
    # Read and extract defines from all included files
    for (f in files_to_scan) {
      lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) character())
      for (line in lines) {
        if (grepl("^\\s*#define\\s+", line) && !grepl("\\(", line)) {
          content <- sub("^\\s*#define\\s+", "", line)
          parts <- strsplit(trimws(content), "\\s+")[[1]]
          if (length(parts) >= 1) {
            name <- parts[1]
            value <- if (length(parts) > 1) paste(parts[-1], collapse = " ") else ""
            defines[[name]] <- value
          }
        }
      }
    }
  }
  
  # Also extract from main header file if provided
  if (!is.null(header_file)) {
    if (!file.exists(header_file)) {
      stop("Header file not found: ", header_file)
    }
    lines <- readLines(header_file)
    for (line in lines) {
      if (grepl("^\\s*#define\\s+", line) && !grepl("\\(", line)) {
        content <- sub("^\\s*#define\\s+", "", line)
        parts <- strsplit(trimws(content), "\\s+")[[1]]
        if (length(parts) >= 1) {
          name <- parts[1]
          value <- if (length(parts) > 1) paste(parts[-1], collapse = " ") else ""
          defines[[name]] <- value
        }
      }
    }
  }
  
  defines
}

#' Compile and run C code using embedded TCC
#' @param code C source code as string
#' @param args Arguments to pass to compiled program
#' @return Output from program
#' @export
tcc_run <- function(code, args = character()) {
  tcc <- tcc_binary_path()
  tmp_src <- tempfile(fileext = ".c")
  on.exit(unlink(tmp_src), add = TRUE)
  
  writeLines(code, tmp_src)
  
  # Use -run to compile and execute
  tcc_args <- c("-run", tmp_src)
  if (length(args) > 0) {
    tcc_args <- c(tcc_args, "--", args)
  }
  
  system2(tcc, tcc_args, stdout = TRUE, stderr = TRUE)
}

#' Check if TCC is available
#' @return Logical indicating if TCC is available
#' @export
tcc_available <- function() {
  tryCatch({
    path <- tcc_binary_path()
    file.exists(path)
  }, error = function(e) FALSE)
}

#' Extract function declarations from preprocessed C code
#' @param preprocessed_lines Character vector from tcc_preprocess()
#' @return Data frame with columns: name, return_type, params, full_declaration
#' @export
tcc_extract_functions <- function(preprocessed_lines) {
  # Remove line markers and empty lines
  code_lines <- preprocessed_lines[!grepl("^#", preprocessed_lines)]
  code_lines <- code_lines[nzchar(trimws(code_lines))]
  
  # Join into single text for multi-line function matching
  code <- paste(code_lines, collapse = " ")
  
  # Remove comments
  code <- gsub("/\\*.*?\\*/", " ", code)
  code <- gsub("//.*?($|;)", ";", code)
  
  # Match function declarations (not definitions)
  # Pattern: return_type function_name(params);
  # Simple pattern - can be enhanced
  pattern <- "([a-zA-Z_][a-zA-Z0-9_*\\s]+)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\(([^)]*)\\)\\s*;"
  
  matches <- gregexpr(pattern, code, perl = TRUE)
  match_data <- regmatches(code, matches)[[1]]
  
  if (length(match_data) == 0) {
    return(data.frame(
      name = character(),
      return_type = character(),
      params = character(),
      full_declaration = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  result <- lapply(match_data, function(decl) {
    # Extract components
    m <- regexec(pattern, decl, perl = TRUE)
    parts <- regmatches(decl, m)[[1]]
    
    if (length(parts) >= 4) {
      list(
        name = trimws(parts[3]),
        return_type = trimws(parts[2]),
        params = trimws(parts[4]),
        full_declaration = trimws(decl)
      )
    } else {
      NULL
    }
  })
  
  result <- Filter(Negate(is.null), result)
  
  if (length(result) == 0) {
    return(data.frame(
      name = character(),
      return_type = character(),
      params = character(),
      full_declaration = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  do.call(rbind.data.frame, c(result, stringsAsFactors = FALSE))
}

#' Extract struct definitions from preprocessed C code
#' @param preprocessed_lines Character vector from tcc_preprocess()
#' @return List of struct definitions (name -> list of fields)
#' @export
tcc_extract_structs <- function(preprocessed_lines) {
  # Remove line markers
  code_lines <- preprocessed_lines[!grepl("^#", preprocessed_lines)]
  code <- paste(code_lines, collapse = " ")
  
  # Remove comments
  code <- gsub("/\\*.*?\\*/", " ", code)
  code <- gsub("//.*?($|;)", ";", code)
  
  result <- list()
  
  # Helper to extract balanced braces content
  extract_balanced_braces <- function(text, start_pos) {
    chars <- strsplit(text, "")[[1]]
    if (start_pos > length(chars) || chars[start_pos] != "{") {
      return(NULL)
    }
    
    depth <- 0
    for (i in start_pos:length(chars)) {
      if (chars[i] == "{") depth <- depth + 1
      if (chars[i] == "}") {
        depth <- depth - 1
        if (depth == 0) {
          return(list(
            content = paste(chars[(start_pos+1):(i-1)], collapse = ""),
            end_pos = i
          ))
        }
      }
    }
    NULL
  }
  
  # Find typedef struct { ... } Name; patterns
  typedef_pattern <- "typedef\\s+struct\\s*\\{"
  typedef_matches <- gregexpr(typedef_pattern, code, perl = TRUE)[[1]]
  
  if (typedef_matches[1] != -1) {
    for (match_start in typedef_matches) {
      # Find the opening brace
      brace_pos <- regexpr("\\{", substring(code, match_start))[[1]] + match_start - 1
      
      # Extract balanced content
      balanced <- extract_balanced_braces(code, brace_pos)
      if (!is.null(balanced)) {
        # Find the type name after closing brace
        remainder <- substring(code, balanced$end_pos + 1)
        name_match <- regexpr("^\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*;", remainder, perl = TRUE)
        
        if (name_match != -1) {
          name_text <- regmatches(remainder, name_match)[[1]]
          name <- trimws(gsub(";", "", name_text))
          
          body <- balanced$content
          fields <- parse_fields_with_nested(body)
          if (length(fields) > 0) {
            result[[name]] <- fields
          }
        }
      }
    }
  }
  
  # Find struct Name { ... }; patterns
  struct_pattern <- "struct\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\{"
  struct_matches <- gregexpr(struct_pattern, code, perl = TRUE)[[1]]
  
  if (struct_matches[1] != -1) {
    for (i in seq_along(struct_matches)) {
      match_start <- struct_matches[i]
      match_len <- attr(struct_matches, "match.length")[i]
      
      # Extract struct name
      name_text <- substring(code, match_start, match_start + match_len - 1)
      name_match <- regexpr("struct\\s+([a-zA-Z_][a-zA-Z0-9_]*)", name_text, perl = TRUE)
      name_capture <- regmatches(name_text, name_match)[[1]]
      name <- sub("struct\\s+", "", name_capture)
      
      # Find opening brace
      brace_pos <- regexpr("\\{", substring(code, match_start))[[1]] + match_start - 1
      
      # Extract balanced content
      balanced <- extract_balanced_braces(code, brace_pos)
      if (!is.null(balanced)) {
        # Check if followed by semicolon
        remainder <- substring(code, balanced$end_pos + 1, balanced$end_pos + 10)
        if (grepl("^\\s*;", remainder)) {
          body <- balanced$content
          fields <- parse_fields_with_nested(body)
          if (length(fields) > 0) {
            result[[name]] <- fields
          }
        }
      }
    }
  }
  
  result
}

# Helper function to parse struct fields, handling nested anonymous types
parse_fields_with_nested <- function(body) {
  fields <- list()
  
  # First, handle nested structs/unions by replacing them temporarily
  placeholders <- list()
  counter <- 1
  
  # Replace nested struct { ... } fieldname; patterns
  while (grepl("struct\\s*\\{", body)) {
    match_pos <- regexpr("struct\\s*\\{", body, perl = TRUE)[[1]]
    if (match_pos == -1) break
    
    # Find balanced braces
    brace_start <- regexpr("\\{", substring(body, match_pos))[[1]] + match_pos - 1
    chars <- strsplit(body, "")[[1]]
    depth <- 0
    end_pos <- NA
    
    for (i in brace_start:length(chars)) {
      if (chars[i] == "{") depth <- depth + 1
      if (chars[i] == "}") {
        depth <- depth - 1
        if (depth == 0) {
          end_pos <- i
          break
        }
      }
    }
    
    if (is.na(end_pos)) break
    
    # Get field name after closing brace
    remainder <- substring(body, end_pos + 1)
    name_match <- regexpr("^\\s*([a-zA-Z_][a-zA-Z0-9_\\[\\]]*)\\s*;", remainder, perl = TRUE)
    
    if (name_match != -1) {
      field_name <- trimws(gsub(";", "", regmatches(remainder, name_match)[[1]]))
      placeholder <- paste0("__NESTED_STRUCT_", counter, "__")
      placeholders[[placeholder]] <- list(type = "struct", name = field_name)
      
      # Replace the whole nested definition with placeholder
      before <- substring(body, 1, match_pos - 1)
      after <- substring(body, end_pos + name_match + attr(name_match, "match.length") - 1)
      body <- paste0(before, placeholder, " ", field_name, ";", after)
      counter <- counter + 1
    } else {
      break
    }
  }
  
  # Replace nested union { ... } fieldname; patterns
  while (grepl("union\\s*\\{", body)) {
    match_pos <- regexpr("union\\s*\\{", body, perl = TRUE)[[1]]
    if (match_pos == -1) break
    
    brace_start <- regexpr("\\{", substring(body, match_pos))[[1]] + match_pos - 1
    chars <- strsplit(body, "")[[1]]
    depth <- 0
    end_pos <- NA
    
    for (i in brace_start:length(chars)) {
      if (chars[i] == "{") depth <- depth + 1
      if (chars[i] == "}") {
        depth <- depth - 1
        if (depth == 0) {
          end_pos <- i
          break
        }
      }
    }
    
    if (is.na(end_pos)) break
    
    remainder <- substring(body, end_pos + 1)
    name_match <- regexpr("^\\s*([a-zA-Z_][a-zA-Z0-9_\\[\\]]*)\\s*;", remainder, perl = TRUE)
    
    if (name_match != -1) {
      field_name <- trimws(gsub(";", "", regmatches(remainder, name_match)[[1]]))
      placeholder <- paste0("__NESTED_UNION_", counter, "__")
      placeholders[[placeholder]] <- list(type = "union", name = field_name)
      
      before <- substring(body, 1, match_pos - 1)
      after <- substring(body, end_pos + name_match + attr(name_match, "match.length") - 1)
      body <- paste0(before, placeholder, " ", field_name, ";", after)
      counter <- counter + 1
    } else {
      break
    }
  }
  
  # Now parse simple fields
  field_pattern <- "([a-zA-Z_][a-zA-Z0-9_*\\s]+)\\s+([a-zA-Z_][a-zA-Z0-9_\\[\\]]*)\\s*;"
  field_matches <- gregexpr(field_pattern, body, perl = TRUE)
  field_data <- regmatches(body, field_matches)[[1]]
  
  for (field in field_data) {
    fm <- regexec(field_pattern, field, perl = TRUE)
    fparts <- regmatches(field, fm)[[1]]
    if (length(fparts) >= 3) {
      type_part <- trimws(fparts[2])
      name_part <- trimws(fparts[3])
      
      # Check if this is a placeholder
      if (type_part %in% names(placeholders)) {
        # It's a nested type
        nested_info <- placeholders[[type_part]]
        fields[[length(fields) + 1]] <- list(
          type = nested_info$type,
          name = name_part
        )
      } else {
        fields[[length(fields) + 1]] <- list(
          type = type_part,
          name = name_part
        )
      }
    }
  }
  
  fields
}

#' Extract enum definitions from preprocessed C code
#' @param preprocessed_lines Character vector from tcc_preprocess()
#' @return List of enum definitions (name -> named integer vector of values)
#' @export
tcc_extract_enums <- function(preprocessed_lines) {
  # Remove line markers
  code_lines <- preprocessed_lines[!grepl("^#", preprocessed_lines)]
  code <- paste(code_lines, collapse = " ")
  
  # Remove comments
  code <- gsub("/\\*.*?\\*/", " ", code)
  code <- gsub("//.*?($|;)", ";", code)
  
  result <- list()
  
  # Pattern 1: enum Name { ... };
  pattern1 <- "enum\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\{([^}]*)\\}\\s*;"
  matches1 <- gregexpr(pattern1, code, perl = TRUE)
  match_data1 <- regmatches(code, matches1)[[1]]
  
  # Pattern 2: typedef enum { ... } TypeName;
  pattern2 <- "typedef\\s+enum\\s*\\{([^}]*)\\}\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*;"
  matches2 <- gregexpr(pattern2, code, perl = TRUE)
  match_data2 <- regmatches(code, matches2)[[1]]
  
  # Helper to parse enum values
  parse_enum_values <- function(body) {
    # Split by comma
    entries <- strsplit(body, ",")[[1]]
    values <- list()
    current_value <- 0L
    
    for (entry in entries) {
      entry <- trimws(entry)
      if (nzchar(entry)) {
        # Check if has explicit value: NAME = VALUE
        if (grepl("=", entry)) {
          parts <- strsplit(entry, "=")[[1]]
          name <- trimws(parts[1])
          value_expr <- trimws(parts[2])
          # Try to evaluate simple expressions
          value <- tryCatch({
            as.integer(eval(parse(text = value_expr)))
          }, error = function(e) {
            # If can't evaluate, use current_value
            current_value
          })
          current_value <- value
        } else {
          name <- entry
          value <- current_value
        }
        
        values[[name]] <- as.integer(value)
        current_value <- current_value + 1L
      }
    }
    
    values
  }
  
  # Process pattern 1: enum Name { ... };
  if (length(match_data1) > 0) {
    enums1 <- lapply(match_data1, function(enum_def) {
      m <- regexec(pattern1, enum_def, perl = TRUE)
      parts <- regmatches(enum_def, m)[[1]]
      
      if (length(parts) >= 3) {
        name <- trimws(parts[2])
        body <- trimws(parts[3])
        values <- parse_enum_values(body)
        list(name = name, values = values)
      } else {
        NULL
      }
    })
    
    enums1 <- Filter(Negate(is.null), enums1)
    for (e in enums1) {
      if (length(e$values) > 0) {
        result[[e$name]] <- unlist(e$values)
      }
    }
  }
  
  # Process pattern 2: typedef enum { ... } TypeName;
  if (length(match_data2) > 0) {
    enums2 <- lapply(match_data2, function(enum_def) {
      m <- regexec(pattern2, enum_def, perl = TRUE)
      parts <- regmatches(enum_def, m)[[1]]
      
      if (length(parts) >= 3) {
        body <- trimws(parts[2])
        name <- trimws(parts[3])
        values <- parse_enum_values(body)
        list(name = name, values = values)
      } else {
        NULL
      }
    })
    
    enums2 <- Filter(Negate(is.null), enums2)
    for (e in enums2) {
      if (length(e$values) > 0) {
        result[[e$name]] <- unlist(e$values)
      }
    }
  }
  
  result
}

#' Extract union definitions from preprocessed C code
#' @param preprocessed_lines Character vector from tcc_preprocess()
#' @return List of union definitions (name -> list of fields)
#' @export
tcc_extract_unions <- function(preprocessed_lines) {
  # Remove line markers
  code_lines <- preprocessed_lines[!grepl("^#", preprocessed_lines)]
  code <- paste(code_lines, collapse = " ")
  
  # Remove comments
  code <- gsub("/\\*.*?\\*/", " ", code)
  code <- gsub("//.*?($|;)", ";", code)
  
  result <- list()
  
  # Helper to extract balanced braces content
  extract_balanced_braces <- function(text, start_pos) {
    chars <- strsplit(text, "")[[1]]
    if (start_pos > length(chars) || chars[start_pos] != "{") {
      return(NULL)
    }
    
    depth <- 0
    for (i in start_pos:length(chars)) {
      if (chars[i] == "{") depth <- depth + 1
      if (chars[i] == "}") {
        depth <- depth - 1
        if (depth == 0) {
          return(list(
            content = paste(chars[(start_pos+1):(i-1)], collapse = ""),
            end_pos = i
          ))
        }
      }
    }
    NULL
  }
  
  # Find typedef union { ... } Name; patterns
  typedef_pattern <- "typedef\\s+union\\s*\\{"
  typedef_matches <- gregexpr(typedef_pattern, code, perl = TRUE)[[1]]
  
  if (typedef_matches[1] != -1) {
    for (match_start in typedef_matches) {
      # Find the opening brace
      brace_pos <- regexpr("\\{", substring(code, match_start))[[1]] + match_start - 1
      
      # Extract balanced content
      balanced <- extract_balanced_braces(code, brace_pos)
      if (!is.null(balanced)) {
        # Find the type name after closing brace
        remainder <- substring(code, balanced$end_pos + 1)
        name_match <- regexpr("^\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*;", remainder, perl = TRUE)
        
        if (name_match != -1) {
          name_text <- regmatches(remainder, name_match)[[1]]
          name <- trimws(gsub(";", "", name_text))
          
          body <- balanced$content
          fields <- parse_fields_with_nested(body)
          if (length(fields) > 0) {
            result[[name]] <- fields
          }
        }
      }
    }
  }
  
  # Find union Name { ... }; patterns
  union_pattern <- "union\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\{"
  union_matches <- gregexpr(union_pattern, code, perl = TRUE)[[1]]
  
  if (union_matches[1] != -1) {
    for (i in seq_along(union_matches)) {
      match_start <- union_matches[i]
      match_len <- attr(union_matches, "match.length")[i]
      
      # Extract union name
      name_text <- substring(code, match_start, match_start + match_len - 1)
      name_match <- regexpr("union\\s+([a-zA-Z_][a-zA-Z0-9_]*)", name_text, perl = TRUE)
      name_capture <- regmatches(name_text, name_match)[[1]]
      name <- sub("union\\s+", "", name_capture)
      
      # Find opening brace
      brace_pos <- regexpr("\\{", substring(code, match_start))[[1]] + match_start - 1
      
      # Extract balanced content
      balanced <- extract_balanced_braces(code, brace_pos)
      if (!is.null(balanced)) {
        # Check if followed by semicolon
        remainder <- substring(code, balanced$end_pos + 1, balanced$end_pos + 10)
        if (grepl("^\\s*;", remainder)) {
          body <- balanced$content
          fields <- parse_fields_with_nested(body)
          if (length(fields) > 0) {
            result[[name]] <- fields
          }
        }
      }
    }
  }
  
  result
}

#' Parse C header file and extract all declarations
#' @param header_file Path to C header file
#' @param includes Additional include directories
#' @return List with components: functions, structs, unions, enums, defines
#' @export
tcc_parse_header <- function(header_file, includes = NULL) {
  # Get preprocessed output
  preprocessed <- tcc_preprocess(header_file, includes = includes)
  
  # Extract components
  list(
    functions = tcc_extract_functions(preprocessed),
    structs = tcc_extract_structs(preprocessed),
    unions = tcc_extract_unions(preprocessed),
    enums = tcc_extract_enums(preprocessed),
    defines = tcc_extract_defines(header_file = header_file, preprocessed_lines = preprocessed)
  )
}

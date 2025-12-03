# Helper function to remove C comments (handles multi-line comments)
# Uses (?s) DOTALL mode so . matches newlines
remove_c_comments <- function(code) {
  # Remove multi-line /* */ comments - (?s) makes . match newlines
  code <- gsub("(?s)/\\*.*?\\*/", " ", code, perl = TRUE)
  # Remove single-line // comments
  code <- gsub("//[^\n]*", "", code, perl = TRUE)
  code
}

#' Get path to embedded TCC binary
#' @return Path to tcc executable in installed package
#' @export
tcc_binary_path <- function() {
  if (.Platform$OS.type == "windows") {
    # On Windows, tcc.exe must be in the root of tinycc/ so it can find include/ and lib/ subdirectories
    tcc_path <- system.file("tinycc", "tcc.exe", package = "RSimpleFFI")
    # message("TCC path (Windows): ", tcc_path)
    if (nzchar(tcc_path) && file.exists(tcc_path)) {
      tcc_path <- normalizePath(tcc_path)
    }
  } else {
    tcc_path <- system.file("tinycc", "bin", "tcc", package = "RSimpleFFI")
    # message("TCC path (Unix): ", tcc_path)
    if (nzchar(tcc_path) && file.exists(tcc_path)) {
      tcc_path <- normalizePath(tcc_path)
    }
  }

  if (!nzchar(tcc_path) || !file.exists(tcc_path)) {
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

  # Helper to extract defines from file content (as single string with comments removed)
  extract_defines_from_content <- function(content) {
    result <- list()
    # Remove all C comments first (handles multi-line)
    content <- remove_c_comments(content)
    # Split into lines
    lines <- strsplit(content, "\n")[[1]]
    for (line in lines) {
      # Match simple #defines (no function-like macros with parentheses)
      if (grepl("^\\s*#define\\s+", line) && !grepl("\\(", line)) {
        content_part <- sub("^\\s*#define\\s+", "", line)
        parts <- strsplit(trimws(content_part), "\\s+")[[1]]
        if (length(parts) >= 1) {
          name <- parts[1]
          value <- if (length(parts) > 1) paste(parts[-1], collapse = " ") else ""
          result[[name]] <- trimws(value)
        }
      }
    }
    result
  }

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

    # Read file as single string, remove comments, then extract defines
    for (f in files_to_scan) {
      content <- tryCatch(
        paste(readLines(f, warn = FALSE), collapse = "\n"),
        error = function(e) ""
      )
      if (nzchar(content)) {
        file_defines <- extract_defines_from_content(content)
        defines <- c(defines, file_defines)
      }
    }
  }

  # Also extract from main header file if provided
  if (!is.null(header_file)) {
    if (!file.exists(header_file)) {
      stop("Header file not found: ", header_file)
    }
    content <- paste(readLines(header_file, warn = FALSE), collapse = "\n")
    file_defines <- extract_defines_from_content(content)
    defines <- c(defines, file_defines)
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
  tryCatch(
    {
      path <- tcc_binary_path()
      file.exists(path)
    },
    error = function(e) FALSE
  )
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

  # Remove comments (handles multi-line)
  code <- remove_c_comments(code)

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
#' @return List of struct definitions (name -> list of fields).
#'   Each struct may have a "packed" attribute set to TRUE if __attribute__((packed))
#'   or #pragma pack was detected.
#' @export
tcc_extract_structs <- function(preprocessed_lines) {
  # Track #pragma pack state
  pack_stack <- c()
  current_pack <- 0L # 0 means natural alignment

  # Process lines to track pragma pack state
  code_lines <- character()
  for (line in preprocessed_lines) {
    if (grepl("^#", line) && !grepl("^#pragma", line)) {
      next # Skip line markers but keep pragmas
    }

    # Check for #pragma pack directives
    if (grepl("#pragma\\s+pack\\s*\\(", line)) {
      if (grepl("push", line)) {
        # #pragma pack(push, N) or #pragma pack(push)
        pack_stack <- c(pack_stack, current_pack)
        num_match <- regmatches(line, regexpr("[0-9]+", line))
        if (length(num_match) > 0 && nchar(num_match) > 0) {
          current_pack <- as.integer(num_match)
        }
      } else if (grepl("pop", line)) {
        # #pragma pack(pop)
        if (length(pack_stack) > 0) {
          current_pack <- pack_stack[length(pack_stack)]
          pack_stack <- pack_stack[-length(pack_stack)]
        } else {
          current_pack <- 0L
        }
      } else {
        # #pragma pack(N) or #pragma pack()
        num_match <- regmatches(line, regexpr("[0-9]+", line))
        if (length(num_match) > 0 && nchar(num_match) > 0) {
          current_pack <- as.integer(num_match)
        } else {
          current_pack <- 0L # #pragma pack() resets to default
        }
      }
      next # Don't include pragma in code
    }

    # Mark lines with current pack state
    attr(line, "pack") <- current_pack
    code_lines <- c(code_lines, line)
  }

  code <- paste(code_lines, collapse = " ")

  # Remove comments (handles multi-line)
  code <- remove_c_comments(code)

  result <- list()

  # Helper to check if a struct definition is packed
  is_packed_struct <- function(prefix_text, suffix_text) {
    # Check for __attribute__((packed)) before or after struct
    has_attr_packed <- grepl("__attribute__\\s*\\(\\s*\\(\\s*packed\\s*\\)\\s*\\)", prefix_text) ||
      grepl("__attribute__\\s*\\(\\s*\\(\\s*packed\\s*\\)\\s*\\)", suffix_text)
    # Check for __packed keyword (some compilers)
    has_packed_keyword <- grepl("__packed", prefix_text)
    has_attr_packed || has_packed_keyword
  }

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
            content = paste(chars[(start_pos + 1):(i - 1)], collapse = ""),
            end_pos = i
          ))
        }
      }
    }
    NULL
  }

  # Find typedef struct { ... } Name; patterns (may have __attribute__((packed)))
  # Patterns: typedef struct __attribute__((packed)) { ... } Name;
  #           typedef struct { ... } __attribute__((packed)) Name;
  typedef_pattern <- "typedef\\s+struct\\s*(__attribute__\\s*\\(\\s*\\([^)]*\\)\\s*\\)\\s*)?\\{"
  typedef_matches <- gregexpr(typedef_pattern, code, perl = TRUE)[[1]]

  if (typedef_matches[1] != -1) {
    for (match_start in typedef_matches) {
      # Get text before the struct for packed detection
      prefix_start <- max(1, match_start - 100)
      prefix_text <- substring(code, prefix_start, match_start + 80)

      # Find the opening brace
      brace_pos <- regexpr("\\{", substring(code, match_start))[[1]] + match_start - 1

      # Extract balanced content
      balanced <- extract_balanced_braces(code, brace_pos)
      if (!is.null(balanced)) {
        # Find the type name after closing brace (may have __attribute__ before name)
        remainder <- substring(code, balanced$end_pos + 1)
        # Match: optional __attribute__((packed)) then name then ;
        name_match <- regexpr("^\\s*(__attribute__\\s*\\(\\s*\\([^)]*\\)\\s*\\)\\s*)?([a-zA-Z_][a-zA-Z0-9_]*)\\s*;", remainder, perl = TRUE)

        if (name_match != -1) {
          name_text <- regmatches(remainder, name_match)[[1]]
          # Extract just the name (last identifier before ;)
          name <- sub(".*?([a-zA-Z_][a-zA-Z0-9_]*)\\s*;\\s*$", "\\1", name_text)

          body <- balanced$content
          fields <- parse_fields_with_nested(body)
          if (length(fields) > 0) {
            # Check if packed
            suffix_text <- substring(remainder, 1, nchar(name_text))
            if (is_packed_struct(prefix_text, suffix_text)) {
              attr(fields, "packed") <- TRUE
            }
            result[[name]] <- fields
          }
        }
      }
    }
  }

  # Find struct Name { ... }; patterns (may have __attribute__((packed)))
  # Patterns: struct __attribute__((packed)) Name { ... };
  #           struct Name __attribute__((packed)) { ... };
  struct_pattern <- "struct\\s+(__attribute__\\s*\\(\\s*\\([^)]*\\)\\s*\\)\\s*)?([a-zA-Z_][a-zA-Z0-9_]*)\\s*(__attribute__\\s*\\(\\s*\\([^)]*\\)\\s*\\)\\s*)?\\{"
  struct_matches <- gregexpr(struct_pattern, code, perl = TRUE)[[1]]

  if (struct_matches[1] != -1) {
    for (i in seq_along(struct_matches)) {
      match_start <- struct_matches[i]
      match_len <- attr(struct_matches, "match.length")[i]

      # Extract the full match for packed detection
      full_match <- substring(code, match_start, match_start + match_len - 1)

      # Extract struct name (skip attributes)
      clean_match <- gsub("__attribute__\\s*\\(\\s*\\([^)]*\\)\\s*\\)", "", full_match)
      name_match <- regexpr("struct\\s+([a-zA-Z_][a-zA-Z0-9_]*)", clean_match, perl = TRUE)
      name_capture <- regmatches(clean_match, name_match)[[1]]
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
            # Check if packed
            if (is_packed_struct(full_match, "")) {
              attr(fields, "packed") <- TRUE
            }
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

  # Now parse fields (both regular and bit-fields)
  # Regular field: type name;
  # Bit-field: type name : width;
  # Array field: type name[N];
  field_pattern <- "([a-zA-Z_][a-zA-Z0-9_*\\s]+)\\s+([a-zA-Z_][a-zA-Z0-9_\\[\\]]*)\\s*(?::\\s*[0-9]+)?\\s*;"
  field_matches <- gregexpr(field_pattern, body, perl = TRUE)
  field_data <- regmatches(body, field_matches)[[1]]

  # Check for bit-fields before processing
  bitfield_pattern <- "([a-zA-Z_][a-zA-Z0-9_*\\s]+)\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*:\\s*([0-9]+)\\s*;"
  if (grepl(bitfield_pattern, body, perl = TRUE)) {
    bitfield_matches <- gregexpr(bitfield_pattern, body, perl = TRUE)
    bitfield_data <- regmatches(body, bitfield_matches)[[1]]

    if (length(bitfield_data) > 0) {
      # Extract bit-field information for warning message
      bitfield_info <- character()
      for (bf in bitfield_data) {
        bf_match <- regexec(bitfield_pattern, bf, perl = TRUE)
        bf_parts <- regmatches(bf, bf_match)[[1]]
        if (length(bf_parts) >= 4) {
          field_name <- trimws(bf_parts[3])
          bit_width <- trimws(bf_parts[4])
          bitfield_info <- c(bitfield_info, sprintf("'%s : %s'", field_name, bit_width))
        }
      }

      # Attach bit-field warning as attribute
      attr(fields, "bitfield_warning") <- list(
        has_bitfields = TRUE,
        fields = bitfield_info
      )
    }
  }

  # Extract just the type and name parts (ignoring bit-field width for field list)
  simple_field_pattern <- "([a-zA-Z_][a-zA-Z0-9_*\\s]+)\\s+([a-zA-Z_][a-zA-Z0-9_\\[\\]]*)\\s*(?::\\s*[0-9]+)?\\s*;"

  for (field in field_data) {
    fm <- regexec(simple_field_pattern, field, perl = TRUE)
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

  # Remove comments (handles multi-line)
  code <- remove_c_comments(code)

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
          value <- tryCatch(
            {
              as.integer(eval(parse(text = value_expr)))
            },
            error = function(e) {
              # If can't evaluate, use current_value
              current_value
            }
          )
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

  # Remove comments (handles multi-line)
  code <- remove_c_comments(code)

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
            content = paste(chars[(start_pos + 1):(i - 1)], collapse = ""),
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

#' Extract simple typedef aliases from preprocessed C code
#'
#' Parses typedefs of the form "typedef <base_type> <alias>;" and returns
#' a named list mapping alias names to their underlying types. This excludes
#' struct/union/enum typedefs which are handled separately.
#'
#' @param preprocessed_lines Character vector from tcc_preprocess()
#' @return Named character vector mapping alias -> base_type
#' @export
#' @examples
#' \dontrun{
#' pp <- tcc_preprocess("myheader.h")
#' typedefs <- tcc_extract_typedefs(pp)
#' # typedefs might contain: c(my_int = "int", real_t = "float", ...)
#' }
tcc_extract_typedefs <- function(preprocessed_lines) {
  # Remove line markers
  code_lines <- preprocessed_lines[!grepl("^#", preprocessed_lines)]
  code <- paste(code_lines, collapse = " ")

  result <- character()

  # Pattern for simple typedefs: typedef <base_type> <alias>;
  # Excludes:
  #   - typedef struct/union/enum { ... } name; (handled by tcc_extract_structs etc)
  #   - typedef <type> (*func_ptr)(args); (function pointers - complex)
  #
  # Captures: typedef <type> <name>;
  # where <type> can be: int, unsigned int, const char *, struct Foo, etc.

  # Split code into statements by semicolons
  statements <- strsplit(code, ";")[[1]]

  for (stmt in statements) {
    stmt <- trimws(stmt)

    # Skip if not a typedef
    if (!grepl("^typedef\\b", stmt)) next

    # Skip struct/union/enum typedefs with braces (handled elsewhere)
    if (grepl("\\{", stmt)) next

    # Skip function pointer typedefs (contain parentheses around name)
    if (grepl("\\(\\s*\\*", stmt)) next

    # Parse: typedef <base_type> <alias_name>
    # The alias is the last word (identifier), base_type is everything between "typedef" and alias

    # Remove "typedef " prefix
    after_typedef <- sub("^typedef\\s+", "", stmt)

    # Split into tokens
    tokens <- strsplit(trimws(after_typedef), "\\s+")[[1]]

    if (length(tokens) < 2) next

    # Last token is the alias name (may have * attached for pointer typedefs)
    alias <- tokens[length(tokens)]

    # Handle pointer in alias: "typedef int *intptr" -> alias="*intptr" or alias="intptr"
    alias <- gsub("^\\*+", "", alias)

    # Validate alias is a valid identifier
    if (!grepl("^[a-zA-Z_][a-zA-Z0-9_]*$", alias)) next

    # Base type is everything before the last token
    base_type <- paste(tokens[-length(tokens)], collapse = " ")
    base_type <- trimws(base_type)

    # Clean up: handle "typedef int * foo" -> base="int *"
    # If after_typedef has *, it should be part of base type unless already there
    if (grepl("\\*", after_typedef) && !grepl("\\*", base_type)) {
      # Find how many * are in the original
      ptr_count <- length(gregexpr("\\*", after_typedef)[[1]])
      if (ptr_count > 0) {
        base_type <- paste0(base_type, " ", paste(rep("*", ptr_count), collapse = ""))
        base_type <- gsub("\\s+", " ", base_type) # normalize spaces
      }
    }

    # Skip if base_type is empty or invalid
    if (nchar(base_type) == 0) next

    # Store alias -> base_type
    result[[alias]] <- base_type
  }

  result
}

#' Parse C header file and extract all declarations
#' @param header_file Path to C header file
#' @param includes Additional include directories
#' @return List with components: functions, structs, unions, enums, defines, typedefs
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
    defines = tcc_extract_defines(header_file = header_file, preprocessed_lines = preprocessed),
    typedefs = tcc_extract_typedefs(preprocessed)
  )
}

#' Parse C header using tree-sitter
#'
#' Internal function that performs tree-sitter-based parsing.
#' Use ffi_parse_header() instead which handles dependency checks.
#'
#' @param header_file Path to C header file
#' @param includes Additional include directories (used for TCC preprocessing)
#' @return List with parsed components (file, defines, structs, unions, enums, functions, typedefs)
#' @keywords internal
#' @importFrom treesitter parser parser_parse tree_root_node query query_captures
#' @importFrom treesitter node_text node_parent node_child node_child_count node_type
ffi_parse_header_ts <- function(header_file, includes = NULL) {
  if (!file.exists(header_file)) {
    stop("Header file not found: ", header_file)
  }

  # Preprocess with TCC first (this expands macros!)
  if (!tcc_available()) {
    stop("TinyCC not available. Package may not be installed correctly.")
  }

  preprocessed <- tryCatch(
    tcc_preprocess(header_file, includes = includes),
    error = function(e) {
      message(sprintf(
        "TCC preprocessing failed for %s: %s",
        header_file,
        e$message
      ))
      warning(sprintf(
        "TCC preprocessing failed for %s: %s",
        header_file,
        e$message
      ))
      character(0)
    }
  )
  if (length(preprocessed) == 0) {
    warning(sprintf(
      "TCC preprocessing produced no output for %s. System headers may be missing or not found. Parsed structs may be empty.",
      header_file
    ))
  }
  preprocessed_text <- paste(preprocessed, collapse = "\n")

  # Parse with tree-sitter
  lang <- treesitter.c::language()
  parser <- treesitter::parser(lang)
  tree <- treesitter::parser_parse(parser, preprocessed_text)
  root <- treesitter::tree_root_node(tree)

  # Extract components using tree-sitter AST
  structs <- ts_extract_structs(root, preprocessed_text)
  unions <- ts_extract_unions(root, preprocessed_text)
  enums <- ts_extract_enums(root, preprocessed_text)
  functions <- ts_extract_functions(root, preprocessed_text)
  typedefs <- ts_extract_typedefs(root, preprocessed_text)

  # Still use regex for defines (since they're preprocessor directives)
  defines <- tcc_extract_defines(
    header_file = header_file,
    preprocessed_lines = preprocessed
  )

  # Create structured result
  structure(
    list(
      file = header_file,
      defines = defines,
      structs = structs,
      unions = unions,
      enums = enums,
      functions = functions,
      typedefs = typedefs,
      parser = "treesitter"
    ),
    class = "parsed_header"
  )
}

#' Extract struct definitions from tree-sitter AST
#' @keywords internal
ts_extract_structs <- function(root_node, source_text) {
  structs <- list()

  # Query 1: Named structs - struct Name { ... }
  query_text1 <- "(struct_specifier name: (type_identifier) @name body: (field_declaration_list) @body)"

  # Query 2: Typedef'd anonymous structs - typedef struct { ... } Name;
  query_text2 <- "(type_definition type: (struct_specifier body: (field_declaration_list) @body) declarator: (type_identifier) @name)"

  tryCatch(
    {
      # Extract named structs
      query1 <- treesitter::query(treesitter.c::language(), query_text1)
      captures1 <- treesitter::query_captures(query1, root_node)

      if (length(captures1$name) > 0) {
        i <- 1
        while (i <= length(captures1$name)) {
          if (
            captures1$name[i] == "name" &&
              i < length(captures1$name) &&
              captures1$name[i + 1] == "body"
          ) {
            struct_name <- treesitter::node_text(captures1$node[[i]])
            struct_body <- captures1$node[[i + 1]]

            # Get parent struct_specifier node to check for packed attribute
            parent_node <- treesitter::node_parent(struct_body)
            is_packed <- ts_check_packed_attribute(parent_node, source_text)

            fields <- ts_extract_struct_fields(struct_body, source_text)

            # Set packed attribute if detected
            if (is_packed) {
              attr(fields, "packed") <- TRUE
            }

            # Preserve attributes when adding to list
            field_attrs <- attributes(fields)
            structs[[struct_name]] <- fields
            attributes(structs[[struct_name]]) <- field_attrs

            i <- i + 2
          } else {
            i <- i + 1
          }
        }
      }

      # Extract typedef'd anonymous structs
      query2 <- treesitter::query(treesitter.c::language(), query_text2)
      captures2 <- treesitter::query_captures(query2, root_node)

      if (length(captures2$name) > 0) {
        i <- 1
        while (i <= length(captures2$name)) {
          if (
            captures2$name[i] == "body" &&
              i < length(captures2$name) &&
              captures2$name[i + 1] == "name"
          ) {
            struct_body <- captures2$node[[i]]
            struct_name <- treesitter::node_text(captures2$node[[i + 1]])

            # Get parent struct_specifier node to check for packed attribute
            parent_node <- treesitter::node_parent(struct_body)
            is_packed <- ts_check_packed_attribute(parent_node, source_text)

            fields <- ts_extract_struct_fields(struct_body, source_text)

            # Set packed attribute if detected
            if (is_packed) {
              attr(fields, "packed") <- TRUE
            }

            # Preserve attributes when adding to list
            field_attrs <- attributes(fields)
            structs[[struct_name]] <- fields
            attributes(structs[[struct_name]]) <- field_attrs

            i <- i + 2
          } else {
            i <- i + 1
          }
        }
      }
    },
    error = function(e) {
      message("Warning: tree-sitter struct extraction failed: ", e$message)
    }
  )

  structs
}

#' Check if a struct has packed attribute
#' @keywords internal
ts_check_packed_attribute <- function(struct_node, source_text) {
  if (is.null(struct_node)) {
    return(FALSE)
  }

  # Check all children of the struct_specifier for attribute_specifier nodes
  child_count <- treesitter::node_child_count(struct_node)

  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(struct_node, i)
    child_type <- treesitter::node_type(child)

    if (child_type == "attribute_specifier") {
      # Get the text of the attribute
      attr_text <- treesitter::node_text(child)
      # Check if it contains "packed" (handles both __packed__ and packed)
      if (grepl("packed", attr_text, fixed = TRUE)) {
        return(TRUE)
      }
    }
  }

  FALSE
}

#' Extract fields from a struct body node
#' @keywords internal
ts_extract_struct_fields <- function(body_node, source_text) {
  fields <- list()
  bitfield_info <- character()
  bitfield_info_detailed <- character()

  child_count <- treesitter::node_child_count(body_node)

  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(body_node, i) # 1-indexed

    if (treesitter::node_type(child) == "field_declaration") {
      field_info <- ts_parse_field_declaration(child, source_text)

      if (!is.null(field_info)) {
        # Return format matches regex parser: list of lists with $name and $type
        fields[[length(fields) + 1]] <- list(
          name = field_info$name,
          type = field_info$type
        )

        # Track bitfields for warning attribute
        if (grepl(":\\s*[0-9]+$", field_info$name)) {
          # Determine signedness from the field type text. If 'unsigned' appears
          # in the type we mark as unsigned, otherwise signed.
          is_unsigned <- grepl("unsigned", field_info$type)
          sign_tag <- if (is_unsigned) "unsigned" else "signed"
          width_part <- sub(".*: ", "", field_info$name)
          # Preserve the original simple spec for backward compatibility
          simple_spec <- sprintf("'%s'", field_info$name)
          # Detailed spec includes signedness: 'name : width : signed'
          # Include the base type in detailed spec so generators can resolve
          # allocation unit size (e.g., int8_t, unsigned int) when creating
          # accessors. Format: 'name : width : signed : base_type'.
          detailed_spec <- sprintf("'%s : %s : %s : %s'", field_info$name, width_part, sign_tag, field_info$type)
          bitfield_info <- c(bitfield_info, simple_spec)
          bitfield_info_detailed <- c(bitfield_info_detailed, detailed_spec)
        }
      }
    }
  }

  # Add bitfield warning attribute if any bitfields detected
  if (length(bitfield_info) > 0) {
    warning_attr <- list(
      has_bitfields = TRUE,
      fields = bitfield_info
    )
    if (length(bitfield_info_detailed) > 0) {
      warning_attr$detailed_fields <- bitfield_info_detailed
    }
    attr(fields, "bitfield_warning") <- warning_attr
  }

  fields
}

#' Parse a field declaration node
#' @keywords internal
ts_parse_field_declaration <- function(field_node, source_text) {
  type_node <- NULL
  declarator_node <- NULL
  bitfield_size <- NULL

  child_count <- treesitter::node_child_count(field_node)

  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(field_node, i)
    node_type <- treesitter::node_type(child)

    if (
      node_type %in%
        c(
          "primitive_type",
          "type_identifier",
          "struct_specifier",
          "sized_type_specifier"
        )
    ) {
      type_node <- child
    } else if (
      node_type %in%
        c("field_identifier", "array_declarator", "pointer_declarator")
    ) {
      declarator_node <- child
    } else if (node_type == "bitfield_clause") {
      # Extract bitfield size from bitfield_clause
      bitfield_child_count <- treesitter::node_child_count(child)
      for (j in seq_len(bitfield_child_count)) {
        bitfield_child <- treesitter::node_child(child, j)
        if (treesitter::node_type(bitfield_child) == "number_literal") {
          bitfield_size <- treesitter::node_text(bitfield_child)
          break
        }
      }
    }
  }

  if (is.null(declarator_node)) {
    return(NULL)
  }

  # Get field name and type
  field_name <- ts_get_declarator_name(declarator_node, source_text)

  # Skip anonymous fields (e.g., anonymous bitfields for padding)
  if (is.null(field_name) || nchar(field_name) == 0) {
    return(NULL)
  }

  if (is.null(type_node)) {
    # No type node found - skip this field
    return(NULL)
  }

  # Handle struct_specifier specially - extract just the struct name
  # to avoid including the full struct body in the type string
  base_type <- if (treesitter::node_type(type_node) == "struct_specifier") {
    ts_get_struct_type_name(type_node)
  } else {
    treesitter::node_text(type_node)
  }

  # Handle pointer declarators - include * in type
  if (treesitter::node_type(declarator_node) == "pointer_declarator") {
    # Count asterisks by traversing AST nodes
    ptr_count <- ts_count_pointer_stars(declarator_node)
    base_type <- paste0(base_type, strrep("*", ptr_count))
  }

  # Check if it's an array
  if (treesitter::node_type(declarator_node) == "array_declarator") {
    dimensions <- ts_get_array_dimensions(declarator_node, source_text)
    field_type <- paste0(base_type, dimensions)
  } else {
    field_type <- base_type
  }

  # Add bitfield marker if present
  if (!is.null(bitfield_size)) {
    field_name <- paste0(field_name, " : ", bitfield_size)
  }

  list(name = field_name, type = field_type)
}

#' Get the name from a declarator node
#' @keywords internal
ts_get_declarator_name <- function(declarator_node, source_text) {
  node_type <- treesitter::node_type(declarator_node)

  if (node_type == "field_identifier") {
    # Check for MISSING node (anonymous bitfield)
    if (treesitter::node_is_missing(declarator_node)) {
      return(NULL)
    }
    return(treesitter::node_text(declarator_node))
  }

  # For array_declarator, pointer_declarator, or other complex types, find the field_identifier child
  child_count <- treesitter::node_child_count(declarator_node)

  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(declarator_node, i)
    child_type <- treesitter::node_type(child)

    if (child_type == "field_identifier") {
      return(treesitter::node_text(child))
    }

    # Recursively check nested declarators
    if (child_type %in% c("array_declarator", "pointer_declarator")) {
      result <- ts_get_declarator_name(child, source_text)
      if (!is.null(result)) {
        return(result)
      }
    }
  }

  NULL
}

#' Count asterisks in a pointer_declarator by traversing AST
#'
#' Counts the number of pointer indirection levels by finding all '*' nodes
#' in the pointer_declarator subtree.
#'
#' @param declarator_node A pointer_declarator node
#' @return Integer count of pointer asterisks
#' @keywords internal
ts_count_pointer_stars <- function(declarator_node) {
  count <- 0
  current <- declarator_node
  # Traverse nested pointer_declarator nodes
  while (
    !is.null(current) && treesitter::node_type(current) == "pointer_declarator"
  ) {
    # Each pointer_declarator level has exactly one '*'
    child_count <- treesitter::node_child_count(current)
    for (i in seq_len(child_count)) {
      child <- treesitter::node_child(current, i)
      if (treesitter::node_type(child) == "*") {
        count <- count + 1
      }
    }
    # Move to the inner declarator (could be another pointer_declarator or the final identifier)
    inner <- treesitter::node_child_by_field_name(current, "declarator")
    current <- inner
  }
  count
}

#' Find function_declarator recursively inside pointer_declarator chain
#'
#' For declarations like `int** foo(...)`, the function_declarator is nested
#' inside multiple pointer_declarator nodes.
#'
#' @param node A pointer_declarator node
#' @return The function_declarator node if found, NULL otherwise
#' @keywords internal
ts_find_function_declarator <- function(node) {
  if (is.null(node)) {
    return(NULL)
  }

  child_count <- treesitter::node_child_count(node)
  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(node, i)
    child_type <- treesitter::node_type(child)

    if (child_type == "function_declarator") {
      return(child)
    } else if (child_type == "pointer_declarator") {
      # Recurse into nested pointer_declarator
      result <- ts_find_function_declarator(child)
      if (!is.null(result)) {
        return(result)
      }
    }
  }
  NULL
}

#' Get struct type name from a struct_specifier node
#'
#' Extracts just the struct name (e.g., "struct Foo") from a struct_specifier
#' node, without including the full body definition if present.
#' For anonymous structs, returns "struct" only.
#'
#' @param struct_node A struct_specifier node
#' @return Character string with the struct type name
#' @keywords internal
ts_get_struct_type_name <- function(struct_node) {
  # Look for type_identifier child which contains the struct name
  child_count <- treesitter::node_child_count(struct_node)
  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(struct_node, i)
    if (treesitter::node_type(child) == "type_identifier") {
      return(paste0("struct ", treesitter::node_text(child)))
    }
  }
  # Anonymous struct - no name
  "struct"
}

#' Get array dimensions from an array declarator
#' @keywords internal
ts_get_array_dimensions <- function(declarator_node, source_text) {
  dimensions <- character()
  current_node <- declarator_node

  while (treesitter::node_type(current_node) == "array_declarator") {
    child_count <- treesitter::node_child_count(current_node)

    # Find the size expression (number_literal or identifier)
    for (i in seq_len(child_count)) {
      child <- treesitter::node_child(current_node, i)
      node_type <- treesitter::node_type(child)

      if (node_type %in% c("number_literal", "identifier")) {
        dim_value <- treesitter::node_text(child)
        dimensions <- c(dimensions, sprintf("[%s]", dim_value))
        break
      }
    }

    # Move to nested array declarator
    first_child <- treesitter::node_child(current_node, 1)
    if (treesitter::node_type(first_child) == "array_declarator") {
      current_node <- first_child
    } else {
      break
    }
  }

  # Reverse to get correct order (outermost to innermost)
  paste(rev(dimensions), collapse = "")
}

#' Extract union definitions from tree-sitter AST
#' @keywords internal
ts_extract_unions <- function(root_node, source_text) {
  unions <- list()

  # Query 1: Named unions - union Name { ... }
  query_text1 <- "(union_specifier name: (type_identifier) @name body: (field_declaration_list) @body)"

  # Query 2: Typedef'd anonymous unions - typedef union { ... } Name;
  query_text2 <- "(type_definition type: (union_specifier body: (field_declaration_list) @body) declarator: (type_identifier) @name)"

  tryCatch(
    {
      # Extract named unions
      query1 <- treesitter::query(treesitter.c::language(), query_text1)
      captures1 <- treesitter::query_captures(query1, root_node)

      if (length(captures1$name) > 0) {
        i <- 1
        while (i <= length(captures1$name)) {
          if (
            captures1$name[i] == "name" &&
              i < length(captures1$name) &&
              captures1$name[i + 1] == "body"
          ) {
            union_name <- treesitter::node_text(captures1$node[[i]])
            union_body <- captures1$node[[i + 1]]

            # Get parent union_specifier node to check for packed attribute
            parent_node <- treesitter::node_parent(union_body)
            is_packed <- ts_check_packed_attribute(parent_node, source_text)

            fields <- ts_extract_struct_fields(union_body, source_text)

            # Set packed attribute if detected
            if (is_packed) {
              attr(fields, "packed") <- TRUE
            }

            # Preserve attributes when adding to list
            field_attrs <- attributes(fields)
            unions[[union_name]] <- fields
            attributes(unions[[union_name]]) <- field_attrs

            i <- i + 2
          } else {
            i <- i + 1
          }
        }
      }

      # Extract typedef'd anonymous unions
      query2 <- treesitter::query(treesitter.c::language(), query_text2)
      captures2 <- treesitter::query_captures(query2, root_node)

      if (length(captures2$name) > 0) {
        i <- 1
        while (i <= length(captures2$name)) {
          if (
            captures2$name[i] == "body" &&
              i < length(captures2$name) &&
              captures2$name[i + 1] == "name"
          ) {
            union_body <- captures2$node[[i]]
            union_name <- treesitter::node_text(captures2$node[[i + 1]])

            # Get parent union_specifier node to check for packed attribute
            parent_node <- treesitter::node_parent(union_body)
            is_packed <- ts_check_packed_attribute(parent_node, source_text)

            fields <- ts_extract_struct_fields(union_body, source_text)

            # Set packed attribute if detected
            if (is_packed) {
              attr(fields, "packed") <- TRUE
            }

            # Preserve attributes when adding to list
            field_attrs <- attributes(fields)
            unions[[union_name]] <- fields
            attributes(unions[[union_name]]) <- field_attrs

            i <- i + 2
          } else {
            i <- i + 1
          }
        }
      }
    },
    error = function(e) {
      message("Warning: tree-sitter union extraction failed: ", e$message)
    }
  )

  unions
}

#' Extract enum definitions from tree-sitter AST
#' @keywords internal
ts_extract_enums <- function(root_node, source_text) {
  enums <- list()

  # Query 1: Named enums - enum Name { ... }
  query_text1 <- "(enum_specifier name: (type_identifier) @name body: (enumerator_list) @body)"

  # Query 2: Typedef'd anonymous enums - typedef enum { ... } Name;
  query_text2 <- "(type_definition type: (enum_specifier body: (enumerator_list) @body) declarator: (type_identifier) @name)"

  tryCatch(
    {
      # Extract named enums
      query1 <- treesitter::query(treesitter.c::language(), query_text1)
      captures1 <- treesitter::query_captures(query1, root_node)

      if (length(captures1$name) > 0) {
        i <- 1
        while (i <= length(captures1$name)) {
          if (
            captures1$name[i] == "name" &&
              i < length(captures1$name) &&
              captures1$name[i + 1] == "body"
          ) {
            enum_name <- treesitter::node_text(captures1$node[[i]])
            enum_body <- captures1$node[[i + 1]]

            values <- ts_parse_enum_values(enum_body, source_text)
            enums[[enum_name]] <- values

            i <- i + 2
          } else {
            i <- i + 1
          }
        }
      }

      # Extract typedef'd anonymous enums
      query2 <- treesitter::query(treesitter.c::language(), query_text2)
      captures2 <- treesitter::query_captures(query2, root_node)

      if (length(captures2$name) > 0) {
        i <- 1
        while (i <= length(captures2$name)) {
          if (
            captures2$name[i] == "body" &&
              i < length(captures2$name) &&
              captures2$name[i + 1] == "name"
          ) {
            enum_body <- captures2$node[[i]]
            enum_name <- treesitter::node_text(captures2$node[[i + 1]])

            values <- ts_parse_enum_values(enum_body, source_text)
            enums[[enum_name]] <- values

            i <- i + 2
          } else {
            i <- i + 1
          }
        }
      }
    },
    error = function(e) {
      message("Warning: tree-sitter enum extraction failed: ", e$message)
    }
  )

  enums
}

#' Parse enum values from enumerator_list node
#' @keywords internal
ts_parse_enum_values <- function(body_node, source_text) {
  values <- integer()
  current_value <- 0L

  child_count <- treesitter::node_child_count(body_node)

  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(body_node, i)

    if (treesitter::node_type(child) == "enumerator") {
      enum_info <- ts_parse_enumerator(child, source_text, current_value)

      if (!is.null(enum_info)) {
        values[enum_info$name] <- enum_info$value
        current_value <- enum_info$value + 1L
      }
    }
  }

  values
}

#' Parse a single enumerator node
#' @keywords internal
ts_parse_enumerator <- function(enum_node, source_text, default_value) {
  name <- NULL
  value <- default_value

  child_count <- treesitter::node_child_count(enum_node)

  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(enum_node, i)
    node_type <- treesitter::node_type(child)

    if (node_type == "identifier") {
      name <- treesitter::node_text(child)
    } else if (node_type == "number_literal") {
      value_text <- treesitter::node_text(child)
      value <- tryCatch(
        as.integer(eval(parse(text = value_text))),
        error = function(e) default_value
      )
    }
  }

  if (!is.null(name)) {
    list(name = name, value = as.integer(value))
  } else {
    NULL
  }
}

#' Extract function declarations from tree-sitter AST
#' @keywords internal
ts_extract_functions <- function(root_node, source_text) {
  functions <- list()

  # Query for function declarations (not definitions - those have compound_statement bodies)
  # Note: This matches both direct function_declarator and pointer_declarator (for pointer returns)
  query_text <- "(declaration type: (_) @return_type declarator: (function_declarator) @declarator)"

  tryCatch(
    {
      query <- treesitter::query(treesitter.c::language(), query_text)
      captures <- treesitter::query_captures(query, root_node)

      if (length(captures$name) > 0) {
        i <- 1
        while (i <= length(captures$name)) {
          if (
            captures$name[i] == "return_type" &&
              i < length(captures$name) &&
              captures$name[i + 1] == "declarator"
          ) {
            return_type <- treesitter::node_text(captures$node[[i]])
            declarator <- captures$node[[i + 1]]

            func_info <- ts_parse_function_declarator(
              declarator,
              source_text,
              return_type
            )

            if (!is.null(func_info)) {
              functions[[func_info$name]] <- func_info
            }

            i <- i + 2
          } else {
            i <- i + 1
          }
        }
      }
    },
    error = function(e) {
      message("Warning: tree-sitter function extraction failed: ", e$message)
    }
  )

  # Also handle pointer return types (FILE* foo(...))
  # These have pointer_declarator as the declarator instead of function_declarator
  query_text2 <- "(declaration type: (_) @return_type declarator: (pointer_declarator) @ptr_declarator)"

  tryCatch(
    {
      query2 <- treesitter::query(treesitter.c::language(), query_text2)
      captures2 <- treesitter::query_captures(query2, root_node)

      if (length(captures2$name) > 0) {
        i <- 1
        while (i <= length(captures2$name)) {
          if (
            captures2$name[i] == "return_type" &&
              i < length(captures2$name) &&
              captures2$name[i + 1] == "ptr_declarator"
          ) {
            base_return_type <- treesitter::node_text(captures2$node[[i]])
            ptr_declarator <- captures2$node[[i + 1]]

            # Count all pointer stars using AST traversal (handles nested pointers like **)
            ptr_count <- ts_count_pointer_stars(ptr_declarator)

            # Find function_declarator recursively inside pointer_declarator chain
            func_declarator <- ts_find_function_declarator(ptr_declarator)

            if (!is.null(func_declarator)) {
              # Build full return type with pointers
              return_type <- paste0(base_return_type, strrep("*", ptr_count))

              func_info <- ts_parse_function_declarator(
                func_declarator,
                source_text,
                return_type
              )

              if (!is.null(func_info)) {
                functions[[func_info$name]] <- func_info
              }
            }

            i <- i + 2
          } else {
            i <- i + 1
          }
        }
      }
    },
    error = function(e) {
      message(
        "Warning: tree-sitter pointer function extraction failed: ",
        e$message
      )
    }
  )

  # Convert to data.frame format like regex parser
  if (length(functions) > 0) {
    # Create data.frame with list column for param_list
    df <- data.frame(
      name = sapply(functions, function(f) f$name),
      return_type = sapply(functions, function(f) f$return_type),
      params = sapply(functions, function(f) f$params),
      full_declaration = sapply(functions, function(f) f$full_declaration),
      stringsAsFactors = FALSE
    )
    # Add param_list as a list column
    df$param_list <- lapply(functions, function(f) f$param_list)
    df
  } else {
    data.frame(
      name = character(),
      return_type = character(),
      params = character(),
      full_declaration = character(),
      param_list = list(),
      stringsAsFactors = FALSE
    )
  }
}

#' Parse function declarator node
#' @keywords internal
ts_parse_function_declarator <- function(
  declarator_node,
  source_text,
  return_type
) {
  name <- NULL
  params <- ""
  param_list <- list()

  child_count <- treesitter::node_child_count(declarator_node)

  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(declarator_node, i)
    node_type <- treesitter::node_type(child)

    if (node_type == "identifier") {
      name <- treesitter::node_text(child)
    } else if (node_type == "parameter_list") {
      # Parse parameter list using tree-sitter AST
      param_list <- ts_parse_parameter_list(child, source_text)
      # Also keep raw text for backward compatibility
      params <- treesitter::node_text(child)
      # Remove outer parentheses
      params <- gsub("^\\((.*)\\)$", "\\1", params)
    }
  }

  if (!is.null(name)) {
    full_decl <- paste0(return_type, " ", name, "(", params, ");")
    list(
      name = name,
      return_type = return_type,
      params = params,
      param_list = param_list, # Structured parameter info
      full_declaration = full_decl
    )
  } else {
    NULL
  }
}

#' Parse parameter list node to extract structured parameter information
#' @keywords internal
ts_parse_parameter_list <- function(param_list_node, source_text) {
  params <- list()

  child_count <- treesitter::node_child_count(param_list_node)

  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(param_list_node, i)
    node_type <- treesitter::node_type(child)

    if (node_type == "parameter_declaration") {
      param_info <- ts_parse_parameter_declaration(child, source_text)
      if (!is.null(param_info)) {
        params[[length(params) + 1]] <- param_info
      }
    } else if (node_type == "variadic_parameter") {
      # Handle ... parameter
      params[[length(params) + 1]] <- list(
        type = "...",
        name = "...",
        is_variadic = TRUE
      )
    }
  }

  params
}

#' Parse a single parameter declaration
#' @keywords internal
ts_parse_parameter_declaration <- function(param_node, source_text) {
  type_text <- ""
  param_name <- NULL

  child_count <- treesitter::node_child_count(param_node)

  # Collect all children to build type and find identifier
  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(param_node, i)
    node_type <- treesitter::node_type(child)

    if (node_type == "identifier") {
      # This is the parameter name
      param_name <- treesitter::node_text(child)
    } else if (
      node_type %in%
        c(
          "primitive_type",
          "type_identifier",
          "sized_type_specifier",
          "struct_specifier",
          "union_specifier",
          "enum_specifier"
        )
    ) {
      # Type specifier
      type_text <- paste(type_text, treesitter::node_text(child))
    } else if (node_type == "pointer_declarator") {
      # Handle pointer types
      ptr_info <- ts_parse_pointer_declarator(child, source_text)
      if (!is.null(ptr_info$name)) {
        param_name <- ptr_info$name
      }
      type_text <- paste(type_text, ptr_info$pointer_prefix)
    } else if (node_type == "array_declarator") {
      # Handle array types
      array_info <- ts_parse_array_declarator(child, source_text)
      if (!is.null(array_info$name)) {
        param_name <- array_info$name
      }
      type_text <- paste(type_text, array_info$array_suffix)
    } else if (node_type %in% c("type_qualifier", "storage_class_specifier")) {
      # const, volatile, restrict, etc.
      type_text <- paste(type_text, treesitter::node_text(child))
    }
    # Ignore attribute_specifier nodes - these are __attribute__(...)
  }

  type_text <- trimws(type_text)

  list(
    type = type_text,
    name = param_name,
    is_variadic = FALSE
  )
}

#' Parse pointer declarator to extract pointer prefix and identifier
#' @keywords internal
ts_parse_pointer_declarator <- function(ptr_node, source_text) {
  pointer_prefix <- ""
  param_name <- NULL

  child_count <- treesitter::node_child_count(ptr_node)

  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(ptr_node, i)
    node_type <- treesitter::node_type(child)

    if (node_type == "*") {
      pointer_prefix <- paste0(pointer_prefix, "*")
    } else if (node_type == "identifier") {
      param_name <- treesitter::node_text(child)
    } else if (node_type == "pointer_declarator") {
      # Nested pointers
      nested <- ts_parse_pointer_declarator(child, source_text)
      pointer_prefix <- paste0(pointer_prefix, nested$pointer_prefix)
      if (!is.null(nested$name)) {
        param_name <- nested$name
      }
    } else if (node_type == "array_declarator") {
      # Pointer to array
      array_info <- ts_parse_array_declarator(child, source_text)
      if (!is.null(array_info$name)) {
        param_name <- array_info$name
      }
    }
  }

  list(
    pointer_prefix = pointer_prefix,
    name = param_name
  )
}

#' Parse array declarator to extract array suffix and identifier
#' @keywords internal
ts_parse_array_declarator <- function(array_node, source_text) {
  array_suffix <- ""
  param_name <- NULL

  child_count <- treesitter::node_child_count(array_node)

  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(array_node, i)
    node_type <- treesitter::node_type(child)

    if (node_type == "identifier") {
      param_name <- treesitter::node_text(child)
    } else if (node_type == "[") {
      array_suffix <- paste0(array_suffix, "[")
    } else if (node_type == "]") {
      array_suffix <- paste0(array_suffix, "]")
    } else if (node_type %in% c("number_literal", "identifier")) {
      # Array size
      array_suffix <- paste0(array_suffix, treesitter::node_text(child))
    }
  }

  list(
    array_suffix = array_suffix,
    name = param_name
  )
}

#' Extract typedef declarations from tree-sitter AST
#' @keywords internal
ts_extract_typedefs <- function(root_node, source_text) {
  typedefs <- list()

  # Query for type_definition nodes
  query_text <- "(type_definition type: (_) @type declarator: (_) @declarator)"

  tryCatch(
    {
      query <- treesitter::query(treesitter.c::language(), query_text)
      captures <- treesitter::query_captures(query, root_node)

      if (length(captures$name) > 0) {
        i <- 1
        while (i <= length(captures$name)) {
          if (
            captures$name[i] == "type" &&
              i < length(captures$name) &&
              captures$name[i + 1] == "declarator"
          ) {
            type_node <- captures$node[[i]]
            declarator_node <- captures$node[[i + 1]]

            # Skip typedef'd anonymous structs/unions/enums - these are handled
            # by ts_extract_structs/unions/enums instead
            type_node_type <- treesitter::node_type(type_node)
            if (
              type_node_type %in%
                c("struct_specifier", "union_specifier", "enum_specifier")
            ) {
              # Check if this is an anonymous definition (has a body)
              has_body <- FALSE
              child_count <- treesitter::node_child_count(type_node)
              for (j in seq_len(child_count)) {
                child <- treesitter::node_child(type_node, j)
                child_type <- treesitter::node_type(child)
                if (
                  child_type %in% c("field_declaration_list", "enumerator_list")
                ) {
                  has_body <- TRUE
                  break
                }
              }
              if (has_body) {
                i <- i + 2
                next
              }
            }

            base_type <- treesitter::node_text(type_node)
            typedef_info <- ts_get_typedef_info(declarator_node, source_text)

            if (!is.null(typedef_info$name)) {
              # Include pointer stars in the type
              full_type <- if (typedef_info$ptr_count > 0) {
                paste0(base_type, strrep("*", typedef_info$ptr_count))
              } else {
                base_type
              }
              typedefs[[typedef_info$name]] <- full_type
            }

            i <- i + 2
          } else {
            i <- i + 1
          }
        }
      }
    },
    error = function(e) {
      message("Warning: tree-sitter typedef extraction failed: ", e$message)
    }
  )

  typedefs
}

#' Get typedef name and pointer count from declarator
#' @keywords internal
ts_get_typedef_info <- function(declarator_node, source_text) {
  node_type <- treesitter::node_type(declarator_node)

  # Handle direct type identifier or primitive_type (tree-sitter may classify
  # some typedef names like ssize_t as primitive_type if they're known types)
  if (
    node_type %in%
      c("type_identifier", "primitive_type", "sized_type_specifier")
  ) {
    return(list(name = treesitter::node_text(declarator_node), ptr_count = 0L))
  }

  # For pointer_declarator, count stars and find identifier
  if (node_type == "pointer_declarator") {
    ptr_count <- 0L
    typedef_name <- NULL

    child_count <- treesitter::node_child_count(declarator_node)
    for (i in seq_len(child_count)) {
      child <- treesitter::node_child(declarator_node, i)
      child_type <- treesitter::node_type(child)

      if (child_type == "*") {
        ptr_count <- ptr_count + 1L
      } else if (
        child_type %in%
          c("type_identifier", "primitive_type", "sized_type_specifier")
      ) {
        typedef_name <- treesitter::node_text(child)
      } else if (child_type == "pointer_declarator") {
        # Nested pointer (e.g., void**)
        nested <- ts_get_typedef_info(child, source_text)
        ptr_count <- ptr_count + nested$ptr_count
        if (!is.null(nested$name)) typedef_name <- nested$name
      }
    }

    return(list(name = typedef_name, ptr_count = ptr_count))
  }

  # For other complex types, try to find the identifier
  child_count <- treesitter::node_child_count(declarator_node)

  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(declarator_node, i)
    child_type <- treesitter::node_type(child)

    if (
      child_type %in%
        c("type_identifier", "primitive_type", "sized_type_specifier")
    ) {
      return(list(name = treesitter::node_text(child), ptr_count = 0L))
    }

    # Recursively check
    result <- ts_get_typedef_info(child, source_text)
    if (!is.null(result$name)) {
      return(result)
    }
  }

  list(name = NULL, ptr_count = 0L)
}

#' Get typedef name from declarator (legacy wrapper)
#' @keywords internal
ts_get_typedef_name <- function(declarator_node, source_text) {
  info <- ts_get_typedef_info(declarator_node, source_text)
  info$name
}

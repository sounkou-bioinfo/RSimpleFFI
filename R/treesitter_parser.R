#' Parse C header using tree-sitter
#' 
#' Internal function that performs tree-sitter-based parsing.
#' Use ffi_parse_header() instead which handles dependency checks.
#' 
#' @param header_file Path to C header file
#' @param includes Additional include directories (used for TCC preprocessing)
#' @return List with parsed components (file, defines, structs, unions, enums, functions, typedefs)
#' @keywords internal
ffi_parse_header_ts <- function(header_file, includes = NULL) {
  if (!file.exists(header_file)) {
    stop("Header file not found: ", header_file)
  }
  
  # Preprocess with TCC first (this expands macros!)
  if (!tcc_available()) {
    stop("TinyCC not available. Package may not be installed correctly.")
  }
  
  preprocessed <- tcc_preprocess(header_file, includes = includes)
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
  query_text1 <- '(struct_specifier name: (type_identifier) @name body: (field_declaration_list) @body)'
  
  # Query 2: Typedef'd anonymous structs - typedef struct { ... } Name;
  query_text2 <- '(type_definition type: (struct_specifier body: (field_declaration_list) @body) declarator: (type_identifier) @name)'
  
  tryCatch({
    # Extract named structs
    query1 <- treesitter::query(treesitter.c::language(), query_text1)
    captures1 <- treesitter::query_captures(query1, root_node)
    
    if (length(captures1$name) > 0) {
      i <- 1
      while (i <= length(captures1$name)) {
        if (captures1$name[i] == "name" && i < length(captures1$name) && captures1$name[i+1] == "body") {
          struct_name <- treesitter::node_text(captures1$node[[i]])
          struct_body <- captures1$node[[i+1]]
          
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
        if (captures2$name[i] == "body" && i < length(captures2$name) && captures2$name[i+1] == "name") {
          struct_body <- captures2$node[[i]]
          struct_name <- treesitter::node_text(captures2$node[[i+1]])
          
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
  }, error = function(e) {
    message("Warning: tree-sitter struct extraction failed: ", e$message)
  })
  
  structs
}

#' Check if a struct has packed attribute
#' @keywords internal
ts_check_packed_attribute <- function(struct_node, source_text) {
  if (is.null(struct_node)) return(FALSE)
  
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
  
  child_count <- treesitter::node_child_count(body_node)
  
  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(body_node, i)  # 1-indexed
    
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
          bitfield_info <- c(bitfield_info, sprintf("'%s'", field_info$name))
        }
      }
    }
  }
  
  # Add bitfield warning attribute if any bitfields detected
  if (length(bitfield_info) > 0) {
    attr(fields, "bitfield_warning") <- list(
      has_bitfields = TRUE,
      fields = bitfield_info
    )
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
    
    if (node_type %in% c("primitive_type", "type_identifier", "struct_specifier", "sized_type_specifier")) {
      type_node <- child
    } else if (node_type %in% c("field_identifier", "array_declarator", "pointer_declarator")) {
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
  base_type <- if (!is.null(type_node)) {
    treesitter::node_text(type_node)
  } else {
    "int"  # default
  }
  
  # Handle pointer declarators - include * in type
  if (treesitter::node_type(declarator_node) == "pointer_declarator") {
    # Count asterisks and add to type
    decl_text <- treesitter::node_text(declarator_node)
    ptr_count <- nchar(gsub("[^*]", "", decl_text))
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
      if (!is.null(result)) return(result)
    }
  }
  
  NULL
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
  query_text1 <- '(union_specifier name: (type_identifier) @name body: (field_declaration_list) @body)'
  
  # Query 2: Typedef'd anonymous unions - typedef union { ... } Name;
  query_text2 <- '(type_definition type: (union_specifier body: (field_declaration_list) @body) declarator: (type_identifier) @name)'
  
  tryCatch({
    # Extract named unions
    query1 <- treesitter::query(treesitter.c::language(), query_text1)
    captures1 <- treesitter::query_captures(query1, root_node)
    
    if (length(captures1$name) > 0) {
      i <- 1
      while (i <= length(captures1$name)) {
        if (captures1$name[i] == "name" && i < length(captures1$name) && captures1$name[i+1] == "body") {
          union_name <- treesitter::node_text(captures1$node[[i]])
          union_body <- captures1$node[[i+1]]
          
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
        if (captures2$name[i] == "body" && i < length(captures2$name) && captures2$name[i+1] == "name") {
          union_body <- captures2$node[[i]]
          union_name <- treesitter::node_text(captures2$node[[i+1]])
          
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
  }, error = function(e) {
    message("Warning: tree-sitter union extraction failed: ", e$message)
  })
  
  unions
}

#' Extract enum definitions from tree-sitter AST
#' @keywords internal
ts_extract_enums <- function(root_node, source_text) {
  enums <- list()
  
  # Query 1: Named enums - enum Name { ... }
  query_text1 <- '(enum_specifier name: (type_identifier) @name body: (enumerator_list) @body)'
  
  # Query 2: Typedef'd anonymous enums - typedef enum { ... } Name;
  query_text2 <- '(type_definition type: (enum_specifier body: (enumerator_list) @body) declarator: (type_identifier) @name)'
  
  tryCatch({
    # Extract named enums
    query1 <- treesitter::query(treesitter.c::language(), query_text1)
    captures1 <- treesitter::query_captures(query1, root_node)
    
    if (length(captures1$name) > 0) {
      i <- 1
      while (i <= length(captures1$name)) {
        if (captures1$name[i] == "name" && i < length(captures1$name) && captures1$name[i+1] == "body") {
          enum_name <- treesitter::node_text(captures1$node[[i]])
          enum_body <- captures1$node[[i+1]]
          
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
        if (captures2$name[i] == "body" && i < length(captures2$name) && captures2$name[i+1] == "name") {
          enum_body <- captures2$node[[i]]
          enum_name <- treesitter::node_text(captures2$node[[i+1]])
          
          values <- ts_parse_enum_values(enum_body, source_text)
          enums[[enum_name]] <- values
          
          i <- i + 2
        } else {
          i <- i + 1
        }
      }
    }
  }, error = function(e) {
    message("Warning: tree-sitter enum extraction failed: ", e$message)
  })
  
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
  query_text <- '(declaration type: (_) @return_type declarator: (function_declarator) @declarator)'
  
  tryCatch({
    query <- treesitter::query(treesitter.c::language(), query_text)
    captures <- treesitter::query_captures(query, root_node)
    
    if (length(captures$name) > 0) {
      i <- 1
      while (i <= length(captures$name)) {
        if (captures$name[i] == "return_type" && i < length(captures$name) && captures$name[i+1] == "declarator") {
          return_type <- treesitter::node_text(captures$node[[i]])
          declarator <- captures$node[[i+1]]
          
          func_info <- ts_parse_function_declarator(declarator, source_text, return_type)
          
          if (!is.null(func_info)) {
            functions[[func_info$name]] <- func_info
          }
          
          i <- i + 2
        } else {
          i <- i + 1
        }
      }
    }
  }, error = function(e) {
    message("Warning: tree-sitter function extraction failed: ", e$message)
  })
  
  # Also handle pointer return types (FILE* foo(...))
  # These have pointer_declarator as the declarator instead of function_declarator
  query_text2 <- '(declaration type: (_) @return_type declarator: (pointer_declarator) @ptr_declarator)'
  
  tryCatch({
    query2 <- treesitter::query(treesitter.c::language(), query_text2)
    captures2 <- treesitter::query_captures(query2, root_node)
    
    if (length(captures2$name) > 0) {
      i <- 1
      while (i <= length(captures2$name)) {
        if (captures2$name[i] == "return_type" && i < length(captures2$name) && captures2$name[i+1] == "ptr_declarator") {
          base_return_type <- treesitter::node_text(captures2$node[[i]])
          ptr_declarator <- captures2$node[[i+1]]
          
          # Find function_declarator inside pointer_declarator
          func_declarator <- NULL
          child_count <- treesitter::node_child_count(ptr_declarator)
          ptr_count <- 0
          
          for (j in seq_len(child_count)) {
            child <- treesitter::node_child(ptr_declarator, j)
            node_type <- treesitter::node_type(child)
            
            if (node_type == "*") {
              ptr_count <- ptr_count + 1
            } else if (node_type == "function_declarator") {
              func_declarator <- child
              break
            }
          }
          
          if (!is.null(func_declarator)) {
            # Build full return type with pointers
            return_type <- paste0(base_return_type, strrep("*", ptr_count))
            
            func_info <- ts_parse_function_declarator(func_declarator, source_text, return_type)
            
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
  }, error = function(e) {
    message("Warning: tree-sitter pointer function extraction failed: ", e$message)
  })
  
  # Convert to data.frame format like regex parser
  if (length(functions) > 0) {
    do.call(rbind.data.frame, c(lapply(functions, function(f) {
      data.frame(
        name = f$name,
        return_type = f$return_type,
        params = f$params,
        full_declaration = f$full_declaration,
        stringsAsFactors = FALSE
      )
    }), stringsAsFactors = FALSE))
  } else {
    data.frame(
      name = character(),
      return_type = character(),
      params = character(),
      full_declaration = character(),
      stringsAsFactors = FALSE
    )
  }
}

#' Parse function declarator node
#' @keywords internal
ts_parse_function_declarator <- function(declarator_node, source_text, return_type) {
  name <- NULL
  params <- ""
  
  child_count <- treesitter::node_child_count(declarator_node)
  
  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(declarator_node, i)
    node_type <- treesitter::node_type(child)
    
    if (node_type == "identifier") {
      name <- treesitter::node_text(child)
    } else if (node_type == "parameter_list") {
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
      full_declaration = full_decl
    )
  } else {
    NULL
  }
}

#' Extract typedef declarations from tree-sitter AST
#' @keywords internal
ts_extract_typedefs <- function(root_node, source_text) {
  typedefs <- list()
  
  # Query for type_definition nodes
  query_text <- '(type_definition type: (_) @type declarator: (_) @declarator)'
  
  tryCatch({
    query <- treesitter::query(treesitter.c::language(), query_text)
    captures <- treesitter::query_captures(query, root_node)
    
    if (length(captures$name) > 0) {
      i <- 1
      while (i <= length(captures$name)) {
        if (captures$name[i] == "type" && i < length(captures$name) && captures$name[i+1] == "declarator") {
          type_node <- captures$node[[i]]
          declarator_node <- captures$node[[i+1]]
          
          base_type <- treesitter::node_text(type_node)
          typedef_name <- ts_get_typedef_name(declarator_node, source_text)
          
          if (!is.null(typedef_name)) {
            typedefs[[typedef_name]] <- base_type
          }
          
          i <- i + 2
        } else {
          i <- i + 1
        }
      }
    }
  }, error = function(e) {
    message("Warning: tree-sitter typedef extraction failed: ", e$message)
  })
  
  typedefs
}

#' Get typedef name from declarator
#' @keywords internal
ts_get_typedef_name <- function(declarator_node, source_text) {
  node_type <- treesitter::node_type(declarator_node)
  
  if (node_type == "type_identifier") {
    return(treesitter::node_text(declarator_node))
  }
  
  # For pointer_declarator or other complex types, find the identifier
  child_count <- treesitter::node_child_count(declarator_node)
  
  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(declarator_node, i)
    
    if (treesitter::node_type(child) == "type_identifier") {
      return(treesitter::node_text(child))
    }
    
    # Recursively check
    result <- ts_get_typedef_name(child, source_text)
    if (!is.null(result)) return(result)
  }
  
  NULL
}

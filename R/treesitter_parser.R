#' Parse C header using tree-sitter (if available)
#' @param header_file Path to C header file
#' @param includes Additional include directories (used for TCC preprocessing)
#' @param use_treesitter If TRUE, use tree-sitter parser. If FALSE or if treesitter unavailable, fall back to regex.
#' @return List with parsed components (file, defines, structs, unions, enums, functions, typedefs)
#' @export
ffi_parse_header_ts <- function(header_file, includes = NULL, use_treesitter = TRUE) {
  if (!file.exists(header_file)) {
    stop("Header file not found: ", header_file)
  }
  
  # Check if tree-sitter is available
  ts_available <- use_treesitter && 
    requireNamespace("treesitter", quietly = TRUE) && 
    requireNamespace("treesitter.c", quietly = TRUE)
  
  if (!ts_available) {
    message("Tree-sitter not available, falling back to regex parser")
    return(ffi_parse_header(header_file, includes))
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
  
  # Query for struct declarations
  query_text <- '(struct_specifier name: (type_identifier) @name body: (field_declaration_list) @body)'
  
  tryCatch({
    query <- treesitter::query(treesitter.c::language(), query_text)
    captures <- treesitter::query_captures(query, root_node)
    
    # query_captures returns list(name = char vector, node = list of nodes)
    # Process captures in pairs (name, body)
    if (length(captures$name) > 0) {
      i <- 1
      while (i <= length(captures$name)) {
        if (captures$name[i] == "name" && i < length(captures$name) && captures$name[i+1] == "body") {
          struct_name <- treesitter::node_text(captures$node[[i]])
          struct_body <- captures$node[[i+1]]
          
          fields <- ts_extract_struct_fields(struct_body, source_text)
          structs[[struct_name]] <- fields
          
          i <- i + 2  # Skip both name and body
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

#' Extract fields from a struct body node
#' @keywords internal
ts_extract_struct_fields <- function(body_node, source_text) {
  fields <- list()
  
  child_count <- treesitter::node_child_count(body_node)
  
  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(body_node, i)  # 1-indexed
    
    if (treesitter::node_type(child) == "field_declaration") {
      field_info <- ts_parse_field_declaration(child, source_text)
      
      if (!is.null(field_info)) {
        fields[[field_info$name]] <- field_info$type
      }
    }
  }
  
  fields
}

#' Parse a field declaration node
#' @keywords internal
ts_parse_field_declaration <- function(field_node, source_text) {
  type_node <- NULL
  declarator_node <- NULL
  
  child_count <- treesitter::node_child_count(field_node)
  
  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(field_node, i)
    node_type <- treesitter::node_type(child)
    
    if (node_type %in% c("primitive_type", "type_identifier", "struct_specifier")) {
      type_node <- child
    } else if (node_type == "field_identifier") {
      declarator_node <- child
    } else if (node_type == "array_declarator") {
      declarator_node <- child
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
  
  # Check if it's an array
  if (treesitter::node_type(declarator_node) == "array_declarator") {
    dimensions <- ts_get_array_dimensions(declarator_node, source_text)
    field_type <- paste0(base_type, dimensions)
  } else {
    field_type <- base_type
  }
  
  list(name = field_name, type = field_type)
}

#' Get the name from a declarator node
#' @keywords internal
ts_get_declarator_name <- function(declarator_node, source_text) {
  if (treesitter::node_type(declarator_node) == "field_identifier") {
    return(treesitter::node_text(declarator_node))
  }
  
  # For array declarators, find the field_identifier child
  child_count <- treesitter::node_child_count(declarator_node)
  
  for (i in seq_len(child_count)) {
    child <- treesitter::node_child(declarator_node, i)
    
    if (treesitter::node_type(child) == "field_identifier") {
      return(treesitter::node_text(child))
    }
    
    # Recursively check nested declarators
    if (treesitter::node_type(child) == "array_declarator") {
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
  # Similar to structs but query for union_specifier
  list()  # Placeholder
}

#' Extract enum definitions from tree-sitter AST
#' @keywords internal
ts_extract_enums <- function(root_node, source_text) {
  # Query for enum_specifier
  list()  # Placeholder
}

#' Extract function declarations from tree-sitter AST
#' @keywords internal
ts_extract_functions <- function(root_node, source_text) {
  # Query for function_declarator
  list()  # Placeholder
}

#' Extract typedef declarations from tree-sitter AST
#' @keywords internal
ts_extract_typedefs <- function(root_node, source_text) {
  # Query for type_definition
  list()  # Placeholder
}

# Helper Functions for Common FFI Patterns

#' Create and initialize a struct from R list
#' 
#' @param struct_type StructType object
#' @param values Named list of field values
#' @return External pointer to allocated and initialized struct
#' @export
#' @examples
#' \dontrun{
#' Point <- ffi_struct(x = ffi_int(), y = ffi_int())
#' pt <- ffi_struct_from_list(Point, list(x = 10L, y = 20L))
#' }
ffi_struct_from_list <- function(struct_type, values) {
  if (!S7::S7_inherits(struct_type, StructType)) {
    stop("struct_type must be a StructType")
  }
  
  if (!is.list(values)) {
    stop("values must be a named list")
  }
  
  # Allocate struct
  ptr <- ffi_alloc(struct_type)
  
  # Set fields
  for (field_name in names(values)) {
    if (!field_name %in% struct_type@fields) {
      warning("Unknown field '", field_name, "' - skipping")
      next
    }
    ffi_set_field(ptr, field_name, values[[field_name]], struct_type)
  }
  
  ptr
}

#' Convert struct to R list
#' 
#' @param ptr External pointer to struct
#' @param struct_type StructType object
#' @return Named list of field values
#' @export
#' @examples
#' \dontrun{
#' Point <- ffi_struct(x = ffi_int(), y = ffi_int())
#' pt <- ffi_alloc(Point)
#' ffi_set_field(pt, "x", 42L, Point)
#' ffi_set_field(pt, "y", 100L, Point)
#' as.list(pt, Point)  # list(x = 42L, y = 100L)
#' }
ffi_struct_to_list <- function(ptr, struct_type) {
  if (!S7::S7_inherits(struct_type, StructType)) {
    stop("struct_type must be a StructType")
  }
  
  result <- list()
  for (field_name in struct_type@fields) {
    result[[field_name]] <- ffi_get_field(ptr, field_name, struct_type)
  }
  
  result
}

#' Allocate array of structs from R list
#' 
#' @param struct_type StructType object
#' @param values List of named lists, one per struct
#' @return External pointer to allocated struct array
#' @export
#' @examples
#' \dontrun{
#' Point <- ffi_struct(x = ffi_int(), y = ffi_int())
#' points <- ffi_struct_array_from_list(Point, list(
#'   list(x = 0L, y = 0L),
#'   list(x = 10L, y = 20L),
#'   list(x = 30L, y = 40L)
#' ))
#' }
ffi_struct_array_from_list <- function(struct_type, values) {
  if (!S7::S7_inherits(struct_type, StructType)) {
    stop("struct_type must be a StructType")
  }
  
  if (!is.list(values)) {
    stop("values must be a list of lists")
  }
  
  n <- length(values)
  if (n == 0) {
    stop("values must contain at least one element")
  }
  
  # Allocate array
  ptr <- ffi_alloc(struct_type, n)
  
  # Set each struct
  for (i in seq_along(values)) {
    elem_ptr <- ffi_get_element(ptr, i, struct_type)
    for (field_name in names(values[[i]])) {
      if (field_name %in% struct_type@fields) {
        ffi_set_field(elem_ptr, field_name, values[[i]][[field_name]], struct_type)
      }
    }
  }
  
  ptr
}

#' Check if external pointer is NULL
#' 
#' @param ptr External pointer
#' @return Logical
#' @export
ffi_is_null <- function(ptr) {
  .Call("R_is_null_pointer", ptr)
}

#' Create a NULL pointer
#' 
#' @return External pointer to NULL
#' @export
ffi_null_pointer <- function() {
  # Return an external pointer to NULL
  .Call("R_alloc_buffer", 0L)  # This returns NULL pointer
}

#' Pretty print struct contents
#' 
#' @param ptr External pointer to struct
#' @param struct_type StructType object
#' @export
ffi_print_struct <- function(ptr, struct_type) {
  if (!S7::S7_inherits(struct_type, StructType)) {
    stop("struct_type must be a StructType")
  }
  
  cat("Struct (", struct_type@name, "):\n", sep = "")
  
  for (i in seq_along(struct_type@fields)) {
    field_name <- struct_type@fields[i]
    field_type <- struct_type@field_types[[i]]
    value <- ffi_get_field(ptr, field_name, struct_type)
    
    cat("  ", field_name, " (", field_type@name, "): ", sep = "")
    
    # Format value based on type
    if (is.null(value)) {
      cat("NULL\n")
    } else if (length(value) == 1) {
      cat(value, "\n")
    } else {
      cat("[", paste(head(value, 5), collapse = ", "), 
          if(length(value) > 5) "..." else "", "]\n", sep = "")
    }
  }
  
  invisible(ptr)
}

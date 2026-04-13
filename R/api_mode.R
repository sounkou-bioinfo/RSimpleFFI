#' Generic Field Accessor Functions for API Mode
#'
#' These functions provide type-aware field access for structs with bitfields,
#' using compiler-computed offsets and the existing FFI type system.
#'
#' @name api_mode
#' @keywords internal
NULL

# Re-use ffi_set_field generic from 00-types.R

#' @export
S7::method(ffi_set_field, list(S7::class_any, S7::class_any, S7::class_any, S7::class_list)) <- function(ptr, field, value, struct_type) {
  # New API mode: (ptr, field, value, struct_type_list)
  
  if (!inherits(ptr, "externalptr")) {
    stop("ptr must be an external pointer")
  }
  
  if (!is.list(struct_type) || !("fields" %in% names(struct_type))) {
    stop("struct_type must contain 'fields' element")
  }
  
  field_info <- struct_type$fields[[field]]
  if (is.null(field_info)) {
    stop("Unknown field: ", field)
  }
  
  offset <- as.numeric(field_info$offset)
  field_type <- field_info$type
  
  # Extract external pointer ref from S7 FFIType object
  if (S7::S7_inherits(field_type, RSimpleFFI::FFIType)) {
    field_type_ptr <- field_type@ref
  } else if (inherits(field_type, "externalptr")) {
    field_type_ptr <- field_type
  } else {
    stop("field_type must be an FFIType or external pointer")
  }
  
  .Call("R_struct_set_field", ptr, offset, field_type_ptr, value, PACKAGE = "RSimpleFFI")
  invisible(NULL)
}

#' @export
S7::method(ffi_set_field, list(S7::class_any, S7::class_any, S7::class_any, StructType)) <- function(ptr, field, value, struct_type) {
  # Old StructType API: (ptr, field, value, StructType)
  
  if (!inherits(ptr, "externalptr")) {
    stop("ptr must be an external pointer")
  }
  
  # Handle both character field names and integer indices
  if (is.character(field)) {
    field_idx <- which(struct_type@fields == field)
    if (length(field_idx) == 0) {
      if (S7::S7_inherits(struct_type, UnionType)) {
        stop("No such field '", field, "' in union")
      } else {
        stop("No such field '", field, "' in struct")
      }
    }
  } else {
    field_idx <- field
    # Validate integer index
    if (field_idx < 1 || field_idx > length(struct_type@fields)) {
      stop("Field index out of range")
    }
  }
  
  offset <- ffi_offsetof(struct_type, field_idx, use_pack = TRUE)
  field_type <- struct_type@field_types[[field_idx]]
  
  # Extract external pointer ref from S7 FFIType object
  if (S7::S7_inherits(field_type, RSimpleFFI::FFIType)) {
    field_type_ptr <- field_type@ref
  } else {
    stop("field_type must be an FFIType")
  }
  
  .Call("R_struct_set_field", ptr, offset, field_type_ptr, value, PACKAGE = "RSimpleFFI")
  invisible(NULL)
}

# Re-use ffi_get_field generic from 00-types.R

#' @export
S7::method(ffi_get_field, list(S7::class_any, S7::class_character, S7::class_list)) <- function(ptr, field, struct_type) {
  # New API mode: (ptr, field, struct_type_list)
  if (!inherits(ptr, "externalptr")) {
    stop("ptr must be an external pointer")
  }
  
  if (!is.list(struct_type) || !("fields" %in% names(struct_type))) {
    stop("struct_type must contain 'fields' element")
  }
  
  field_info <- struct_type$fields[[field]]
  if (is.null(field_info)) {
    stop("Unknown field: ", field)
  }
  
  offset <- as.numeric(field_info$offset)
  field_type <- field_info$type
  
  # Extract external pointer ref from S7 FFIType object
  if (S7::S7_inherits(field_type, RSimpleFFI::FFIType)) {
    field_type_ptr <- field_type@ref
  } else if (inherits(field_type, "externalptr")) {
    field_type_ptr <- field_type
  } else {
    stop("field_type must be an FFIType or external pointer")
  }
  
  # Get pointer to field (tagged with type)
  field_ptr <- .Call("R_struct_get_field_ptr", ptr, offset, field_type_ptr, PACKAGE = "RSimpleFFI")
  
  # Convert to R value
  .Call("R_field_to_r", field_ptr, PACKAGE = "RSimpleFFI")
}

#' @export
S7::method(ffi_get_field, list(S7::class_any, S7::class_any, StructType)) <- function(ptr, field, struct_type) {
  # Old StructType API: (ptr, field, StructType)
  if (!inherits(ptr, "externalptr")) {
    stop("ptr must be an external pointer")
  }
  
  # Handle both character field names and integer indices
  if (is.character(field)) {
    field_idx <- which(struct_type@fields == field)
    if (length(field_idx) == 0) {
      if (S7::S7_inherits(struct_type, UnionType)) {
        stop("No such field '", field, "' in union")
      } else {
        stop("No such field '", field, "' in struct")
      }
    }
  } else {
    field_idx <- field
    # Validate integer index
    if (field_idx < 1 || field_idx > length(struct_type@fields)) {
      stop("Field index out of range")
    }
  }
  
  offset <- ffi_offsetof(struct_type, field_idx, use_pack = TRUE)
  field_type <- struct_type@field_types[[field_idx]]
  
  # Extract external pointer ref from S7 FFIType object
  if (S7::S7_inherits(field_type, RSimpleFFI::FFIType)) {
    field_type_ptr <- field_type@ref
  } else {
    stop("field_type must be an FFIType")
  }
  
  # Get pointer to field (tagged with type)
  field_ptr <- .Call("R_struct_get_field_ptr", ptr, offset, field_type_ptr, PACKAGE = "RSimpleFFI")
  
  # Convert to R value
  .Call("R_field_to_r", field_ptr, PACKAGE = "RSimpleFFI")
}

#' @export
S7::method(ffi_set_field, list(S7::class_any, S7::class_any, S7::class_any, UnionType)) <- function(ptr, field, value, struct_type) {
  # Old UnionType API: (ptr, field, value, UnionType)
  
  if (!inherits(ptr, "externalptr")) {
    stop("ptr must be an external pointer")
  }
  
  # Handle both character field names and integer indices
  if (is.character(field)) {
    field_idx <- which(struct_type@fields == field)
    if (length(field_idx) == 0) {
      stop("No such field '", field, "' in union")
    }
  } else {
    field_idx <- field
    # Validate integer index
    if (field_idx < 1 || field_idx > length(struct_type@fields)) {
      stop("Field index out of range")
    }
  }
  
  # For unions, offset is always 0 (all fields share the same memory)
  offset <- 0
  field_type <- struct_type@field_types[[field_idx]]
  
  # Extract external pointer ref from S7 FFIType object
  if (S7::S7_inherits(field_type, RSimpleFFI::FFIType)) {
    field_type_ptr <- field_type@ref
  } else {
    stop("field_type must be an FFIType")
  }
  
  .Call("R_struct_set_field", ptr, offset, field_type_ptr, value, PACKAGE = "RSimpleFFI")
  invisible(NULL)
}

#' @export
S7::method(ffi_get_field, list(S7::class_any, S7::class_any, UnionType)) <- function(ptr, field, struct_type) {
  # Old UnionType API: (ptr, field, UnionType)
  if (!inherits(ptr, "externalptr")) {
    stop("ptr must be an external pointer")
  }
  
  # Handle both character field names and integer indices
  if (is.character(field)) {
    field_idx <- which(struct_type@fields == field)
    if (length(field_idx) == 0) {
      stop("No such field '", field, "' in union")
    }
  } else {
    field_idx <- field
    # Validate integer index
    if (field_idx < 1 || field_idx > length(struct_type@fields)) {
      stop("Field index out of range")
    }
  }
  
  # For unions, offset is always 0 (all fields share the same memory)
  offset <- 0
  field_type <- struct_type@field_types[[field_idx]]
  
  # Extract external pointer ref from S7 FFIType object
  if (S7::S7_inherits(field_type, RSimpleFFI::FFIType)) {
    field_type_ptr <- field_type@ref
  } else {
    stop("field_type must be an FFIType")
  }
  
  # Get pointer to field (tagged with type)
  field_ptr <- .Call("R_struct_get_field_ptr", ptr, offset, field_type_ptr, PACKAGE = "RSimpleFFI")
  
  # Convert to R value
  .Call("R_field_to_r", field_ptr, PACKAGE = "RSimpleFFI")
}

#' Get field pointer from struct (internal helper)
#'
#' Returns an external pointer to a field within a struct, tagged with the
#' field's FFI type information for later type-aware operations.
#'
#' @param struct_ptr External pointer to the struct
#' @param field_name Name of the field to access
#' @param struct_type Struct type metadata (contains field offsets and types)
#' @return External pointer to the field, tagged with FFI type
#' @keywords internal
ffi_get_field_ptr <- function(struct_ptr, field_name, struct_type) {
  if (!inherits(struct_ptr, "externalptr")) {
    stop("struct_ptr must be an external pointer")
  }
  
  if (!is.list(struct_type) || !("fields" %in% names(struct_type))) {
    stop("struct_type must contain 'fields' element")
  }
  
  field_info <- struct_type$fields[[field_name]]
  if (is.null(field_info)) {
    stop("Unknown field: ", field_name)
  }
  
  offset <- as.numeric(field_info$offset)
  field_type <- field_info$type
  
  # Extract external pointer ref from S7 FFIType object
  if (S7::S7_inherits(field_type, RSimpleFFI::FFIType)) {
    field_type_ptr <- field_type@ref
  } else if (inherits(field_type, "externalptr")) {
    field_type_ptr <- field_type
  } else {
    stop("field_type must be an FFIType or external pointer")
  }
  
  .Call("R_struct_get_field_ptr", struct_ptr, offset, field_type_ptr, PACKAGE = "RSimpleFFI")
}

#' Convert field pointer to R value
#'
#' Converts a tagged field pointer to an R value using the type information
#' stored in the pointer's tag.
#'
#' @param field_ptr External pointer to field (must be tagged with FFI type)
#' @return Field value converted to appropriate R type
#' @export
#' @keywords internal
ffi_field_to_r <- function(field_ptr) {
  if (!inherits(field_ptr, "externalptr")) {
    stop("field_ptr must be an external pointer")
  }
  
  .Call("R_field_to_r", field_ptr, PACKAGE = "RSimpleFFI")
}

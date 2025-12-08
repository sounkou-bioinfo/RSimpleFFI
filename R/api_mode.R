#' Generic Field Accessor Functions for API Mode
#'
#' These functions provide type-aware field access for structs with bitfields,
#' using compiler-computed offsets and the existing FFI type system.
#'
#' @name api_mode
#' @keywords internal
NULL

#' Set field value in struct
#'
#' S7 generic for setting struct field values. Supports both:
#' - New API mode: ffi_set_field(ptr, field, struct_type_list, value)
#' - Old StructType: ffi_set_field(ptr, field, value, StructType)
#'
#' @param struct_ptr External pointer to the struct
#' @param field_name Name of the field to set (character or integer index)
#' @param arg3 Third argument (struct_type or value)
#' @param arg4 Fourth argument (value or struct_type)
#' @return NULL (invisible)
#' @export
ffi_set_field <- S7::new_generic("ffi_set_field", c("struct_ptr", "field_name", "arg3", "arg4"))

#' @export
S7::method(ffi_set_field, list(S7::class_any, S7::class_character, S7::class_list, S7::class_any)) <- function(struct_ptr, field_name, arg3, arg4) {
  # New API mode: (ptr, field_name, struct_type_list, value)
  struct_type <- arg3
  value <- arg4
  
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
  
  .Call("R_struct_set_field", struct_ptr, offset, field_type_ptr, value, PACKAGE = "RSimpleFFI")
  invisible(NULL)
}

#' @export
S7::method(ffi_set_field, list(S7::class_any, S7::class_any, S7::class_any, StructType)) <- function(struct_ptr, field_name, arg3, arg4) {
  # Old StructType API: (ptr, field_name, value, StructType)
  value <- arg3
  struct_type <- arg4
  
  if (!inherits(struct_ptr, "externalptr")) {
    stop("struct_ptr must be an external pointer")
  }
  
  # Handle both character field names and integer indices
  if (is.character(field_name)) {
    field_idx <- which(struct_type@fields == field_name)
    if (length(field_idx) == 0) {
      if (S7::S7_inherits(struct_type, UnionType)) {
        stop("No such field '", field_name, "' in union")
      } else {
        stop("No such field '", field_name, "' in struct")
      }
    }
  } else {
    field_idx <- field_name
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
  
  .Call("R_struct_set_field", struct_ptr, offset, field_type_ptr, value, PACKAGE = "RSimpleFFI")
  invisible(NULL)
}

#' Get field value from struct
#'
#' S7 generic for getting struct field values. Supports both:
#' - New API mode: ffi_get_field(ptr, field, struct_type_list)
#' - Old StructType: ffi_get_field(ptr, field, StructType)
#'
#' @param struct_ptr External pointer to the struct
#' @param field_name Name of the field to get (character or integer index)
#' @param struct_type Struct type information (list or StructType)
#' @return The field value
#' @export
ffi_get_field <- S7::new_generic("ffi_get_field", c("struct_ptr", "field_name", "struct_type"))

#' @export
S7::method(ffi_get_field, list(S7::class_any, S7::class_character, S7::class_list)) <- function(struct_ptr, field_name, struct_type) {
  # New API mode: (ptr, field_name, struct_type_list)
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
  
  # Get pointer to field (tagged with type)
  field_ptr <- .Call("R_struct_get_field_ptr", struct_ptr, offset, field_type_ptr, PACKAGE = "RSimpleFFI")
  
  # Convert to R value
  .Call("R_field_to_r", field_ptr, PACKAGE = "RSimpleFFI")
}

#' @export
S7::method(ffi_get_field, list(S7::class_any, S7::class_any, StructType)) <- function(struct_ptr, field_name, struct_type) {
  # Old StructType API: (ptr, field_name, StructType)
  if (!inherits(struct_ptr, "externalptr")) {
    stop("struct_ptr must be an external pointer")
  }
  
  # Handle both character field names and integer indices
  if (is.character(field_name)) {
    field_idx <- which(struct_type@fields == field_name)
    if (length(field_idx) == 0) {
      if (S7::S7_inherits(struct_type, UnionType)) {
        stop("No such field '", field_name, "' in union")
      } else {
        stop("No such field '", field_name, "' in struct")
      }
    }
  } else {
    field_idx <- field_name
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
  field_ptr <- .Call("R_struct_get_field_ptr", struct_ptr, offset, field_type_ptr, PACKAGE = "RSimpleFFI")
  
  # Convert to R value
  .Call("R_field_to_r", field_ptr, PACKAGE = "RSimpleFFI")
}

#' @export
S7::method(ffi_set_field, list(S7::class_any, S7::class_any, S7::class_any, UnionType)) <- function(struct_ptr, field_name, arg3, arg4) {
  # Old UnionType API: (ptr, field_name, value, UnionType)
  value <- arg3
  struct_type <- arg4  # Use struct_type to match generic signature
  
  if (!inherits(struct_ptr, "externalptr")) {
    stop("struct_ptr must be an external pointer")
  }
  
  # Handle both character field names and integer indices
  if (is.character(field_name)) {
    field_idx <- which(struct_type@fields == field_name)
    if (length(field_idx) == 0) {
      stop("No such field '", field_name, "' in union")
    }
  } else {
    field_idx <- field_name
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
  
  .Call("R_struct_set_field", struct_ptr, offset, field_type_ptr, value, PACKAGE = "RSimpleFFI")
  invisible(NULL)
}

#' @export
S7::method(ffi_get_field, list(S7::class_any, S7::class_any, UnionType)) <- function(struct_ptr, field_name, struct_type) {
  # Old UnionType API: (ptr, field_name, UnionType)
  if (!inherits(struct_ptr, "externalptr")) {
    stop("struct_ptr must be an external pointer")
  }
  
  # Handle both character field names and integer indices
  if (is.character(field_name)) {
    field_idx <- which(struct_type@fields == field_name)
    if (length(field_idx) == 0) {
      stop("No such field '", field_name, "' in union")
    }
  } else {
    field_idx <- field_name
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
  field_ptr <- .Call("R_struct_get_field_ptr", struct_ptr, offset, field_type_ptr, PACKAGE = "RSimpleFFI")
  
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

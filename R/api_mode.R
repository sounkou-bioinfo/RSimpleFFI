#' Generic Field Accessor Functions for API Mode
#'
#' These functions provide type-aware field access for structs with bitfields,
#' using compiler-computed offsets and the existing FFI type system.
#'
#' @name api_mode
#' @keywords internal
NULL

#' Get field pointer from struct
#'
#' Returns an external pointer to a field within a struct, tagged with the
#' field's FFI type information for later type-aware operations.
#'
#' @param struct_ptr External pointer to the struct
#' @param field_name Name of the field to access
#' @param struct_type Struct type metadata (contains field offsets and types)
#' @return External pointer to the field, tagged with FFI type
#' @export
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
  
  .Call("R_struct_get_field_ptr", struct_ptr, offset, field_type, PACKAGE = "RSimpleFFI")
}

#' Set field value in struct
#'
#' Sets a field value using type-aware writing. Works with bitfields, regular
#' fields, pointers, nested structs, etc.
#'
#' @param struct_ptr External pointer to the struct
#' @param field_name Name of the field to set
#' @param struct_type Struct type metadata (contains field offsets and types)
#' @param value Value to set
#' @return NULL (invisible)
#' @export
#' @keywords internal
ffi_set_field <- function(struct_ptr, field_name, struct_type, value) {
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
  
  .Call("R_struct_set_field", struct_ptr, offset, field_type, value, PACKAGE = "RSimpleFFI")
  invisible(NULL)
}

#' Get field value from struct
#'
#' Reads a field value using type-aware conversion. Works with bitfields, regular
#' fields, pointers, nested structs, etc.
#'
#' @param struct_ptr External pointer to the struct
#' @param field_name Name of the field to get
#' @param struct_type Struct type metadata (contains field offsets and types)
#' @return Field value converted to appropriate R type
#' @export
#' @keywords internal
ffi_get_field <- function(struct_ptr, field_name, struct_type) {
  # Get field pointer (tagged with type)
  field_ptr <- ffi_get_field_ptr(struct_ptr, field_name, struct_type)
  
  # Convert to R value
  ffi_field_to_r(field_ptr)
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

# RSimpleFFI Structure Support

#' @import S7
NULL

#' Allocate memory for FFI structure or array
#' @param type StructType or ArrayType object
#' @param n (optional) number of elements for array allocation
#' @export
ffi_alloc <- S7::new_generic("ffi_alloc", "type")

S7::method(ffi_alloc, StructType) <- function(type, n = 1L) {
  if (n != 1L) {
    stop("StructType allocation only supports n = 1")
  }
  .Call("R_alloc_struct", type@ref)
}

S7::method(ffi_alloc, ArrayType) <- function(type, n = 1L) {
  .Call("R_alloc_typed_buffer", type@ref, as.integer(type@length * n))
}

#' Get field value from FFI structure
#' @param ptr External pointer to structure
#' @param field Character field name or integer field index
#' @param struct_type StructType object
#' @export
ffi_get_field <- S7::new_generic(
  "ffi_get_field",
  c("ptr", "field", "struct_type")
)

#' @export
S7::method(
  ffi_get_field,
  list(S7::class_any, S7::class_character, StructType)
) <- function(ptr, field, struct_type) {
  field_index <- match(field, struct_type@fields)
  if (is.na(field_index)) {
    stop(
      "No such field '",
      field,
      "' in struct. Available fields: ",
      paste(struct_type@fields, collapse = ", ")
    )
  }

  .Call("R_get_struct_field", ptr, as.integer(field_index - 1), struct_type@ref)
}

#' @export
S7::method(
  ffi_get_field,
  list(S7::class_any, S7::class_integer, StructType)
) <- function(ptr, field, struct_type) {
  if (field < 1 || field > length(struct_type@fields)) {
    stop(
      "Field index out of range: ",
      field,
      ". Struct has ",
      length(struct_type@fields),
      " fields."
    )
  }

  .Call("R_get_struct_field", ptr, as.integer(field - 1), struct_type@ref)
}

#' Set field value in FFI structure
#' @param ptr External pointer to structure
#' @param field Character field name or integer field index
#' @param value Value to set
#' @param struct_type StructType object
#' @export
ffi_set_field <- S7::new_generic(
  "ffi_set_field",
  c("ptr", "field", "value", "struct_type")
)

#' @export
S7::method(
  ffi_set_field,
  list(S7::class_any, S7::class_character, S7::class_any, StructType)
) <- function(ptr, field, value, struct_type) {
  field_index <- match(field, struct_type@fields)
  if (is.na(field_index)) {
    stop(
      "No such field '",
      field,
      "' in struct. Available fields: ",
      paste(struct_type@fields, collapse = ", ")
    )
  }

  .Call(
    "R_set_struct_field",
    ptr,
    as.integer(field_index - 1),
    value,
    struct_type@ref
  )
  invisible(ptr)
}

#' @export
S7::method(
  ffi_set_field,
  list(S7::class_any, S7::class_integer, S7::class_any, StructType)
) <- function(ptr, field, value, struct_type) {
  if (field < 1 || field > length(struct_type@fields)) {
    stop(
      "Field index out of range: ",
      field,
      ". Struct has ",
      length(struct_type@fields),
      " fields."
    )
  }

  .Call(
    "R_set_struct_field",
    ptr,
    as.integer(field - 1),
    value,
    struct_type@ref
  )
  invisible(ptr)
}

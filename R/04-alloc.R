#' @name ffi_alloc
#' @title Allocate a buffer for a given FFI type
#' @description Allocates a buffer for n elements of the given FFIType (e.g., int, double, etc). Returns an external pointer tagged with the type.
#' @param type FFIType object
#' @param ... Additional arguments
#' @return External pointer to buffer
#' @rdname alloc
#' @export
ffi_alloc <- S7::new_generic("ffi_alloc", "type")

#' @export
S7::method(ffi_alloc, FFIType) <- function(type, n = 1L) {
  if (!S7::S7_inherits(type, FFIType)) {
    stop("type must be an FFIType object")
  }
  if (length(n) != 1 || !is.numeric(n) || n < 1) {
    stop("n must be a positive integer")
  }
  .Call("R_alloc_typed_buffer", type@ref, as.integer(n))
}

#' @export
S7::method(ffi_alloc, StructType) <- function(type, n = 1L) {
  if (n != 1L) {
    stop("StructType allocation only supports n = 1")
  }
  .Call("R_alloc_struct", type@ref)
}

#' @export
S7::method(ffi_alloc, ArrayType) <- function(type, n = 1L) {
  .Call("R_alloc_typed_buffer", type@ref, as.integer(type@length * n))
}

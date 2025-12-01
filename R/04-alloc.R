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

#' Free memory pointed to by an external pointer
#'
#' Explicitly frees memory that was allocated by C code and returned as a pointer.
#' Use this when you know the pointer was allocated with malloc/calloc and it's
#' your responsibility to free it.
#'
#' @param ptr External pointer to free
#' @return NULL invisibly
#'
#' @details
#' **When to use this:**
#' - Pointers returned from C functions that allocate memory (e.g., `strdup`, `malloc`)
#' - When C documentation says "caller must free"
#'
#' **When NOT to use this:**
#' - Pointers allocated via `ffi_alloc()` (auto-freed by R's GC)
#' - Static or global pointers from C
#' - Pointers into existing structures
#' - Pointers that C will free itself
#'
#' Calling `ffi_free()` on an already-freed pointer or invalid pointer
#' will cause undefined behavior (likely crash).
#'
#' @examples
#' \dontrun{
#' # C function that allocates and returns a string
#' strdup_fn <- ffi_function("strdup", ffi_pointer(), ffi_string())
#' ptr <- strdup_fn("hello")
#' # ... use ptr ...
#' ffi_free(ptr) # We must free because strdup allocates
#' }
#'
#' @export
ffi_free <- function(ptr) {
  if (!inherits(ptr, "externalptr")) {
    stop("ptr must be an external pointer")
  }
  .Call("R_ffi_free", ptr)
  invisible(NULL)
}

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
  if (n == 1L) {
    .Call("R_alloc_struct", type@ref)
  } else {
    if (!is.numeric(n) || n < 1) {
      stop("n must be a positive integer")
    }
    .Call("R_alloc_struct_array", type@ref, as.integer(n))
  }
}

#' @export
S7::method(ffi_alloc, UnionType) <- function(type, n = 1L) {
  if (!S7::S7_inherits(type, UnionType)) {
    stop("type must be a UnionType object")
  }
  if (length(n) != 1 || !is.numeric(n) || n < 1) {
    stop("n must be a positive integer")
  }
  .Call("R_alloc_typed_buffer", type@ref, as.integer(n))
}

#' @export
S7::method(ffi_alloc, EnumType) <- function(type, n = 1L) {
  if (!S7::S7_inherits(type, EnumType)) {
    stop("type must be an EnumType object")
  }
  if (length(n) != 1 || !is.numeric(n) || n < 1) {
    stop("n must be a positive integer")
  }
  # Allocate based on underlying type
  .Call("R_alloc_typed_buffer", type@underlying_type@ref, as.integer(n))
}



#' @export
S7::method(
  ffi_alloc,
  S7::class_list
) <- function(type, n = 1L) {
  print(names(type))
  if (!is.null(type[["is_bitfield"]])) {
    total_bits <- sum(type[["field_widths"]])
    if (total_bits <= 8) {
      ffi_type <- ffi_uint8()
    } else if (total_bits <= 16) {
      ffi_type <- ffi_uint16()
    } else if (total_bits <= 32) {
      ffi_type <- ffi_uint32()
    } else if (total_bits <= 64) {
      ffi_type <- ffi_uint64()
    } else {
      stop("Bitfield total width exceeds 64 bits; cannot allocate as a single integer type.")
    }
    if (n > 1L) {
      warning("Bitfield accessor allocation is typically for a single packed value; n > 1 is rarely useful.")
    }
    return(ffi_alloc(ffi_type, n))
  }
  print("-----")
  stop(sprintf("Unsupported type %s for ffi_alloc", typeof(type)))
}

#' Get element from struct array
#'
#' Returns a pointer to the i-th struct in a contiguous array of structs.
#' The returned pointer shares memory with the original array.
#'
#' @param ptr External pointer to struct array (from ffi_alloc with n > 1)
#' @param index 1-based index of element to get
#' @param struct_type StructType describing the element type
#' @return External pointer to the element (no finalizer - parent owns memory)
#'
#' @examples
#' \dontrun{
#' Point <- ffi_struct(x = ffi_int(), y = ffi_int())
#' points <- ffi_alloc(Point, 10L) # array of 10 Points
#' p3 <- ffi_get_element(points, 3L, Point)
#' ffi_set_field(p3, "x", 100L, Point)
#' }
#'
#' @export
ffi_get_element <- function(ptr, index, struct_type) {
  if (!inherits(ptr, "externalptr")) {
    stop("ptr must be an external pointer")
  }
  if (!S7::S7_inherits(struct_type, StructType)) {
    stop("struct_type must be a StructType object")
  }
  if (!is.numeric(index) || length(index) != 1 || index < 1) {
    stop("index must be a positive integer (1-based)")
  }
  # Convert 1-based R index to 0-based C index
  .Call(
    "R_get_struct_array_element",
    ptr,
    as.integer(index - 1L),
    struct_type@ref
  )
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

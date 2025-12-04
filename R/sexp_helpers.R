#' @name sexp_helpers
#' @title SEXP Pointer Helpers for Safe FFI Usage
#' @description
#' Functions for safely extracting pointers from R objects while ensuring
#' the R object remains protected from garbage collection.
#'
#' @details
#' When passing R objects to C functions via FFI, we need to:
#' 1. Extract the underlying pointer (SEXP or data pointer)
#' 2. Prevent R from garbage collecting the object while we use the pointer
#' 3. Release the protection when we're done
#'
#' These helpers use R's `R_PreserveObject`/`R_ReleaseObject` mechanism
#' to prevent GC. The returned external pointer includes a finalizer that
#' automatically releases the protection when the pointer is no longer used.
NULL

#' Get SEXP pointer from R object with GC protection
#'
#' Returns an external pointer to the SEXP (R object header) while ensuring
#' the object won't be garbage collected as long as the pointer exists.
#'
#' @param x Any R object
#' @return External pointer to the SEXP, with finalizer to release protection
#' @export
#' @examples
#' \dontrun{
#' x <- c(1L, 2L, 3L) # Use c() not 1:3 to avoid ALTREP
#' ptr <- sexp_ptr(x)
#' # ptr is now safe to pass to C functions expecting SEXP
#' # When ptr is garbage collected, the protection is released
#' }
sexp_ptr <- function(x) {
  .Call("R_sexp_ptr", x)
}

#' Get data pointer from R vector with GC protection
#'
#' Returns an external pointer to the underlying data (INTEGER, REAL, etc.)
#' while ensuring the object won't be garbage collected.
#'
#' Note: For ALTREP objects (like `1:10`), use `data_ptr_ro()` instead,
#' which properly handles deferred materialization.
#'
#' @param x An R vector (integer, double, complex, character, raw, or list)
#' @return External pointer to the data, with finalizer to release protection
#' @export
#' @examples
#' \dontrun{
#' x <- c(1.0, 2.0, 3.0) # Regular vector, not ALTREP
#' ptr <- data_ptr(x)
#' # ptr points to the double* array
#' # Safe to pass to C functions expecting double*
#' }
data_ptr <- function(x) {
  .Call("R_data_ptr", x)
}

#' Get read-only data pointer from R vector with GC protection
#'
#' Like `data_ptr()` but attempts read-only access first (for ALTREP support).
#' If the ALTREP implementation doesn't provide direct access, falls back to
#' materializing the data (which may allocate memory).
#'
#' @param x An R vector
#' @return External pointer to the data
#' @export
data_ptr_ro <- function(x) {
  .Call("R_data_ptr_ro", x)
}

#' Check if an object is a protected SEXP pointer
#'
#' @param ptr An external pointer
#' @return TRUE if ptr was created by sexp_ptr() or data_ptr()
#' @export
is_protected_ptr <- function(ptr) {
  if (!inherits(ptr, "externalptr")) {
    return(FALSE)
  }
  .Call("R_is_protected_ptr", ptr)
}

#' Manually release a protected pointer
#'
#' Normally not needed - the finalizer handles this automatically.
#' Use only if you need to release protection early.
#'
#' @param ptr A protected pointer from sexp_ptr() or data_ptr()
#' @return NULL invisibly
#' @export
release_ptr <- function(ptr) {
  .Call("R_release_protected_ptr", ptr)
  invisible(NULL)
}

#' Get the R object from a protected SEXP pointer
#'
#' Retrieves the original R object from a pointer created by sexp_ptr().
#'
#' @param ptr A protected SEXP pointer
#' @return The original R object
#' @export
ptr_to_sexp <- function(ptr) {
  .Call("R_ptr_to_sexp", ptr)
}

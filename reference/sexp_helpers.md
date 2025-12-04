# SEXP Pointer Helpers for Safe FFI Usage

Functions for safely extracting pointers from R objects while ensuring
the R object remains protected from garbage collection.

## Details

When passing R objects to C functions via FFI, we need to:

1.  Extract the underlying pointer (SEXP or data pointer)

2.  Prevent R from garbage collecting the object while we use the
    pointer

3.  Release the protection when we're done

These helpers use R's `R_PreserveObject`/`R_ReleaseObject` mechanism to
prevent GC. The returned external pointer includes a finalizer that
automatically releases the protection when the pointer is no longer
used.

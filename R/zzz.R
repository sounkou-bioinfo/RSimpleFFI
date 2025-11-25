# Package loading and initialization

#' @import S7
#' @useDynLib RSimpleFFI, .registration=TRUE
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

.onLoad <- function(libname, pkgname) {
  # Register S7 methods
  S7::methods_register()
}
# Package loading and initialization

#' @keywords internal
#' @import S7
#' @importFrom stats setNames
#' @importFrom utils head
"_PACKAGE"
#' @useDynLib RSimpleFFI, .registration=TRUE
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

.onLoad <- function(libname, pkgname) {
  # Register S7 methods
  S7::methods_register()
}

# Minimal testthat-style compatibility layer for the existing RSimpleFFI tests.
# Tests are executed by tinytest; this file only provides names that were used
# by the old testthat suite.
suppressPackageStartupMessages(library(tinytest))
suppressPackageStartupMessages(library(RSimpleFFI))

# testthat runs package tests with the package namespace on the search path,
# which made internal helpers visible to the old suite. Recreate that behavior
# for tinytest so the migration does not change the tested surface by accident.
.rsf_ns <- asNamespace("RSimpleFFI")
for (.nm in ls(.rsf_ns, all.names = TRUE)) {
  if (!exists(.nm, envir = environment(), inherits = FALSE)) {
    assign(.nm, get(.nm, envir = .rsf_ns), envir = environment())
  }
}
rm(.rsf_ns, .nm)

test_that <- function(desc, code) {
  force(code)
}

.tt_expect_true <- expect_true
expect_true <- function(object, ..., info = NULL, label = NULL) {
  .tt_expect_true(object, info = info %||% label)
}

.tt_expect_false <- expect_false
expect_false <- function(object, ..., info = NULL, label = NULL) {
  .tt_expect_false(object, info = info %||% label)
}

skip_if <- function(condition, message = NULL) {
  if (isTRUE(condition)) {
    tinytest::exit_file(message %||% "skipped")
  }
  invisible(NULL)
}

skip_if_not <- function(condition, message = NULL) {
  if (!isTRUE(condition)) {
    tinytest::exit_file(message %||% "skipped")
  }
  invisible(NULL)
}

skip_if_not_installed <- function(pkg, minimum_version = NULL) {
  ok <- requireNamespace(pkg, quietly = TRUE)
  if (ok && !is.null(minimum_version)) {
    ok <- utils::packageVersion(pkg) >= minimum_version
  }
  if (!ok) {
    tinytest::exit_file(sprintf("package %s is not installed", pkg))
  }
  invisible(NULL)
}

skip_on_cran <- function() {
  if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    tinytest::exit_file("skipped on CRAN")
  }
  invisible(NULL)
}

expect_no_error <- function(object, regexp = NA, ..., info = NULL, label = NULL) {
  expect_true_fn <- get("expect_true", envir = parent.frame(), inherits = TRUE)
  expr <- substitute(object)
  env <- parent.frame()
  err <- tryCatch({
    eval(expr, envir = env)
    NULL
  }, error = identity)
  if (is.null(err)) {
    expect_true_fn(TRUE, info = info %||% label %||% "no error")
  } else {
    expect_true_fn(
      FALSE,
      info = paste0(info %||% label %||% "unexpected error", ": ", conditionMessage(err))
    )
  }
  invisible(NULL)
}

expect_type <- function(object, type, ..., info = NULL, label = NULL) {
  expect_equal_fn <- get("expect_equal", envir = parent.frame(), inherits = TRUE)
  expect_equal_fn(typeof(object), type, info = info %||% label)
}

expect_s3_class <- function(object, class, ..., info = NULL, label = NULL) {
  expect_true_fn <- get("expect_true", envir = parent.frame(), inherits = TRUE)
  classes <- class(object)
  ok <- inherits(object, class) || class %in% classes
  if (!ok && requireNamespace("S7", quietly = TRUE)) {
    s7_cls <- tryCatch(S7::S7_class(object), error = function(e) NULL)
    if (!is.null(s7_cls)) {
      cls_name <- tryCatch(s7_cls@name, error = function(e) NULL)
      cls_pkg <- tryCatch(s7_cls@package, error = function(e) NULL)
      ok <- identical(class, cls_name) || identical(class, paste0(cls_pkg, "::", cls_name))
    }
  }
  expect_true_fn(ok, info = info %||% label %||% paste("expected class", class))
}

expect_s7_class <- function(object, class, ..., info = NULL, label = NULL) {
  expect_true_fn <- get("expect_true", envir = parent.frame(), inherits = TRUE)
  ok <- tryCatch(S7::S7_inherits(object, class), error = function(e) FALSE)
  expect_true_fn(ok, info = info %||% label %||% "expected S7 class")
}

expect_named <- function(object, expected, ..., ignore.order = FALSE, info = NULL, label = NULL) {
  expect_equal_fn <- get("expect_equal", envir = parent.frame(), inherits = TRUE)
  nm <- names(object)
  if (isTRUE(ignore.order)) {
    expect_equal_fn(sort(nm), sort(expected), info = info %||% label)
  } else {
    expect_equal_fn(nm, expected, info = info %||% label)
  }
}

expect_gt <- function(object, expected, ..., info = NULL, label = NULL) {
  expect_true_fn <- get("expect_true", envir = parent.frame(), inherits = TRUE)
  expect_true_fn(object > expected, info = info %||% label)
}

expect_gte <- function(object, expected, ..., info = NULL, label = NULL) {
  expect_true_fn <- get("expect_true", envir = parent.frame(), inherits = TRUE)
  expect_true_fn(object >= expected, info = info %||% label)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (is.character(x) && length(x) == 1 && is.na(x))) y else x
}

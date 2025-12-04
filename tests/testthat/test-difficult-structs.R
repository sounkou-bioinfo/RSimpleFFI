# Tests for Difficult Struct Cases

library(testthat)
library(RSimpleFFI)

# Helper function to dump debug info when tests fail
dump_parse_debug_info <- function(parsed, header_path) {
  message("\n=== DEBUG INFO FOR DIFFICULT STRUCTS TEST ===")
  message("Header path: ", header_path)
  message("Header exists: ", file.exists(header_path))

  # Show preprocessed code (first 200 lines)
  if (!is.null(parsed$preprocessed_code)) {
    message("\n--- Preprocessed code (first 200 lines) ---")
    lines <- strsplit(parsed$preprocessed_code, "\n")[[1]]
    message(paste(head(lines, 200), collapse = "\n"))
    if (length(lines) > 200) {
      message("... (", length(lines) - 200, " more lines)")
    }
  }

  # Show parsed structs
  message("\n--- Parsed structs (", nrow(parsed$structs), " total) ---")
  if (!is.null(parsed$structs) && nrow(parsed$structs) > 0) {
    for (i in seq_len(min(nrow(parsed$structs), 20))) {
      s <- parsed$structs[i, ]
      message(sprintf("  Struct[%d]: %s", i, s$name))
      if (!is.null(s$fields) && length(s$fields[[1]]) > 0) {
        for (j in seq_along(s$fields[[1]])) {
          f <- s$fields[[1]][[j]]
          bitfield_info <- if (
            !is.null(f$bitfield_width) && !is.na(f$bitfield_width)
          ) {
            sprintf(" :BITFIELD(%s)", f$bitfield_width)
          } else {
            ""
          }
          message(sprintf(
            "    field[%d]: %s %s%s",
            j,
            f$type,
            f$name,
            bitfield_info
          ))
        }
      }
    }
  }

  # Show parsed functions
  message("\n--- Parsed functions (", nrow(parsed$functions), " total) ---")
  if (!is.null(parsed$functions) && nrow(parsed$functions) > 0) {
    for (i in seq_len(min(nrow(parsed$functions), 20))) {
      fn <- parsed$functions[i, ]
      message(sprintf("  Function[%d]: %s %s(...)", i, fn$return_type, fn$name))
    }
  }

  # Check for bitfield_warning attribute
  message("\n--- Bitfield detection ---")
  message(
    "bitfield_warning attribute: ",
    if (!is.null(attr(parsed, "bitfield_warning"))) {
      attr(parsed, "bitfield_warning")
    } else {
      "NULL"
    }
  )

  # Look for any struct fields that might be bitfields
  message("\n--- Searching for bitfield candidates in structs ---")
  if (!is.null(parsed$structs) && nrow(parsed$structs) > 0) {
    found_any <- FALSE
    for (i in seq_len(nrow(parsed$structs))) {
      s <- parsed$structs[i, ]
      if (!is.null(s$fields) && length(s$fields[[1]]) > 0) {
        for (j in seq_along(s$fields[[1]])) {
          f <- s$fields[[1]][[j]]
          if (!is.null(f$bitfield_width) && !is.na(f$bitfield_width)) {
            message(sprintf(
              "  FOUND BITFIELD: %s.%s : %s",
              s$name,
              f$name,
              f$bitfield_width
            ))
            found_any <- TRUE
          }
        }
      }
    }
    if (!found_any) {
      message("  NO BITFIELDS DETECTED IN ANY STRUCT")
    }
  }

  message("=== END DEBUG INFO ===\n")
}

# Helper to generate bindings and check for bitfield warning with debug output
generate_bindings_with_debug <- function(parsed, header_path) {
  warning_caught <- FALSE
  code <- tryCatch(
    withCallingHandlers(
      generate_r_bindings(parsed),
      warning = function(w) {
        if (grepl("bit-?field", conditionMessage(w), ignore.case = TRUE)) {
          warning_caught <<- TRUE
        }
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      dump_parse_debug_info(parsed, header_path)
      stop(e)
    }
  )

  if (!warning_caught) {
    dump_parse_debug_info(parsed, header_path)
  }

  list(code = code, warning_caught = warning_caught)
}

test_that("FILE* and opaque pointers generate valid code", {
  skip_if_not(tcc_available(), "TCC not available")

  header <- system.file(
    "extdata",
    "difficult_structs.h",
    package = "RSimpleFFI"
  )
  skip_if(header == "", "difficult_structs.h not found")

  parsed <- ffi_parse_header(header)

  # Try to get warning, dump debug info if not found
  result <- generate_bindings_with_debug(parsed, header)
  code <- result$code

  expect_true(
    result$warning_caught,
    info = "Expected warning about bit-fields but none was produced. See debug output above."
  )

  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)

  # Must parse without errors
  expect_no_error(parse(tmpfile))

  # Must source without errors
  env <- new.env()
  expect_no_error(source(tmpfile, local = env))

  # Check that some code was generated
  objects <- ls(env)
  expect_true(
    length(objects) > 0,
    info = "Expected some bindings to be generated"
  )

  # Check that our custom FILE* functions are generated if they were parsed
  # (they should be in the parsed functions list)
  parsed_func_names <- parsed$functions$name
  if ("open_file" %in% parsed_func_names) {
    expect_true(
      "r_open_file" %in% objects,
      info = paste(
        "open_file was parsed but r_open_file was not generated.",
        "Generated objects:",
        paste(head(objects, 20), collapse = ", ")
      )
    )
  }
  if ("close_file" %in% parsed_func_names) {
    expect_true(
      "r_close_file" %in% objects,
      info = paste(
        "close_file was parsed but r_close_file was not generated.",
        "Generated objects:",
        paste(head(objects, 20), collapse = ", ")
      )
    )
  }

  unlink(tmpfile)
})

test_that("recursive structs generate valid definitions", {
  skip_if_not(tcc_available(), "TCC not available")

  header <- system.file(
    "extdata",
    "difficult_structs.h",
    package = "RSimpleFFI"
  )
  parsed <- ffi_parse_header(header)
  result <- generate_bindings_with_debug(parsed, header)
  code <- result$code
  expect_true(
    result$warning_caught,
    info = "Expected warning about bit-fields but none was produced. See debug output above."
  )

  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)

  env <- new.env()
  expect_no_error(source(tmpfile, local = env))

  # Check recursive structs exist
  if ("Node" %in% ls(env)) {
    expect_s3_class(env$Node, "RSimpleFFI::StructType")
    # Should have fields: value, next, prev
    expect_true("value" %in% env$Node@fields)
  }

  if ("Tree" %in% ls(env)) {
    expect_s3_class(env$Tree, "RSimpleFFI::StructType")
  }

  unlink(tmpfile)
})

test_that("nested structs generate complete definitions", {
  skip_if_not(tcc_available(), "TCC not available")

  header <- system.file(
    "extdata",
    "difficult_structs.h",
    package = "RSimpleFFI"
  )
  parsed <- ffi_parse_header(header)
  result <- generate_bindings_with_debug(parsed, header)
  code <- result$code
  expect_true(
    result$warning_caught,
    info = "Expected warning about bit-fields but none was produced. See debug output above."
  )

  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)

  env <- new.env()
  expect_no_error(source(tmpfile, local = env))

  # Check nested structs
  if ("Outer" %in% ls(env)) {
    expect_s3_class(env$Outer, "RSimpleFFI::StructType")
  }

  if ("Middle" %in% ls(env)) {
    expect_s3_class(env$Middle, "RSimpleFFI::StructType")
  }

  if ("Inner" %in% ls(env)) {
    expect_s3_class(env$Inner, "RSimpleFFI::StructType")
  }

  unlink(tmpfile)
})

test_that("function pointer types are handled", {
  skip_if_not(tcc_available(), "TCC not available")

  header <- system.file(
    "extdata",
    "difficult_structs.h",
    package = "RSimpleFFI"
  )
  parsed <- ffi_parse_header(header)
  result <- generate_bindings_with_debug(parsed, header)
  code <- result$code
  expect_true(
    result$warning_caught,
    info = "Expected warning about bit-fields but none was produced. See debug output above."
  )

  # Should not have syntax errors even with function pointers
  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)

  expect_no_error(parse(tmpfile))
  expect_no_error(source(tmpfile, local = new.env()))

  unlink(tmpfile)
})

test_that("structs with arrays generate correctly", {
  skip_if_not(tcc_available(), "TCC not available")

  header <- system.file(
    "extdata",
    "difficult_structs.h",
    package = "RSimpleFFI"
  )
  parsed <- ffi_parse_header(header)
  result <- generate_bindings_with_debug(parsed, header)
  code <- result$code
  expect_true(
    result$warning_caught,
    info = "Expected warning about bit-fields but none was produced. See debug output above."
  )

  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)

  env <- new.env()
  expect_no_error(source(tmpfile, local = env))

  # Check struct with array of structs
  if ("Polygon" %in% ls(env)) {
    expect_s3_class(env$Polygon, "RSimpleFFI::StructType")
    expect_true("vertices" %in% env$Polygon@fields)
  }

  unlink(tmpfile)
})

test_that("typedef structs are handled", {
  skip_if_not(tcc_available(), "TCC not available")

  header <- system.file(
    "extdata",
    "difficult_structs.h",
    package = "RSimpleFFI"
  )
  parsed <- ffi_parse_header(header)
  result <- generate_bindings_with_debug(parsed, header)
  code <- result$code
  expect_true(
    result$warning_caught,
    info = "Expected warning about bit-fields but none was produced. See debug output above."
  )

  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)

  env <- new.env()
  expect_no_error(source(tmpfile, local = env))

  # Check typedef'd struct
  if ("LinkedList" %in% ls(env)) {
    expect_s3_class(env$LinkedList, "RSimpleFFI::StructType")
  }

  unlink(tmpfile)
})

test_that("complex function signatures generate valid wrappers", {
  skip_if_not(tcc_available(), "TCC not available")

  header <- system.file(
    "extdata",
    "difficult_structs.h",
    package = "RSimpleFFI"
  )
  parsed <- ffi_parse_header(header)
  result <- generate_bindings_with_debug(parsed, header)
  code <- result$code
  expect_true(
    result$warning_caught,
    info = "Expected warning about bit-fields but none was produced. See debug output above."
  )

  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)

  env <- new.env()
  expect_no_error(source(tmpfile, local = env))

  # Check various function types were generated
  if ("r_create_node" %in% ls(env)) {
    expect_type(env$r_create_node, "closure")
  }

  if ("r_insert_node" %in% ls(env)) {
    expect_type(env$r_insert_node, "closure")
  }

  unlink(tmpfile)
})

test_that("all generated code from difficult_structs.h is valid", {
  skip_if_not(tcc_available(), "TCC not available")

  header <- system.file(
    "extdata",
    "difficult_structs.h",
    package = "RSimpleFFI"
  )
  parsed <- ffi_parse_header(header)
  result <- generate_bindings_with_debug(parsed, header)
  code <- result$code
  expect_true(
    result$warning_caught,
    info = "Expected warning about bit-fields but none was produced. See debug output above."
  )

  # Write and check
  tmpfile <- tempfile(fileext = ".R")
  writeLines(code, tmpfile)

  # This is the ultimate test - must parse
  parsed_code <- parse(tmpfile)
  expect_true(length(parsed_code) > 0)

  # Must source
  env <- new.env()
  expect_no_error(eval(parsed_code, envir = env))

  # Check we got something
  objects <- ls(env)
  expect_true(length(objects) > 0)

  unlink(tmpfile)
})

test_that("no incomplete struct definitions in difficult cases", {
  skip_if_not(tcc_available(), "TCC not available")

  header <- system.file(
    "extdata",
    "difficult_structs.h",
    package = "RSimpleFFI"
  )
  parsed <- ffi_parse_header(header)
  result <- generate_bindings_with_debug(parsed, header)
  code <- result$code
  expect_true(
    result$warning_caught,
    info = "Expected warning about bit-fields but none was produced. See debug output above."
  )

  lines <- strsplit(code, "\n")[[1]]

  # Check for common syntax errors
  for (i in seq_along(lines)) {
    line <- trimws(lines[i])

    # No incomplete assignments
    expect_false(
      grepl("^\\w+\\s*=$", line),
      label = paste("Line", i, "incomplete")
    )

    # No lines with just =
    expect_false(
      grepl("^\\s*=\\s*$", line),
      label = paste("Line", i, "orphan equals")
    )
  }

  # Check struct definitions are balanced
  struct_starts <- grep("<- ffi_struct\\(", lines)
  if (length(struct_starts) > 0) {
    for (start in struct_starts) {
      # Find matching closing paren
      paren_count <- 1
      found_close <- FALSE
      for (j in (start + 1):length(lines)) {
        if (j > length(lines)) {
          break
        }
        paren_count <- paren_count + nchar(gsub("[^(]", "", lines[j]))
        paren_count <- paren_count - nchar(gsub("[^)]", "", lines[j]))
        if (paren_count == 0) {
          found_close <- TRUE
          break
        }
      }
      expect_true(
        found_close,
        label = paste("Struct at line", start, "must close")
      )
    }
  }
})

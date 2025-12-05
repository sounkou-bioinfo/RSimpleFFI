# Generate Package Initialization Code for Library Loading

#' Read and fill a template file
#'
#' Reads a template from inst/templates and replaces placeholders
#'
#' @param template_name Name of the template file (e.g., "DESCRIPTION.template")
#' @param replacements Named list of placeholder replacements
#' @return Character string with filled template
#' @keywords internal
fill_template <- function(template_name, replacements) {
  # Try installed package location first, then source package

  template_path <- system.file(
    "templates",
    template_name,
    package = "RSimpleFFI"
  )

  if (template_path == "" || !file.exists(template_path)) {
    # Try source package location (for development)
    template_path <- file.path("inst", "templates", template_name)
  }

  if (!file.exists(template_path)) {
    stop("Template not found: ", template_name)
  }

  content <- paste(readLines(template_path, warn = FALSE), collapse = "\n")

  for (name in names(replacements)) {
    placeholder <- paste0("{{", name, "}}")
    content <- gsub(placeholder, replacements[[name]], content, fixed = TRUE)
  }

  content
}

#' Generate .onLoad/.onUnload for package
#'
#' Creates the zzz.R file content for loading external libraries
#'
#' @param library_name Name of the shared library (e.g., "mylib")
#' @param package_name Name of the R package
#' @param library_path Optional: specific path to library, or NULL for system search
#' @param use_system_lib Logical: search system library paths
#' @return Character string with zzz.R content
#' @export
#' @examples
#' \dontrun{
#' # Generate for system library
#' code <- generate_package_init("mylib", "MyRPackage", use_system_lib = TRUE)
#' writeLines(code, "R/zzz.R")
#'
#' # Generate for bundled library
#' code <- generate_package_init("mylib", "MyRPackage", use_system_lib = FALSE)
#' }
generate_package_init <- function(
  library_name,
  package_name,
  library_path = NULL,
  use_system_lib = TRUE
) {
  # Use basename without extension for variable name (handles libhts.so.3 -> hts)
  var_name <- sub("^lib", "", sub("\\.(so|dll|dylib).*$", "", library_name))

  if (use_system_lib) {
    # Don't append extension if library_name already has one
    lib_file <- if (grepl("\\.(so|dll|dylib)", library_name)) {
      library_name
    } else {
      paste0(library_name, .Platform$dynlib.ext)
    }
    load_code <- sprintf(
      '  .%s_lib <<- dll_load_system("%s")',
      var_name,
      lib_file
    )
  } else if (!is.null(library_path)) {
    load_code <- sprintf(
      '  .%s_lib <<- dll_load("%s")',
      var_name,
      library_path
    )
  } else {
    # Package-bundled library
    load_code <- sprintf(
      '  lib_file <- system.file("libs",
                          paste0("%s", .Platform$dynlib.ext),
                          package = pkgname)
  if (file.exists(lib_file)) {
    .%s_lib <<- dll_load(lib_file)
  } else {
    warning("Library file not found: ", lib_file)
  }',
      var_name,
      var_name
    )
  }

  fill_template(
    "zzz.R.template",
    list(
      LIBRARY_NAME = var_name,
      LOAD_CODE = load_code
    )
  )
}

#' Generate complete package from header files
##
#' Generate complete package from header files
#'
#' Creates all necessary R files for a package wrapping a C library.
#' Generates a proper R package structure with DESCRIPTION, NAMESPACE,
#' and R code in the R/ subfolder. Uses templates from inst/templates/.
#'
#' @param header_files Character vector of header file paths
#' @param package_name Name of the R package
#' @param library_name Name of the shared library
#' @param output_dir Directory to create the package (package root)
#' @param library_path Optional: full path to shared library (for custom installs)
#' @param use_system_lib Logical: search system library paths
#' @param include_helpers Logical: include allocation helper functions
#' @param authors_r Authors@@R field for DESCRIPTION (R code string). Default creates a placeholder person().
#' @param title Package title (default: auto-generated)
#' @param description Package description (default: auto-generated)
#' @return Invisibly returns list of generated files
#' @export
#' @examples
#' \dontrun{
#' generate_package_from_headers(
#'   header_files = c("mylib.h", "mylib_utils.h"),
#'   package_name = "MyRPackage",
#'   library_name = "mylib",
#'   output_dir = "MyRPackage",
#'   library_path = "/custom/path/libmylib.so",
#'   use_system_lib = TRUE,
#'   include_helpers = TRUE,
#'   authors_r = 'person("John", "Doe", email = "john@example.com", role = c("aut", "cre"))',
#'   title = "FFI Bindings to mylib",
#'   description = "Auto-generated FFI bindings for mylib."
#' )
#' }
generate_package_from_headers <- function(
  header_files,
  package_name,
  library_name,
  output_dir = package_name,
  library_path = NULL,
  use_system_lib = TRUE,
  include_helpers = TRUE,
  authors_r = NULL,
  title = NULL,
  description = NULL
) {
  # Create package root directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Create R/ subdirectory
  r_dir <- file.path(output_dir, "R")
  if (!dir.exists(r_dir)) {
    dir.create(r_dir, recursive = TRUE)
  }

  generated_files <- character()

  # Set defaults
  if (is.null(title)) {
    title <- sprintf("R Bindings for %s Library", library_name)
  }
  if (is.null(description)) {
    description <- sprintf(
      "Auto-generated R bindings for the %s C library using RSimpleFFI.",
      library_name
    )
  }
  if (is.null(authors_r)) {
    authors_r <- 'person("Package", "Author", email = "author@example.com", role = c("aut", "cre"))'
  }

  # 1. Generate DESCRIPTION file from template
  description_content <- fill_template(
    "DESCRIPTION.template",
    list(
      PACKAGE_NAME = package_name,
      TITLE = title,
      AUTHORS_R = authors_r,
      DESCRIPTION = description
    )
  )
  description_file <- file.path(output_dir, "DESCRIPTION")
  writeLines(description_content, description_file)
  generated_files <- c(generated_files, description_file)

  # 2. Generate NAMESPACE file from template
  namespace_content <- fill_template(
    "NAMESPACE.template",
    list(
      LIBRARY_NAME = library_name
    )
  )
  namespace_file <- file.path(output_dir, "NAMESPACE")
  writeLines(namespace_content, namespace_file)
  generated_files <- c(generated_files, namespace_file)

  # 3. Generate zzz.R for library loading (in R/ subfolder)
  zzz_code <- generate_package_init(
    library_name,
    package_name,
    library_path = library_path,
    use_system_lib = use_system_lib
  )
  zzz_file <- file.path(r_dir, "zzz.R")
  writeLines(zzz_code, zzz_file)
  generated_files <- c(generated_files, zzz_file)

  # 4. Parse each header and generate bindings (in R/ subfolder)
  all_bindings <- list()
  all_exports <- character()

  for (header_file in header_files) {
    if (!file.exists(header_file)) {
      warning("Header file not found: ", header_file)
      next
    }

    parsed <- ffi_parse_header(header_file)
    base_name <- tools::file_path_sans_ext(basename(header_file))

    # Generate bindings
    bindings_code <- generate_r_bindings(parsed)
    bindings_file <- file.path(r_dir, paste0(base_name, "_bindings.R"))
    writeLines(bindings_code, bindings_file)
    generated_files <- c(generated_files, bindings_file)

    # Collect function names for NAMESPACE exports
    if (length(parsed$functions) > 0) {
      func_names <- paste0("r_", parsed$functions$name)
      all_exports <- c(all_exports, func_names)
    }

    all_bindings[[base_name]] <- parsed
  }

  # 5. Generate helper functions if requested (in R/ subfolder)
  if (include_helpers) {
    helpers_content <- fill_template(
      "helpers.R.template",
      list(
        PACKAGE_NAME = package_name
      )
    )
    helpers_file <- file.path(r_dir, "helpers.R")
    writeLines(helpers_content, helpers_file)
    generated_files <- c(generated_files, helpers_file)

    # Add helper exports to NAMESPACE
    namespace_content <- paste0(
      namespace_content,
      "\nexport(create_struct_from_list)\nexport(struct_to_list)\n"
    )
    writeLines(namespace_content, namespace_file)
  }

  # 6. Create LICENSE file from template
  license_content <- fill_template(
    "LICENSE.template",
    list(
      YEAR = format(Sys.Date(), "%Y"),
      COPYRIGHT_HOLDER = "Package Author"
    )
  )
  license_file <- file.path(output_dir, "LICENSE")
  writeLines(license_content, license_file)
  generated_files <- c(generated_files, license_file)

  invisible(list(
    files = generated_files,
    bindings = all_bindings,
    package_dir = output_dir
  ))
}

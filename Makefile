# h/t to @jimhester and @yihui for this parse block:
# https://github.com/yihui/knitr/blob/dc5ead7bcfc0ebd2789fe99c527c7d91afb3de4a/Makefile#L1-L4
# Note the portability change as suggested in the manual:
# https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Writing-portable-packages
PKGNAME = `sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION`
PKGVERS = `sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION`


all: check



rd:
	R -e 'roxygen2::roxygenize()'
build: install_deps
	R CMD build .

check: build
	R CMD check --no-manual $(PKGNAME)_$(PKGVERS).tar.gz

install_deps:
	R \
	-e 'if (!requireNamespace("remotes")) install.packages("remotes")' \
	-e 'remotes::install_deps(dependencies = TRUE)'

install: build
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

clean:
	@rm -rf $(PKGNAME)_$(PKGVERS).tar.gz $(PKGNAME).Rcheck

# Development targets
dev-install:
	R CMD INSTALL --preclean .

dev-test:
	R -e 'library(RSimpleFFI); cat("TCC available:", tcc_available(), "\n"); cat("Test data dir:", system.file("extdata", package="RSimpleFFI"), "\n")'

dev-preprocess-test:
	@R -e 'library(RSimpleFFI); header <- system.file("extdata", "simple_types.h", package="RSimpleFFI"); result <- tcc_preprocess(header); cat("=== Preprocessed simple_types.h ===\n"); cat(head(result[!grepl("^#", result)], 20), sep="\n")'

dev-parse-test:
	@R -e 'library(RSimpleFFI); header <- system.file("extdata", "simple_types.h", package="RSimpleFFI"); result <- tcc_parse_header(header); cat("=== simple_types.h ===\n"); cat("DEFINES:", length(result$$defines), "\n"); cat("STRUCTS:", paste(names(result$$structs), collapse=", "), "\n"); cat("FUNCTIONS:", nrow(result$$functions), "\n")'

dev-function-test:
	@R -e 'library(RSimpleFFI); header <- system.file("extdata", "simple_types.h", package="RSimpleFFI"); result <- tcc_parse_header(header); cat("=== Functions from simple_types.h ===\n"); print(result$$functions[, c("name", "return_type", "params")])'

dev-struct-test:
	@R -e 'library(RSimpleFFI); header <- system.file("extdata", "typedef_structs.h", package="RSimpleFFI"); result <- tcc_parse_header(header); cat("=== Typedef structs ===\n"); for (s in names(result$$structs)) { cat("\n", s, ":\n"); for (f in result$$structs[[s]]) cat("  ", f$$type, f$$name, "\n") }'

dev-weird-spacing-test:
	@R -e 'library(RSimpleFFI); header <- system.file("extdata", "weird_spacing.h", package="RSimpleFFI"); result <- tcc_parse_header(header); cat("=== Weird spacing header ===\n"); cat("DEFINES:", names(result$$defines), "\n"); cat("STRUCTS:", names(result$$structs), "\n"); cat("FUNCTIONS:\n"); print(result$$functions[, c("name", "return_type")])'

dev-full-parse-test:
	@R -e 'library(RSimpleFFI); header <- system.file("extdata", "simple_types.h", package="RSimpleFFI"); result <- tcc_parse_header(header); cat("\n=== DEFINES ===\n"); print(result$$defines); cat("\n=== STRUCTS ===\n"); str(result$$structs, max.level=2); cat("\n=== FUNCTIONS ===\n"); print(result$$functions)'

dev-typedef-test:
	@R -e 'library(RSimpleFFI); header <- system.file("extdata", "with_includes.h", package="RSimpleFFI"); inc_dir <- system.file("extdata", "includes", package="RSimpleFFI"); result <- tcc_parse_header(header, includes=inc_dir); cat("=== with_includes.h (using include dir) ===\n"); cat("DEFINES from includes - BUFFER_SIZE:", result$$defines$$BUFFER_SIZE, "\n"); cat("DEFINES from includes - TIMEOUT_MS:", result$$defines$$TIMEOUT_MS, "\n"); cat("Typedef structs found:", sum(c("Event", "Buffer", "EventQueue") %in% names(result$$structs)), "/ 3\n"); cat("Has Event struct:", "Event" %in% names(result$$structs), "\n"); cat("Has Buffer struct:", "Buffer" %in% names(result$$structs), "\n"); cat("\nFunctions:\n"); print(result$$functions[!grepl("^extern", result$$functions$$return_type), c("name", "return_type")])'

dev-complex-test:
	@R -e 'library(RSimpleFFI); header <- system.file("extdata", "complex_types.h", package="RSimpleFFI"); result <- tcc_parse_header(header); cat("=== complex_types.h ===\n"); cat("Structs with arrays/pointers:\n"); for (s in names(result$$structs)) { cat("\n", s, ":\n"); for (f in head(result$$structs[[s]], 3)) cat("  ", f$$type, f$$name, "\n") }'

dev-all-tests: dev-install dev-test dev-preprocess-test dev-parse-test dev-function-test dev-struct-test dev-weird-spacing-test dev-typedef-test dev-full-parse-test dev-complex-test
	@echo "All development tests completed!"

.PHONY: all rd build check install_deps install clean dev-install dev-test dev-preprocess-test dev-parse-test dev-all-tests

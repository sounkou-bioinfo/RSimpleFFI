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
	R -e 'library(RSimpleFFI); cat("TCC available:", tcc_available(), "\n")'

dev-preprocess-test:
	@echo "# Test header" > /tmp/test.h
	@echo "#define MAX 100" >> /tmp/test.h
	@echo "struct Point { int x; int y; };" >> /tmp/test.h
	@echo "int add(int a, int b);" >> /tmp/test.h
	@R -e 'library(RSimpleFFI); result <- tcc_preprocess("/tmp/test.h"); cat(result, sep="\n")'

dev-parse-test:
	@echo "# Test header" > /tmp/test.h
	@echo "#define MAX 100" >> /tmp/test.h
	@echo "struct Point { int x; int y; };" >> /tmp/test.h
	@echo "int add(int a, int b);" >> /tmp/test.h
	@R -e 'library(RSimpleFFI); defines <- tcc_extract_defines("/tmp/test.h"); print(defines)'

dev-function-test:
	@echo "# Test header" > /tmp/test.h
	@echo "#define MAX 100" >> /tmp/test.h
	@echo "struct Point { int x; int y; };" >> /tmp/test.h
	@echo "int add(int a, int b);" >> /tmp/test.h
	@echo "double multiply(double x, double y);" >> /tmp/test.h
	@R -e 'library(RSimpleFFI); pre <- tcc_preprocess("/tmp/test.h"); funcs <- tcc_extract_functions(pre); print(funcs)'

dev-struct-test:
	@echo "# Test header" > /tmp/test.h
	@echo "struct Point { int x; int y; };" >> /tmp/test.h
	@echo "struct Color { unsigned char r; unsigned char g; unsigned char b; };" >> /tmp/test.h
	@R -e 'library(RSimpleFFI); pre <- tcc_preprocess("/tmp/test.h"); structs <- tcc_extract_structs(pre); print(structs)'

dev-full-parse-test:
	@echo "Creating comprehensive test header..."
	@echo "#define MAX_SIZE 1024" > /tmp/test.h
	@echo "#define PI 3.14159" >> /tmp/test.h
	@echo "" >> /tmp/test.h
	@echo "struct Point { int x; int y; };" >> /tmp/test.h
	@echo "struct Rectangle { int width; int height; };" >> /tmp/test.h
	@echo "" >> /tmp/test.h
	@echo "int add(int a, int b);" >> /tmp/test.h
	@echo "double sqrt(double x);" >> /tmp/test.h
	@echo "void print_point(struct Point p);" >> /tmp/test.h
	@R -e 'library(RSimpleFFI); result <- tcc_parse_header("/tmp/test.h"); cat("\n=== DEFINES ===\n"); print(result$$defines); cat("\n=== STRUCTS ===\n"); print(result$$structs); cat("\n=== FUNCTIONS ===\n"); print(result$$functions)'

dev-all-tests: dev-install dev-test dev-preprocess-test dev-parse-test dev-function-test dev-struct-test dev-full-parse-test
	@echo "All development tests completed!"

.PHONY: all rd build check install_deps install clean dev-install dev-test dev-preprocess-test dev-parse-test dev-all-tests

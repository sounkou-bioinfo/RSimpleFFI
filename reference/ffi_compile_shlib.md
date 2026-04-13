# Compile C code using R CMD SHLIB

Takes C source code, writes it to a temporary file, compiles it with R
CMD SHLIB, and loads the resulting shared library. Uses the system
compiler (gcc/clang/MSVC) for reliability and full language support.

## Usage

``` r
ffi_compile_shlib(c_source, include_dirs = NULL, verbose = FALSE)
```

## Arguments

- c_source:

  Character string containing C code

- include_dirs:

  Character vector of include directories (optional)

- verbose:

  Logical, print compilation output (default FALSE)

## Value

List with elements:

- dll: DLL handle from dyn.load()

- path: Path to the compiled .so/.dll file

- tmpdir: Temporary directory (kept alive for DLL lifetime)

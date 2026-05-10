# Compile C code with Rtinycc

Takes C source code, writes it to a temporary file for diagnostics,
compiles it in memory with Rtinycc, and returns a handle that can
resolve \`.Call()\` symbols.

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

- state: Rtinycc compilation state

- path: Path to the diagnostic source file

- tmpdir: Temporary directory kept alive for compiler state

# Load a shared library/DLL

This function compiles C code using R's configured compiler and loads
it. Uses the same compiler configuration that R was built with.

## Usage

``` r
dll_load(filename, now = TRUE, local = TRUE, verbose = FALSE)

dll_unload(handle, verbose = FALSE)

dll_symbol(symbol_name, package = NULL)

dll_compile_and_load(
  code,
  name = "temp_dll",
  includes = NULL,
  libs = NULL,
  verbose = FALSE,
  cflags = NULL,
  compilation_directory = tempfile("dll_compile_")
)

dll_load_system(lib_name, verbose = FALSE)
```

## Arguments

- filename:

  Path to the shared library

- now:

  Whether to resolve all symbols immediately (default TRUE)

- local:

  Keep symbols local to avoid namespace pollution (default TRUE)

- verbose:

  Print loading information (default FALSE)

- handle:

  Library handle (path) returned by dll_load()

- symbol_name:

  Name of the symbol to find

- package:

  Package name where symbol is registered (optional)

- code:

  Character vector of C code to compile

- name:

  Base name for the compiled library (default "temp_dll")

- includes:

  Additional include directories

- libs:

  Additional libraries to link

- cflags:

  Additional compiler flags (e.g., "-O2", "-O3")

- compilation_directory:

  Directory to use for compilation (default temp dir)

- lib_name:

  Name of system library (e.g., libc.so.6, libm.dylib, kernel32.dll)

## Value

Library handle (character string of loaded library path)

dyn.unload result invisibly

Symbol information including address as external pointer

Library handle that can be used with dll\_\* functions

Library handle or NULL if not found

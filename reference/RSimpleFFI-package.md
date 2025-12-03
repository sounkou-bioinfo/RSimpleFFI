# RSimpleFFI: Simple Foreign Function Interface using 'S7' and 'libffi'

Simple Foreign Function Interface for 'R' using 'libffi' and 'S7'
classes. Supports calling 'C' functions with type conversion and struct
handling. Includes standard 'C' types (int8, int16, int32, int64, uint
variants), platform types (size_t, bool), floating point types, and
complex struct types. Experimental header parsing using 'tinycc' enables
automatic generation of 'R' bindings from 'C' header files, simplifying
package development for 'C' libraries.

## See also

Useful links:

- <https://github.com/sounkou-bioinfo/RSimpleFFI>

- <https://sounkou-bioinfo.github.io/RSimpleFFI/>

- Report bugs at <https://github.com/sounkou-bioinfo/RSimpleFFI/issues>

## Author

**Maintainer**: Sounkou Mahamane Toure <sounkoutoure@gmail.com>

Other contributors:

- Anthony Green, Red Hat, Inc and others (libffi authors and COPYRIGHT
  holders) \[contributor\]

- Fabrice Bellard and tinycc Authors (Tinycc Compiler (tinycc) authors
  and COPYRIGHT holders) \[contributor\]

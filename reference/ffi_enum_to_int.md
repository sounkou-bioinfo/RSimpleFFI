# Convert enum name to integer value

Look up the integer value for a named enum constant.

## Usage

``` r
ffi_enum_to_int(enum_type, name)
```

## Arguments

- enum_type:

  EnumType object

- name:

  Character name of enum constant

## Value

Integer value

## Examples

``` r
if (FALSE) { # \dontrun{
Color <- ffi_enum(RED = 0L, GREEN = 1L, BLUE = 2L)
ffi_enum_to_int(Color, "GREEN")  # 1L
} # }
```

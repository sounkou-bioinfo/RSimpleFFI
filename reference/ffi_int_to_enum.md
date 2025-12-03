# Convert integer value to enum name

Look up the enum constant name for an integer value.

## Usage

``` r
ffi_int_to_enum(enum_type, value)
```

## Arguments

- enum_type:

  EnumType object

- value:

  Integer value

## Value

Character name of enum constant (or NA if not found)

## Examples

``` r
if (FALSE) { # \dontrun{
Color <- ffi_enum(RED = 0L, GREEN = 1L, BLUE = 2L)
ffi_int_to_enum(Color, 1L)  # "GREEN"
} # }
```

# Convert pointer to string safely

Explicitly converts an external pointer to a character string. Use this
instead of relying on automatic conversion heuristics.

## Usage

``` r
pointer_to_string(ptr)
```

## Arguments

- ptr:

  External pointer that points to a null-terminated string

## Value

Character vector of length 1, or NULL if pointer is NULL

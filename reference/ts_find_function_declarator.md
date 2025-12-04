# Find function_declarator recursively inside pointer_declarator chain

For declarations like `int** foo(...)`, the function_declarator is
nested inside multiple pointer_declarator nodes.

## Usage

``` r
ts_find_function_declarator(node)
```

## Arguments

- node:

  A pointer_declarator node

## Value

The function_declarator node if found, NULL otherwise

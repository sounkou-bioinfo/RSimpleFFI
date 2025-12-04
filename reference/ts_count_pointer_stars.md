# Count asterisks in a pointer_declarator by traversing AST

Counts the number of pointer indirection levels by finding all '\*'
nodes in the pointer_declarator subtree.

## Usage

``` r
ts_count_pointer_stars(declarator_node)
```

## Arguments

- declarator_node:

  A pointer_declarator node

## Value

Integer count of pointer asterisks

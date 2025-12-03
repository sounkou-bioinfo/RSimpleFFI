# Extract function declarations from preprocessed C code

Extract function declarations from preprocessed C code

## Usage

``` r
tcc_extract_functions(preprocessed_lines)
```

## Arguments

- preprocessed_lines:

  Character vector from tcc_preprocess()

## Value

Data frame with columns: name, return_type, params, full_declaration

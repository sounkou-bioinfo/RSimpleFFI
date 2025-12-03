# Extract struct definitions from preprocessed C code

Extract struct definitions from preprocessed C code

## Usage

``` r
tcc_extract_structs(preprocessed_lines)
```

## Arguments

- preprocessed_lines:

  Character vector from tcc_preprocess()

## Value

List of struct definitions (name -\> list of fields). Each struct may
have a "packed" attribute set to TRUE if **attribute**((packed)) or
\#pragma pack was detected.

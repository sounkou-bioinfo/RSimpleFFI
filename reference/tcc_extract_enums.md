# Extract enum definitions from preprocessed C code

Extract enum definitions from preprocessed C code

## Usage

``` r
tcc_extract_enums(preprocessed_lines)
```

## Arguments

- preprocessed_lines:

  Character vector from tcc_preprocess()

## Value

List of enum definitions (name -\> named integer vector of values)

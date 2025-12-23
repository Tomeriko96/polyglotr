# Check if a language code is valid

This function checks if a given language code is in the
`google_supported_languages` dataset.

## Usage

``` r
google_is_valid_language_code(language_code)
```

## Arguments

- language_code:

  The language code to check.

## Value

A logical value indicating if the language code is valid.

## Examples

``` r
if (FALSE) { # \dontrun{
google_is_valid_language_code("en") # TRUE
google_is_valid_language_code("fr") # TRUE
google_is_valid_language_code("xx") # FALSE
} # }
```

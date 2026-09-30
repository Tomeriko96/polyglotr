# Get Supported Languages

Returns the languages that polyglotr's Google Translate functions
accept, i.e. the bundled
[google_supported_languages](https://tomeriko96.github.io/polyglotr/reference/google_supported_languages.md)
snapshot. This is the same table
[`google_is_valid_language_code()`](https://tomeriko96.github.io/polyglotr/reference/google_is_valid_language_code.md)
validates against, so a code listed here is always accepted by the
validator and vice versa.

## Usage

``` r
google_get_supported_languages()
```

## Value

A tibble with the columns `Language` and `ISO-639 code`.

## Details

Up to polyglotr 1.7.5 this function scraped the Google Cloud Translation
documentation page and returned its first table. That page now carries
several tables and the first one is no longer the language list, so the
scrape returned a different set of languages with different columns.

## See also

[google_supported_languages](https://tomeriko96.github.io/polyglotr/reference/google_supported_languages.md),
[`google_is_valid_language_code()`](https://tomeriko96.github.io/polyglotr/reference/google_is_valid_language_code.md)

## Examples

``` r
head(google_get_supported_languages())
#> # A tibble: 6 × 2
#>   Language  `ISO-639 code`
#>   <chr>     <chr>         
#> 1 Afrikaans af            
#> 2 Albanian  sq            
#> 3 Amharic   am            
#> 4 Arabic    ar            
#> 5 Armenian  hy            
#> 6 Assamese  as            
```

# Get Apertium Language Pairs

This function retrieves the supported language pairs from the Apertium
API.

## Usage

``` r
apertium_get_language_pairs(host = "https://apertium.org/apy")
```

## Arguments

- host:

  Host URL for the Apertium API (default is "https://apertium.org/apy").

## Value

A list of language pairs. Each element contains sourceLanguage and
targetLanguage.

## Examples

``` r
# \donttest{
pairs <- apertium_get_language_pairs()
head(pairs, 5)
#> # A tibble: 5 × 2
#>   sourceLanguage targetLanguage
#>   <chr>          <chr>         
#> 1 afr            nld           
#> 2 ara            mlt           
#> 3 ara            mlt_translit  
#> 4 arg            cat           
#> 5 arg            cat_pre2017   

# }
```

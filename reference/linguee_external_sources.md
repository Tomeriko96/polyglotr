# Retrieve external sources using Linguee Translation API

Retrieve external sources using Linguee Translation API

## Usage

``` r
linguee_external_sources(query, src, dst, limit = 5)
```

## Arguments

- query:

  The word or phrase for which you want to retrieve external sources.

- src:

  The source language of the word or phrase. Accepts language codes such
  as "en", "es", "fr", etc.

- dst:

  The target language for the external source retrieval. Accepts
  language codes such as "en", "es", "fr", etc.

- limit:

  The maximum number of external sources to retrieve. Defaults to 5.

## Value

A dataframe of external sources with columns: src, dst, src_url,
dst_url.

## See also

linguee_word_translation, linguee_translation_examples

## Examples

``` r
# \donttest{
linguee_external_sources(query = "hello", src = "en", dst = "es")
#> # A tibble: 27 × 4
#>    src                                                     dst   src_url dst_url
#>    <chr>                                                   <chr> <chr>   <chr>  
#>  1 "Hello, my name is Maitri [...] and I just moved to th… "Hol… http:/… http:/…
#>  2 "Hello, I have just tested your service and I must say… "Bue… http:/… http:/…
#>  3 "I therefore believe that a standardised portal along … "Por… http:/… http:/…
#>  4 "Hello, Just a word to let [...] you know how satisfie… "Hol… http:/… http:/…
#>  5 "After all, whenever she visits the classrooms [...] w… "Des… http:/… http:/…
#>  6 "The man joins and [...] woman and men say: hello."     "El … http:/… http:/…
#>  7 "When living abroad, even [...] a stranger saying, \"H… "Cua… http:/… http:/…
#>  8 "Could we find a more user-friendly [...] name, such a… "¿No… http:/… http:/…
#>  9 "Hello, I would like to request [...] information of t… "Hol… http:/… http:/…
#> 10 "Another picks up a banana, holds it to his ear like a… "Otr… http:/… http:/…
#> # ℹ 17 more rows
# }
```

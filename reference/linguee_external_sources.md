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
#> linguee-api is unavailable: Couldn't resolve host name [linguee-api.fly.dev]:
#> Could not resolve host: linguee-api.fly.dev
# }
```

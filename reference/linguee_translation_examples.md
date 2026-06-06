# Provide translation examples using Linguee Translation API

Provide translation examples using Linguee Translation API

## Usage

``` r
linguee_translation_examples(
  query,
  src,
  dst,
  guess_direction = FALSE,
  follow_corrections = "always"
)
```

## Arguments

- query:

  The word or phrase for which you want translation examples.

- src:

  The source language of the word or phrase. Accepts language codes such
  as "en", "es", "fr", etc.

- dst:

  The target language for the translation examples. Accepts language
  codes such as "en", "es", "fr", etc.

- guess_direction:

  A boolean flag that determines whether the API should guess the
  translation direction. The default value is FALSE.

- follow_corrections:

  Specifies how to treat responses with a "did you mean" link. Possible
  values are "always", "never", or "on_empty_translations". The default
  value is "always".

## Value

A dataframe of translation examples with columns: source, target, pos.

## See also

linguee_word_translation

## Examples

``` r
# \donttest{
linguee_translation_examples(query = "hello", src = "en", dst = "es")
#> linguee-api is unavailable: Couldn't resolve host name [linguee-api.fly.dev]:
#> Could not resolve host: linguee-api.fly.dev
# }
```

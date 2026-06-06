# Translate word using Linguee Translation API

Translate word using Linguee Translation API

## Usage

``` r
linguee_word_translation(
  word,
  target_language,
  source_language,
  guess_direction = FALSE,
  follow_corrections = "always"
)
```

## Arguments

- word:

  This is the word that you want to translate.

- target_language:

  This is the language that you want to translate the word into.

- source_language:

  This is the language of the word that you want to translate.

- guess_direction:

  Specifies whether the API should guess the translation direction when
  the source language is set to "auto". The default value is FALSE.

- follow_corrections:

  Specifies whether the API should include translations that have been
  marked as corrections. The default value is "always" to include
  corrections.

## Value

Translated word options.

## Examples

``` r
# \donttest{
linguee_word_translation("hello", target_language = "es", source_language = "en")
#> linguee-api is unavailable: Couldn't resolve host name [linguee-api.fly.dev]:
#> Could not resolve host: linguee-api.fly.dev
# }
```

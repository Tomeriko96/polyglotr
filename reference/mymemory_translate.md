# Translate text using mymemory translate

Translate text using mymemory translate

## Usage

``` r
mymemory_translate(text, target_language = "en", source_language = "auto")
```

## Arguments

- text:

  Text to translate.

- target_language:

  Language to translate text to.

- source_language:

  Language to translate text from

## Value

Translated text.

## Examples

``` r
# \donttest{
mymemory_translate("Hello World", target_language = "es", source_language = "en")
#> [1] "Hola mundo"
# }
```

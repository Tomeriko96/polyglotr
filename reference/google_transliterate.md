# Transliterate a single word or a sentence to the required language.

Transliterate a single word or a sentence to the required language.

## Usage

``` r
google_transliterate(text, language_tag = "el", num = 5)
```

## Arguments

- text:

  The word or sentence to transliterate from Latin/Roman (English)
  script.

- language_tag:

  The target language's ISO639 code. The default value for this argument
  is "el" for Greek.

- num:

  The maximum number of suggestions to fetch. The default value for this
  argument is 5.

## Value

Character vector of transliterated sentences or larger pieces of text.

## Examples

``` r
if (FALSE) { # \dontrun{
google_transliterate("Hello world", "fr", 10)
google_transliterate("hello", "el", 10)
} # }
```

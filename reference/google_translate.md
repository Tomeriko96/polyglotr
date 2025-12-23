# Translate text using Google Translate

Translates input text to a specified language using the Google Translate
mobile web interface. Automatically detects and preserves URLs by
temporarily replacing them with placeholders.

## Usage

``` r
google_translate(text, target_language = "en", source_language = "auto")
```

## Arguments

- text:

  This is the text that you want to translate. Can be a single string or
  a vector of strings.

- target_language:

  This is the language that you want to translate the text into. The
  default value is "en" (English).

- source_language:

  This is the language of the text to be translated. The default value
  is "auto", which attempts automatic language detection.

## Value

A translated string or vector of translated strings, matching the length
of the input.

## Examples

``` r
# \donttest{
# Translate a simple sentence
google_translate("I love languages", target_language = "es")
#> [1] "me encantan los idiomas"

# Translate a vector of words
text_to_translate <- c("the", "quick", "brown")
google_translate(text_to_translate, "fr", "en")
#> [1] "le"     "rapide" "brun"  

# Translate text containing a URL
google_translate("Visit http://example.com for more info.", target_language = "de")
#> [1] "Weitere Informationen finden Sie unter http://example.com."
# }
```

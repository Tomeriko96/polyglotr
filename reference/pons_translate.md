# Translate text using PONS

Translate text using PONS

## Usage

``` r
pons_translate(text, target_language = "pt", source_language = "en")
```

## Arguments

- text:

  This is the text that you want to translate. Can be a single string or
  a vector of strings.

- target_language:

  This is the language that you want to translate the text into. The
  default value for this argument is "pt" for Portuguese.

- source_language:

  This is the language of the text that you want to translate. The
  default value for this argument is "en" for English.

## Value

Translated text. If the input is a vector, it returns a character vector
of translated strings.

## Examples

``` r
if (FALSE) { # \dontrun{
pons_translate("I love languages!", target_language = "pt", source_language = "en")
text_to_translate <- c("The", "Greatest", "Language")
pons_translate(text_to_translate, "pt", "en")
} # }
```

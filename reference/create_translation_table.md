# Create a Translation Table

This function generates a translation table by translating a list of
words into multiple languages.

## Usage

``` r
create_translation_table(words, languages)
```

## Arguments

- words:

  A character vector containing the words to be translated.

- languages:

  A character vector specifying the target languages for translation.

## Value

A data frame representing the translation table with original words and
translations in each language.

## Examples

``` r
if (FALSE) { # \dontrun{
words <- c("Hello", "Translate", "Table", "Script")
languages <- c("es", "fr", "de", "nl")
translations <- create_translation_table(words, languages)
print(translations)
} # }
```

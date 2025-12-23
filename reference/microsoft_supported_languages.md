# Get the set of languages currently supported by the Microsoft Translator API

Get the set of languages currently supported by the Microsoft Translator
API

## Usage

``` r
microsoft_supported_languages(scope = NULL)
```

## Arguments

- scope:

  (optional) A comma-separated list of names defining the group of
  languages to return. Allowed group names are: translation,
  transliteration, and dictionary. If no scope is given, then all groups
  are returned.

## Value

A list of supported languages for the specified groups.

## Examples

``` r
if (FALSE) { # \dontrun{
microsoft_supported_languages(scope = "translation,transliteration,dictionary")
} # }
```

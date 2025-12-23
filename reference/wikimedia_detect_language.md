# Detect the language of a text

This function sends a POST request to the Wikimedia Language ID API with
the specified text, parses the JSON response, and returns the detected
language.

## Usage

``` r
wikimedia_detect_language(text)
```

## Arguments

- text:

  The text whose language is to be detected.

## Value

The detected language.

## Examples

``` r
{
# \donttest{
# Detect the language of a text
wikimedia_detect_language("Hallo, wereld")
# }
}
#> [1] "nl"
```

# Translate text using Apertium

Translate text using Apertium

## Usage

``` r
apertium_translate(
  text,
  target_language,
  source_language,
  host = "https://apertium.org/apy"
)
```

## Arguments

- text:

  Text to translate. Can be a single string or a vector of strings.

- target_language:

  Language to translate text to.

- source_language:

  Language to translate text from.

- host:

  Host URL for the Apertium API (default is "https://apertium.org/apy").

## Value

Translated text. Returns a vector if input is a vector.

## Examples

``` r
# \donttest{
apertium_translate("Hello World", target_language = "es", source_language = "en")
#> [1] "Hola Mundo"
apertium_translate("Hola mundo", target_language = "en", source_language = "es")
#> [1] "Hello World"

# Translate multiple texts
texts <- c("Hello", "Good morning", "Thank you")
apertium_translate(texts, target_language = "es", source_language = "en")
#> [1] "Hola"        "Buenos días" "Gracias"    
# }
```

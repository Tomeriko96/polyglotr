# Translate content using WMCloud

This function sends a POST request to the WMCloud translation API with
the specified parameters, parses the JSON response, and returns the
translated content.

## Usage

``` r
wmcloud_translate(
  content,
  target_language = "en",
  source_language = "en",
  format = "text",
  model = "nllb200-600M"
)
```

## Arguments

- content:

  The content to translate. Can be plain text, a URL (for a webpage), a
  JSON string, or a Markdown string.

- target_language:

  The target language for the translation (default is "en").

- source_language:

  The source language of the content (default is "en").

- format:

  The format of the content ("json", "markdown", "text", "webpage").

- model:

  The model to use for the translation (only "nllb200-600M" is currently
  known to work).

## Value

The translated content.

## Examples

``` r
if (FALSE) { # \dontrun{
# Translate plain text
wmcloud_translate("rijst",
target_language = "es",
source_language = "nl", format = "text")

# Translate a webpage
wmcloud_translate("https://en.m.wikivoyage.org/wiki/Goes",
target_language = "es",
source_language = "en", format = "webpage")

# Translate JSON content
wmcloud_translate('{
    "id": 1,
    "title": "Chicken Biryani",
    "description": "Chicken Biryani is a savory chicken and rice dish",
    "ingredients": [ "Vegetable oil", "Garlic", "Ginger" ,"Rice"]
}
               ', target_language = "es", source_language = "en", format = "json")

# Translate Markdown content
wmcloud_translate('# Heading

This is a [link to Wikipedia](https://wikipedia.org)
                ', target_language = "es", source_language = "en", format = "markdown")
} # }
```

# QCRI Translate Text

This function translates a text from the source language to the target
language using the QCRI Multiterm API.

## Usage

``` r
qcri_translate_text(text, langpair, domain, api_key = qcri_api_key())
```

## Arguments

- text:

  The text to be translated. This must be URL encoded.

- langpair:

  The source-target language pair, where source is language of the
  provided text and target is the language into which the text has to be
  translated.

- domain:

  The domain over which the translation is tuned.

- api_key:

  The API key associated with the user account being used. If not
  provided, the function will attempt to retrieve it from the
  QCRI_API_KEY environment variable.

## Value

Translated text.

## Examples

``` r
if (FALSE) { # \dontrun{
qcri_translate_text(text = "Hello, world!",
langpair = "en-ar",
domain = "general",
api_key = "YourApiKey")
qcri_translate_text(text = "Hello, world!",
langpair = "en-ar",
domain = "general")
} # }
```

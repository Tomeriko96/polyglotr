# Translate Long Text Using Google Translate

Translates long text from one language to another using Google Translate
by splitting the input into manageable chunks if necessary.

## Usage

``` r
google_translate_long_text(
  text,
  target_language = "en",
  source_language = "auto",
  chunk_size = 1000,
  preserve_newlines = FALSE
)
```

## Arguments

- text:

  A single character string with the text to translate.

- target_language:

  The language code to translate the text into (default: "en" for
  English).

- source_language:

  The language code of the input text (default: "auto" for automatic
  detection).

- chunk_size:

  Maximum number of characters per translation request (default: 1000).

- preserve_newlines:

  Logical; if TRUE, preserves newlines between chunks in the output. If
  FALSE (default), newlines are replaced with spaces.

## Value

A single character string containing the translated text.

## Examples

``` r
if (FALSE) { # \dontrun{
long_text <- paste(rep("This is a long text to translate.", 100), collapse = " ")
google_translate_long_text(
  long_text,
  target_language = "de",
  source_language = "en",
  chunk_size = 500,
  preserve_newlines = TRUE
)
} # }
```

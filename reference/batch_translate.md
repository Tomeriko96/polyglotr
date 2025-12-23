# Batch Translation Function

This function translates a file into each target language using the
polyglotr package's translate_file function, and saves the translated
files.

## Usage

``` r
batch_translate(input_file, source_language, target_languages)
```

## Arguments

- input_file:

  A character string indicating the path to the input file.

- source_language:

  A character string indicating the source language.

- target_languages:

  A character vector indicating the target languages.

## Value

Nothing is returned.

## Examples

``` r
if (FALSE) { # \dontrun{
batch_translate("README.md", "nl", c("fr", "es", "de"))
} # }
```

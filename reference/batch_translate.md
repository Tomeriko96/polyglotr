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

A character vector with the paths of the translated files, named by
target language, invisibly.

## Details

One file is written per target language, next to the input file, named
`<name>_<target_language>_translated.<ext>`. The input file is not
modified. The input file and all language codes are checked before
anything is written, so an invalid code does not leave a partial set of
files behind.

## Examples

``` r
if (FALSE) { # \dontrun{
batch_translate("README.md", "nl", c("fr", "es", "de"))
} # }
```

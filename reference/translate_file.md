# Translate File

Translates the content of a file using Google Translate API.

## Usage

``` r
translate_file(
  file_path,
  target_language = "en",
  source_language = "auto",
  overwrite = FALSE
)
```

## Arguments

- file_path:

  The path to the file to be translated.

- target_language:

  The target language to translate the file content to. Default is "en".

- source_language:

  The source language of the file content. Default is "auto".

- overwrite:

  Logical indicating whether to overwrite the original file with the
  translated content. Default is FALSE.

## Examples

``` r
if (FALSE) { # \dontrun{
translate_file("path/to/file.txt", target_language = "fr", source_language = "en", overwrite = TRUE)
} # }
```

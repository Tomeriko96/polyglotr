# Translate File

Translates the content of a file line by line using Google Translate.

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

## Value

The path of the file that was written, invisibly. With
`overwrite = FALSE` this is a new file next to the input named
`<name>_<target_language>_translated.<ext>` (without `.<ext>` if the
input has no extension).

## Details

Leading indentation is preserved and is not sent to the translation
service. Blank lines are kept as they are and are not sent either. Lines
starting with a roxygen comment marker (`#'`) keep the marker and only
the text after it is translated. A line that cannot be translated (for
example because the service is unavailable) is kept in its original
form, and a warning reports how many lines were left untranslated.
Invalid language codes are rejected before the file is read.

## Examples

``` r
if (FALSE) { # \dontrun{
translate_file("path/to/file.txt", target_language = "fr", source_language = "en", overwrite = TRUE)
} # }
```

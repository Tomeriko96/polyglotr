# Google Supported Languages

A snapshot of the language names and ISO-639 codes of languages
supported by Google Translate.
[`google_is_valid_language_code()`](https://tomeriko96.github.io/polyglotr/reference/google_is_valid_language_code.md)
validates language codes against this table and
[`google_get_supported_languages()`](https://tomeriko96.github.io/polyglotr/reference/google_get_supported_languages.md)
returns it.

## Usage

``` r
google_supported_languages
```

## Format

A tibble with two columns:

- Language:

  Language name in English.

- ISO-639 code:

  Language code as used by Google Translate.

## Source

Google Cloud Translation documentation, "Language support"
(<https://cloud.google.com/translate/docs/languages>); snapshot bundled
with polyglotr and last updated before version 1.7.5.

## Details

The snapshot is static and is not refreshed automatically. Google's live
translation service accepts more codes than are listed here, so a code
that is missing from this table may still be translatable by Google, but
will be rejected by polyglotr's validator.

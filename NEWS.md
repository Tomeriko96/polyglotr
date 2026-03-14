# polyglotr 1.8.0 (development)

## Breaking changes / deprecations

* `google_translate_long_text()` is deprecated. `google_translate()` now handles
  long texts automatically via the `chunk_size` parameter (default 1000 chars,
  split at word boundaries). Replace all calls with `google_translate()`.
* `create_translation_table()` is deprecated. Use `create_table()` instead.
* `create_transliteration_table()` is deprecated. Use `create_table()` with a
  custom `fn` argument instead.

## Improvements

* `google_translate()` fixes silent `character(0)` return on texts exceeding
  Google's URL length limit (issue #13). Text is now automatically chunked at
  word boundaries before sending requests.
* `google_translate()` now accepts character vectors of any length (vectorised).
* New `create_table()` function replaces the two deprecated table builders with
  a single, function-agnostic interface.
* `batch_translate()` fixed: was incorrectly calling `google_translate()` on a
  file path instead of `translate_file()`.
* `mymemory_translate()`: fixed URL encoding (was only escaping spaces).
* `pons_translate()`: fixed vectorisation bug — single-string branch was dead
  code due to `is.vector()` always returning `TRUE` for character vectors.
* `translate_to_morse_audio()`: API key now sent as a request header instead of
  a URL query parameter; endpoint upgraded from HTTP to HTTPS.
* `batch_translate()` missing `@export` tag restored.

## Infrastructure

* Migrated all HTTP calls from `httr` to `httr2`; removed dependencies on
  `dplyr`, `httr`, `jsonlite`, `magrittr`, `purrr`, `RCurl`, `rlang`,
  `stringr`, `tibble`, and `urltools`. Package now imports only `httr2` and
  `rvest`.
* Minimum R version bumped to 4.1 (native pipe `|>` support).
* Removed magrittr `%>%` re-export.

# polyglotr 1.7.1
* Changed maintainer email address.

# polyglotr 1.7.0
* Adds Shiny app

# polyglotr 1.6.1
* Fixes language codes in `google_translate()` for Traditional and Simple Chinese

# polyglotr 1.6.0

# polyglotr 1.5.2
* Fixes encoding issue in `google_translate()`

# polyglotr 1.5.1
* Adds specialized function to translate long text objects
* Adds more translation models for wmcloud

# polyglotr 1.5.0
* Adds Pons dictionary method
* Adds FunTranslaion methods for morse code

# polyglotr 1.4.0
* Adds QCRI methods
* Adds Pons methods
* Adds Wikimedia Foundation methods
* Adds Google Transliteration methods

# polyglotr 1.3.1
* fixes testing issue in CRAN checks

# polyglotr 1.3.0
* fixes bug concerning special characters in `google_translate()`
* adds function to retrieve supported languages for Google Translate
* adds function to validate language codes
* adds package dataset for the supported languages in Google Translate

# polyglotr 1.2.2
* add batch_translate

# polyglotr 1.2.1

# polyglotr 1.2.0

* Fixes vectorization issue in google_translate()
* `language_detect()` function to return input language.
* `google_translate_file()` function to translate an entire file.
* Added vignettes.
* Added a `NEWS.md` file to track changes to the package.

# polyglotr 1.1.0

* Published to CRAN

# Changelog

## polyglotr (development version)

### Breaking changes

- [`google_get_supported_languages()`](https://tomeriko96.github.io/polyglotr/reference/google_get_supported_languages.md)
  now returns the bundled `google_supported_languages` snapshot (columns
  `Language` and `ISO-639 code`) instead of scraping the Google Cloud
  documentation. The scrape took the first table on the page, which is
  no longer the language list, so it returned a different set of
  languages with different columns. Getter and
  [`google_is_valid_language_code()`](https://tomeriko96.github.io/polyglotr/reference/google_is_valid_language_code.md)
  now agree by construction.

### Bug fixes

- `translate_file(overwrite = TRUE)` no longer fails with “can only
  write character objects”.
- [`translate_file()`](https://tomeriko96.github.io/polyglotr/reference/translate_file.md)
  no longer silently replaces lines it cannot translate with an empty
  line; the original line is kept and a warning reports how many lines
  were skipped.
- [`translate_file()`](https://tomeriko96.github.io/polyglotr/reference/translate_file.md)
  no longer doubles the indentation of indented lines, and no longer
  sends blank lines to the translation service. A roxygen line that
  fails to translate keeps its text instead of becoming a bare `#'`, and
  whitespace after `#'` (e.g. indented `@examples` code) is preserved.
- [`batch_translate()`](https://tomeriko96.github.io/polyglotr/reference/batch_translate.md)
  now does what its documentation says: it calls
  [`translate_file()`](https://tomeriko96.github.io/polyglotr/reference/translate_file.md)
  once per target language and writes one file each. It is now exported.
  All language codes are validated before any file is written.
- [`translate_file()`](https://tomeriko96.github.io/polyglotr/reference/translate_file.md)
  no longer produces a trailing dot (`README_de_translated.`) for input
  files without an extension.

### Other changes

- [`translate_file()`](https://tomeriko96.github.io/polyglotr/reference/translate_file.md)
  and
  [`batch_translate()`](https://tomeriko96.github.io/polyglotr/reference/batch_translate.md)
  return the path(s) they wrote, invisibly, instead of `NULL`.
- [`translate_file()`](https://tomeriko96.github.io/polyglotr/reference/translate_file.md)
  now warns when lines could not be translated (code run with
  `options(warn = 2)` will stop there) and checks language codes before
  reading the file.
  [`batch_translate()`](https://tomeriko96.github.io/polyglotr/reference/batch_translate.md)
  stops if the input file does not exist.
- Internal: chunking of long texts moved out of
  [`google_translate()`](https://tomeriko96.github.io/polyglotr/reference/google_translate.md)
  into `split_translation_chunks()`;
  [`create_translation_table()`](https://tomeriko96.github.io/polyglotr/reference/create_translation_table.md)
  and
  [`create_transliteration_table()`](https://tomeriko96.github.io/polyglotr/reference/create_transliteration_table.md)
  share one implementation. No behaviour change.
- Removed `RCurl`, `rvest` and `rlang` from Imports; they are no longer
  used. `dplyr` stays:
  [`purrr::map_dfr()`](https://purrr.tidyverse.org/reference/map_dfr.html)
  in
  [`apertium_get_language_pairs()`](https://tomeriko96.github.io/polyglotr/reference/apertium_get_language_pairs.md)
  needs it.
- Documented the source and staleness of the
  `google_supported_languages` dataset.
- Offline tests (mocked with
  [`testthat::local_mocked_bindings()`](https://testthat.r-lib.org/reference/local_mocked_bindings.html))
  for the functions above.

## polyglotr 1.7.5

CRAN release: 2026-09-15

- Fixed
  [`google_translate()`](https://tomeriko96.github.io/polyglotr/reference/google_translate.md)
  after Google’s mobile HTML endpoint began redirecting automated
  requests to an HTTP 429 bot-detection page. Translation now uses the
  JSON endpoint and `dict-chrome-ex` client identifier (issue
  [\#32](https://github.com/Tomeriko96/polyglotr/issues/32)).
- Updated
  [`language_detect()`](https://tomeriko96.github.io/polyglotr/reference/language_detect.md)
  to use the same working client identifier and to parse the detector’s
  structured response instead of scraping the raw array.

## polyglotr 1.7.4

CRAN release: 2026-06-08

- Fixed 301-redirect URL for QCRI in README.md: replaced `qcri.org` with
  `hbku.edu.qa/en/qcri`.
- Removed `linguee_external_sources()`,
  `linguee_translation_examples()`, and `linguee_word_translation()` —
  the upstream API (`linguee-api.fly.dev`) is no longer available.

## polyglotr 1.7.3

- Fixed dead URL in README.md: replaced defunct mt.qcri.org/api/ with
  qcri.org.
- Added weekly lychee link-check CI workflow.

## polyglotr 1.7.2

- All functions that use internet resources now fail gracefully with an
  informative message when the service is unavailable (CRAN policy
  compliance). Network-level errors (DNS failure, connection refused,
  timeout) are caught via
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html) and reported
  via [`message()`](https://rdrr.io/r/base/message.html) rather than
  propagating as errors.
- [`wikipedia_get_language_names()`](https://tomeriko96.github.io/polyglotr/reference/wikipedia_get_language_names.md)
  example wrapped in `\donttest{}`.

## polyglotr 1.7.1

CRAN release: 2026-01-11

- Changed maintainer email address.

## polyglotr 1.7.0

CRAN release: 2025-07-23

- Adds Shiny app

## polyglotr 1.6.1

CRAN release: 2025-07-09

- Fixes language codes in
  [`google_translate()`](https://tomeriko96.github.io/polyglotr/reference/google_translate.md)
  for Traditional and Simple Chinese

## polyglotr 1.6.0

CRAN release: 2025-05-14

## polyglotr 1.5.2

CRAN release: 2024-08-23

- Fixes encoding issue in
  [`google_translate()`](https://tomeriko96.github.io/polyglotr/reference/google_translate.md)

## polyglotr 1.5.1

CRAN release: 2024-07-27

- Adds specialized function to translate long text objects
- Adds more translation models for wmcloud

## polyglotr 1.5.0

CRAN release: 2024-05-03

- Adds Pons dictionary method
- Adds FunTranslaion methods for morse code

## polyglotr 1.4.0

CRAN release: 2024-02-12

- Adds QCRI methods
- Adds Pons methods
- Adds Wikimedia Foundation methods
- Adds Google Transliteration methods

## polyglotr 1.3.1

CRAN release: 2024-01-09

- fixes testing issue in CRAN checks

## polyglotr 1.3.0

CRAN release: 2023-12-06

- fixes bug concerning special characters in
  [`google_translate()`](https://tomeriko96.github.io/polyglotr/reference/google_translate.md)
- adds function to retrieve supported languages for Google Translate
- adds function to validate language codes
- adds package dataset for the supported languages in Google Translate

## polyglotr 1.2.2

CRAN release: 2023-10-30

- add batch_translate

## polyglotr 1.2.1

CRAN release: 2023-08-08

## polyglotr 1.2.0

CRAN release: 2023-07-17

- Fixes vectorization issue in google_translate()
- [`language_detect()`](https://tomeriko96.github.io/polyglotr/reference/language_detect.md)
  function to return input language.
- `google_translate_file()` function to translate an entire file.
- Added vignettes.
- Added a `NEWS.md` file to track changes to the package.

## polyglotr 1.1.0

CRAN release: 2023-06-17

- Published to CRAN

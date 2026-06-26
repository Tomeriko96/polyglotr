# Changelog

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

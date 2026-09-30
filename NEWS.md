# polyglotr (development version)

## Breaking changes
* `google_get_supported_languages()` now returns the bundled
  `google_supported_languages` snapshot (columns `Language` and `ISO-639 code`)
  instead of scraping the Google Cloud documentation. The scrape took the first
  table on the page, which is no longer the language list, so it returned a
  different set of languages with different columns. Getter and
  `google_is_valid_language_code()` now agree by construction.

## Bug fixes
* `translate_file(overwrite = TRUE)` no longer fails with
  "can only write character objects".
* `translate_file()` no longer silently replaces lines it cannot translate with
  an empty line; the original line is kept and a warning reports how many
  lines were skipped.
* `translate_file()` no longer doubles the indentation of indented lines, and no
  longer sends blank lines to the translation service. A roxygen line that
  fails to translate keeps its text instead of becoming a bare `#'`, and
  whitespace after `#'` (e.g. indented `@examples` code) is preserved.
* `batch_translate()` now does what its documentation says: it calls
  `translate_file()` once per target language and writes one file each. It is
  now exported.

## Other changes
* `translate_file()` and `batch_translate()` return the path(s) they wrote,
  invisibly.
* Internal: chunking of long texts moved out of `google_translate()` into
  `split_translation_chunks()`; `create_translation_table()` and
  `create_transliteration_table()` share one implementation. No behaviour
  change.
* Removed `RCurl`, `rvest` and `rlang` from Imports; they are no longer used.
  `dplyr` stays: `purrr::map_dfr()` in `apertium_get_language_pairs()` needs it.
* Documented the source and staleness of the `google_supported_languages`
  dataset.
* Offline tests (mocked with `testthat::local_mocked_bindings()`) for the
  functions above.

# polyglotr 1.7.5
* Fixed `google_translate()` after Google's mobile HTML endpoint began redirecting
  automated requests to an HTTP 429 bot-detection page. Translation now uses the
  JSON endpoint and `dict-chrome-ex` client identifier (issue #32).
* Updated `language_detect()` to use the same working client identifier and to
  parse the detector's structured response instead of scraping the raw array.

# polyglotr 1.7.4
* Fixed 301-redirect URL for QCRI in README.md: replaced `qcri.org` with `hbku.edu.qa/en/qcri`.
* Removed `linguee_external_sources()`, `linguee_translation_examples()`, and `linguee_word_translation()` — the upstream API (`linguee-api.fly.dev`) is no longer available.

# polyglotr 1.7.3
* Fixed dead URL in README.md: replaced defunct mt.qcri.org/api/ with qcri.org.
* Added weekly lychee link-check CI workflow.

# polyglotr 1.7.2
* All functions that use internet resources now fail gracefully with an informative
  message when the service is unavailable (CRAN policy compliance). Network-level
  errors (DNS failure, connection refused, timeout) are caught via `tryCatch()` and
  reported via `message()` rather than propagating as errors.
* `wikipedia_get_language_names()` example wrapped in `\donttest{}`.

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

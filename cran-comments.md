## R CMD check results

0 errors | 0 warnings | 0 notes

## Reason for resubmission

This is a new version (1.7.5) fixing `google_translate()` and
`language_detect()`, which returned errors when Google's mobile HTML
endpoint began redirecting automated requests to a HTTP 429 bot-detection
page (issue #32).

Changes:

1. `google_translate()` now talks to the undocumented JSON endpoint
   `translate.googleapis.com/translate_a/single` using the `dict-chrome-ex`
   client identifier (with a fallback to `at`), instead of scraping the
   retired `translate.google.com/m` HTML page.

2. `language_detect()` uses the same working client identifier and parses the
   structured detection result instead of scraping the raw response array.

3. URL placeholder handling and long-text chunking behaviour are preserved.

Affected functions and their users (`translate_file()`,
`create_translation_table()`, `translate_file()` wrappers) are fixed by the
main change; `google_translate_long_text()` (deprecated) now uses the same
transport.
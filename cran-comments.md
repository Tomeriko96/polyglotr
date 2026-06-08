## R CMD check results

0 errors | 0 warnings | 1 note

The single NOTE is:

  Found the following (possibly) invalid URLs:
    URL: https://github.com/Tomeriko96/polyglotr/actions/workflows/R-CMD-check.yaml
      From: README.md
      Status: 504
      Message: Gateway Timeout

This URL is a CI badge link. It returns HTTP 200 when accessed directly and
is a valid GitHub Actions page. The 504 is a transient timeout from the CRAN
check server reaching GitHub, not a broken URL.

## Reason for resubmission

This release (1.7.4) addresses two issues flagged in the 1.7.3 pre-test:

1. NOTE: URL https://www.qcri.org/ in README.md returned 301. Replaced with
   the final destination https://www.hbku.edu.qa/en/qcri.

2. donttest ERROR (BDR check): linguee_external_sources() crashed because the
   upstream API (linguee-api.fly.dev) is permanently unavailable. Resolved by
   removing all three Linguee functions (linguee_external_sources,
   linguee_translation_examples, linguee_word_translation) and all associated
   documentation, vignette, and Shiny app references.

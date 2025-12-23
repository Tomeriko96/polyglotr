# Restore URLs from placeholders in a translated text

Replaces placeholders like `__URL1__`, `__URL2__`, etc. in a translated
sentence back with their original URLs. Handles both original and
lowercased versions of the placeholder (to account for translation
artifacts).

## Usage

``` r
restore_urls_from_placeholders(translated, urls)
```

## Arguments

- translated:

  A character string where placeholders should be replaced with original
  URLs.

- urls:

  A character vector of the original URLs to restore.

## Value

A character string with the placeholders replaced by the corresponding
URLs.

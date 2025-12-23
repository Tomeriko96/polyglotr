# Replace URLs in a sentence with placeholders

Detects and replaces protocol-style links (e.g., `http://`, `https://`,
`ftp://`) in a given sentence with unique placeholders like `__URL1__`,
`__URL2__`, etc. This is useful for preparing text for translation or
further processing while preserving original URLs.

## Usage

``` r
replace_urls_with_placeholders(sentence)
```

## Arguments

- sentence:

  A character string potentially containing one or more URLs.

## Value

A list with two elements:

- text:

  The input sentence with URLs replaced by placeholders.

- urls:

  A character vector of the extracted URLs.

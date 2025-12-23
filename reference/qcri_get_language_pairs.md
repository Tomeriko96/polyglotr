# QCRI Get Language Pairs

This function retrieves the supported language pairs from the QCRI
Multiterm API.

## Usage

``` r
qcri_get_language_pairs(api_key = qcri_api_key())
```

## Arguments

- api_key:

  The API key associated with the user account being used. If not
  provided, the function will attempt to retrieve it from the
  QCRI_API_KEY environment variable. You can register for an API key at
  https://mt.qcri.org/api/register

## Value

Language pairs.

## Examples

``` r
if (FALSE) { # \dontrun{
qcri_get_language_pairs(api_key = "YourApiKey")
qcri_get_language_pairs()
} # }
```

# QCRI Get Domains

This function retrieves the supported domains from the QCRI Multiterm
API.

## Usage

``` r
qcri_get_domains(api_key = qcri_api_key())
```

## Arguments

- api_key:

  The API key associated with the user account being used. If not
  provided, the function will attempt to retrieve it from the
  QCRI_API_KEY environment variable.

## Value

A list with keys:

- `success`: Boolean indicating whether the request succeeded.

- `domains`: Array of supported domains, such as news, tedtalks etc.
  Only present if success is true.

- `error`: Error message in case success is false.

## Examples

``` r
if (FALSE) { # \dontrun{
qcri_get_domains(api_key = "YourApiKey")
qcri_get_domains()
} # }
```

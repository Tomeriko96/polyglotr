# Launch polyglotr Shiny App

This function launches the Shiny web application for polyglotr,
providing a user-friendly interface to access multiple translation
services.

## Usage

``` r
launch_polyglotr_app(launch.browser = TRUE, port = NULL, host = "127.0.0.1")
```

## Arguments

- launch.browser:

  Logical. Should the app be launched in the default browser? Default is
  TRUE.

- port:

  The port number for the Shiny app. Default is NULL (auto-assign).

- host:

  The host IP address. Default is "127.0.0.1".

## Value

Starts the Shiny application. No return value.

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch the app in default browser
launch_polyglotr_app()

# Launch on specific port
launch_polyglotr_app(port = 3838)

# Launch without opening browser
launch_polyglotr_app(launch.browser = FALSE)
} # }
```

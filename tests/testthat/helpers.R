skip_if_interactive <- function() {
  testthat::skip_if(interactive())
}

skip_if_http_error <- function() {
  response <- try(
    httr::GET(
      "https://translate.googleapis.com/translate_a/single",
      query = list(
        client = "dict-chrome-ex",
        sl = "en",
        tl = "es",
        dt = "t",
        dj = "1",
        q = "hello"
      ),
      httr::timeout(10)
    ),
    silent = TRUE
  )

  testthat::skip_if(
    inherits(response, "try-error") || httr::http_error(response),
    "Google Translate endpoint is unavailable"
  )
}
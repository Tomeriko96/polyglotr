skip_if_interactive <- function() {
  testthat::skip_if(interactive())
}

skip_if_http_error <- function() {
  formatted_text <- stringr::str_replace_all("hello", " ", "%20")

  formatted_link <- paste0(
    "https://translate.google.com/m?tl=",
    "es", "&sl=", "en",
    "&q=",
    formatted_text
  )
  skip_if(httr::http_error(httr::GET(formatted_link)))
}

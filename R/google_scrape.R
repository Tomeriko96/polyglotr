#' Fetch a translation from the Google Translate mobile interface
#'
#' Low-level helper used by \code{\link{google_translate}}. Performs a single
#' HTTP request to the Google Translate mobile page and returns the translated
#' text. The input \code{text} must already be a single, short string (i.e.
#' pre-chunked). URL-encoding is handled internally.
#'
#' @param text A single character string to translate.
#' @param target_language Target language code.
#' @param source_language Source language code.
#'
#' @return A single translated character string.
#' @keywords internal
google_scrape <- function(text, target_language, source_language) {
  encoded <- utils::URLencode(text, repeated = TRUE)
  url <- sprintf(
    "https://translate.google.com/m?tl=%s&sl=%s&q=%s",
    target_language, source_language, encoded
  )

  html_text <- httr2::request(url) |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  result <- rvest::read_html(html_text) |>
    rvest::html_nodes("div.result-container") |>
    rvest::html_text()

  if (length(result) == 0 || nchar(result) == 0) {
    stop(
      "Google Translate returned no result. ",
      "The text may exceed the service character limit or the service may be unavailable."
    )
  }

  utils::URLdecode(gsub("\n", "", result))
}

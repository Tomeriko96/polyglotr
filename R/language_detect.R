#' Detect Language using Google Translate API
#'
#' This function detects the language of a given text using the Google Translate API.
#'
#' @param text The text for which the language needs to be detected.
#' @return A character string representing the detected language.
#' @export
language_detect <- function(text) {
  result <- http_get_json(
    "https://translate.googleapis.com/translate_a/single",
    query = list(client = "gtx", sl = "auto", tl = "en", dt = "t", q = text)
  )
  # The detected source language is the third top-level element of the response
  result[[3]]
}

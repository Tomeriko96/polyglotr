#' Detect the language of a text
#'
#' This function sends a POST request to the Wikimedia Language ID API with the specified text,
#' parses the JSON response, and returns the detected language.
#'
#' @param text The text whose language is to be detected.
#'
#' @return The detected language code.
#' @examples
#' \donttest{
#' wikimedia_detect_language("Hallo, wereld")
#' }
#' @export
wikimedia_detect_language <- function(text) {
  result <- http_post_json(
    "https://api.wikimedia.org/service/lw/inference/v1/models/langid:predict",
    body = list(text = text)
  )
  result$wikicode
}

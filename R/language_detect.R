#' Detect Language using Google Translate API
#'
#' This function detects the language of a given text using the Google Translate API.
#'
#' @param text The text whose language needs to be detected.
#' @return A character string representing the detected language.
#' @export
language_detect <- function(text) {
  response <- safe_http(
    google_api_get(list(
      sl = "auto",
      tl = "en",
      dt = "t",
      dj = "1",
      q = text
    )),
    "Google Translate API"
  )
  if (is.null(response)) return(invisible(NULL))

  content <- httr::content(response, as = "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content, simplifyVector = TRUE)
  language <- result$ld_result$srclangs[1]

  if (is.null(language) || length(language) == 0) {
    message("Language detection failed. Please check your connection and try again.")
    return(invisible(NULL))
  }
  language
}

#' @keywords internal
apertium_request <- function(t, target_language, source_language, host) {
  url <- sprintf(
    "%s/translate?q=%s&langpair=%s|%s",
    host,
    utils::URLencode(t, repeated = TRUE),
    source_language,
    target_language
  )
  result <- httr2::request(url) |>
    httr2::req_error(is_error = \(r) httr2::resp_status(r) >= 400) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)
  result$responseData$translatedText
}

#' Translate text using Apertium
#'
#' @param text A character vector of one or more strings to translate.
#' @param target_language Language code to translate into.
#' @param source_language Language code of the input.
#' @param host Host URL for the Apertium API. Default:
#'   \code{"https://apertium.org/apy"}.
#'
#' @return A character vector of translated strings, the same length as
#'   \code{text}.
#' @export
#'
#' @examples
#' \donttest{
#' apertium_translate("Hello World", target_language = "es", source_language = "en")
#' apertium_translate(c("Hello", "Good morning"), target_language = "es", source_language = "en")
#' }
apertium_translate <- function(text, target_language, source_language,
                                host = "https://apertium.org/apy") {
  vapply(text, apertium_request, character(1),
         target_language, source_language, host,
         USE.NAMES = FALSE)
}

#' @keywords internal
pons_headers <- c(
  "authority"        = "api.pons.com",
  "accept"           = "*/*",
  "cache-control"    = "no-cache",
  "origin"           = "https://en.pons.com",
  "pragma"           = "no-cache",
  "referer"          = "https://en.pons.com/",
  "sec-ch-ua-mobile" = "?0",
  "sec-fetch-dest"   = "empty",
  "sec-fetch-mode"   = "cors",
  "sec-fetch-site"   = "same-site"
)

#' @keywords internal
pons_request <- function(t, target_language, source_language, headers) {
  result <- http_post_json(
    "https://api.pons.com/text-translation-web/v4/translate?locale=en",
    body = list(
      targetLanguage = target_language,
      text = t,
      sourceLanguage = source_language
    ),
    headers = headers
  )
  result$text
}

#' Translate text using PONS
#'
#' @param text A character vector of one or more strings to translate.
#' @param target_language Language code to translate into. Default: \code{"pt"}.
#' @param source_language Language code of the input. Default: \code{"en"}.
#'
#' @return A character vector of translated strings, the same length as
#'   \code{text}.
#' @export
#'
#' @examples
#' \dontrun{
#' pons_translate("I love languages!", target_language = "pt", source_language = "en")
#' pons_translate(c("The", "Greatest", "Language"), target_language = "pt", source_language = "en")
#' }
pons_translate <- function(text, target_language = "pt", source_language = "en") {
  vapply(text, pons_request, character(1),
         target_language, source_language, pons_headers,
         USE.NAMES = FALSE)
}

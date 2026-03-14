#' Translate text using MyMemory
#'
#' @param text A character vector of one or more strings to translate.
#' @param target_language Language code to translate into. Default: \code{"en"}.
#' @param source_language Language code of the input. Default: \code{"auto"}.
#'
#' @return A character vector of translated strings, the same length as
#'   \code{text}.
#' @export
#'
#' @examples
#' \donttest{
#' mymemory_translate("Hello World", target_language = "es", source_language = "en")
#' }
mymemory_request <- function(t, target_language, source_language) {
  url <- sprintf(
    "https://api.mymemory.translated.net/get?q=%s&langpair=%s|%s",
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

mymemory_translate <- function(text, target_language = "en", source_language = "auto") {
  vapply(text, mymemory_request, character(1),
         target_language, source_language,
         USE.NAMES = FALSE)
}

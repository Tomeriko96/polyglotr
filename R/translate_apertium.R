#' Translate text using Apertium
#'
#' @param text Text to translate.
#' @param target_language Language to translate text to.
#' @param source_language Language to translate text from.
#' @param host Host URL for the Apertium API (default is "https://apertium.org/apy").
#'
#' @return Translated text.
#' @export
#'
#' @examples
#' \donttest{
#' translate_apertium("Hello World", target_language = "es", source_language = "en")
#' translate_apertium("Hola mundo", target_language = "en", source_language = "es")
#' }
translate_apertium <- function(text, target_language, source_language, host = "https://apertium.org/apy") {
  formatted_text <- stringr::str_replace_all(text, " ", "%20")

  formatted_link <- paste0(
    host,
    "/translate?q=",
    formatted_text,
    "&langpair=",
    source_language,
    "|",
    target_language
  )

  response <- httr::GET(formatted_link) %>%
    httr::content()

  translation <- response$translatedText

  return(translation)
}
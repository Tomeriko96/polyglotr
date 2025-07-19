#' Translate text using Apertium
#'
#' @param text Text to translate. Can be a single string or a vector of strings.
#' @param target_language Language to translate text to.
#' @param source_language Language to translate text from.
#' @param host Host URL for the Apertium API (default is "https://apertium.org/apy").
#'
#' @return Translated text. Returns a vector if input is a vector.
#' @export
#'
#' @examples
#' \donttest{
#' translate_apertium("Hello World", target_language = "es", source_language = "en")
#' translate_apertium("Hola mundo", target_language = "en", source_language = "es")
#' 
#' # Translate multiple texts
#' texts <- c("Hello", "Good morning", "Thank you")
#' translate_apertium(texts, target_language = "es", source_language = "en")
#' }
translate_apertium <- function(text, target_language, source_language, host = "https://apertium.org/apy") {
  
  # Handle vectorized input
  if (length(text) > 1) {
    return(sapply(text, function(single_text) {
      translate_apertium(single_text, target_language, source_language, host)
    }, USE.NAMES = FALSE))
  }
  
  formatted_text <- urltools::url_encode(text)

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
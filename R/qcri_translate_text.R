#' QCRI Translate Text
#'
#' This function translates a text from the source language to the target language using the QCRI Multiterm API.
#'
#' @param text The text to be translated. This must be URL encoded.
#' @param langpair The source-target language pair, where source is language of the provided text and target is the language into which the text has to be translated.
#' @param domain The domain over which the translation is tuned.
#' @param api_key The API key associated with the user account being used. If not provided, the function will attempt to retrieve it from the QCRI_API_KEY environment variable.
#'
#' @return Translated text.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' qcri_translate_text(text = "Hello, world!",
#' langpair = "en-ar",
#' domain = "general",
#' api_key = "YourApiKey")
#' qcri_translate_text(text = "Hello, world!",
#' langpair = "en-ar",
#' domain = "general")
#' }
qcri_translate_text <- function(text, langpair, domain, api_key = qcri_api_key()) {
  result <- http_get_json(
    "https://mt.qcri.org/api/v1/translate",
    query = list(key = api_key, langpair = langpair, domain = domain, text = text)
  )
  result$translatedText
}

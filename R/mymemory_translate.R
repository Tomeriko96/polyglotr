#' Translate text using mymemory translate
#'
#' @param text Text to translate.
#' @param target_language Language to translate text to.
#' @param source_language Language to translate text from
#'
#' @return Translated text.
#' @export
#'
#' @examples
#'\donttest{
#'mymemory_translate("Hello World", target_language = "es", source_language = "en")
#'}
mymemory_translate <- function(text, target_language = "en", source_language = "auto") {
  formatted_text <- stringr::str_replace_all(text, " ", "%20")

  formatted_link <- paste0("https://api.mymemory.translated.net/get?q=",
                           formatted_text,
                           "&langpair=",
                           source_language,
                           "|",
                           target_language)

  response <- httr::GET(formatted_link) %>%
    httr::content()

  translation <- response$responseData$translatedText

  return(translation)

}

#' Translate text using google translate
#'
#' @param text This is the text that you want to translate.
#' @param target_language This is the language that you want to translate the text into.
#' The default value for this argument is "en" for English.
#' @param source_language This is the language of the text that you want to translate.
#' The default value for this argument is "auto",
#' which means that the function will try to automatically detect the language of the text.
#'
#' @return Translated text.
#' @export
#'
#' @examples
#'\donttest{
#'google_translate("I love languages", target_language = "es")
#'}
google_translate <- function(text, target_language = "en", source_language = "auto") {
  formatted_text <- stringr::str_replace_all(text, " ", "%20")

  formatted_link <- paste0("https://translate.google.com/m?tl=",
                           target_language, "&sl=", source_language,
                           "&q=",
                           formatted_text)

  response <- httr::GET(formatted_link)

  translation <- httr::content(response) %>%
    rvest::html_nodes("div.result-container") %>%
    rvest::html_text()

  translation <- urltools::url_decode(translation)
  translation <- gsub("\n", "", translation)

  return(translation)

}


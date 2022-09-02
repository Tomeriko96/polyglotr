#' Translate text using google translate
#'
#' @param text Text to translate.
#' @param target_language Language to translate text to.
#' @param source_language Language to translate text from
#'
#' @return Translated text.
#' @export
#'
#' @examples
#'\dontrun{
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

  return(translation)

}

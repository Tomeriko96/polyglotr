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
#' \donttest{
#' google_translate("I love languages", target_language = "es")
#' text_to_translate <- c("the", "quick", "brown")
#' google_translate (text_to_translate, "fr", "en")
#' }
google_translate <- function(text, target_language = "en", source_language = "auto") {
  is_vector <- is.vector(text) && length(text) > 1

  formatted_text <- stringr::str_replace_all(text, " ", "%20")

  formatted_link <- paste0(
    "https://translate.google.com/m?tl=",
    target_language, "&sl=", source_language,
    "&q=",
    formatted_text
  )

  if (is_vector) {
    responses <- purrr::map(formatted_link, httr::GET)

    translations <- purrr::map(responses, ~{
      translation <- httr::content(.x) %>%
        rvest::html_nodes("div.result-container") %>%
        rvest::html_text()

      translation <- urltools::url_decode(translation)
      translation <- gsub("\n", "", translation)

      translation
    })

    return(translations)
  } else {
    response <- httr::GET(formatted_link)

    translation <- httr::content(response) %>%
      rvest::html_nodes("div.result-container") %>%
      rvest::html_text()

    translation <- urltools::url_decode(translation)
    translation <- gsub("\n", "", translation)

    return(translation)
  }
}

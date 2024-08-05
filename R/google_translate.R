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
#' google_translate(text_to_translate, "fr", "en")
#' }
google_translate <- function(text, target_language = "en", source_language = "auto") {

  if (!google_is_valid_language_code(target_language)) {
    stop("Invalid target language code.")
  }

  if (!google_is_valid_language_code(source_language)) {
    stop("Invalid source language code.")
  }

  is_vector <- is.vector(text) && length(text) > 1

  formatted_text <- urltools::url_encode(text)

  formatted_link <- paste0(
    "https://translate.google.com/m?tl=",
    target_language, "&sl=", source_language,
    "&q=",
    formatted_text
  )

  if (is_vector) {
    translations <- purrr::map(formatted_link, ~ {
      translation <- rvest::read_html(.x) %>%
        rvest::html_nodes("div.result-container") %>%
        rvest::html_text()

      translation <- urltools::url_decode(translation)
      translation <- gsub("\n", "", translation)

      translation
    })

    return(translations)
  } else {
    translation <- rvest::read_html(formatted_link) %>%
      rvest::html_nodes("div.result-container") %>%
      rvest::html_text()

    translation <- urltools::url_decode(translation)
    translation <- gsub("\n", "", translation)

    return(translation)
  }
}

## helperfunctions/google_translate.R
source("helperfunctions/url_helpers.R")

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
  ## Validate target language code
  if (!google_is_valid_language_code(target_language)) {
    stop("Invalid target language code.")
  }

  ## Validate source language code
  if (!google_is_valid_language_code(source_language)) {
    stop("Invalid source language code.")
  }

  ## Check if input is a vector of strings
  is_vector <- is.vector(text) && length(text) > 1

  if (is_vector) {
    ## Translate each sentence in the vector
    translations <- purrr::map_chr(text, function(t) {
      replaced <- replace_urls_with_placeholders(t)
      encoded <- urltools::url_encode(replaced$text)

      ## Construct the Google Translate mobile URL
      link <- paste0("https://translate.google.com/m?tl=", target_language,
                     "&sl=", source_language, "&q=", encoded)

      ## Scrape the translation from the resulting page
      translated <- rvest::read_html(link) %>%
        rvest::html_nodes("div.result-container") %>%
        rvest::html_text() %>%
        urltools::url_decode() %>%
        gsub("\n", "", .)

      ## Restore URLs and return the final translated string
      restore_urls_from_placeholders(translated, replaced$urls)
    })

    return(translations)

  } else {
    ## Single input string case
    replaced <- replace_urls_with_placeholders(text)
    encoded <- urltools::url_encode(replaced$text)

    ## Create the translation URL
    link <- paste0("https://translate.google.com/m?tl=", target_language,
                   "&sl=", source_language, "&q=", encoded)

    ## Fetch and decode the translated content
    translated <- rvest::read_html(link) %>%
      rvest::html_nodes("div.result-container") %>%
      rvest::html_text() %>%
      urltools::url_decode() %>%
      gsub("\n", "", .)

    ## Return translated sentence with URLs restored
    return(restore_urls_from_placeholders(translated, replaced$urls))
  }
}

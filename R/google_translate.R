#' Translate text using Google Translate
#'
#' Translates input text to a specified language using the Google Translate mobile web interface.
#' Automatically detects and preserves URLs by temporarily replacing them with placeholders.
#'
#' @param text This is the text that you want to translate. Can be a single string or a vector of strings.
#' @param target_language This is the language that you want to translate the text into.
#' The default value is "en" (English).
#' @param source_language This is the language of the text to be translated.
#' The default value is "auto", which attempts automatic language detection.
#'
#' @return A translated string or vector of translated strings, matching the length of the input.
#' @export
#'
#' @examples
#' \donttest{
#' # Translate a simple sentence
#' google_translate("I love languages", target_language = "es")
#'
#' # Translate a vector of words
#' text_to_translate <- c("the", "quick", "brown")
#' google_translate(text_to_translate, "fr", "en")
#'
#' # Translate text containing a URL
#' google_translate("Visit http://example.com for more info.", target_language = "de")
#' }
google_translate <- function(text, target_language = "en", source_language = "auto") {
  . <- NULL
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

    translations

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
    restore_urls_from_placeholders(translated, replaced$urls)
  }
}

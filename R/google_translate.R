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

  replace_urls_with_placeholders <- function(sentence) {
    urls <- stringr::str_extract_all(sentence, "https?://[^\\s]+")[[1]]
    placeholder_text <- sentence
    if (length(urls) > 0) {
      for (i in seq_along(urls)) {
        placeholder <- paste0("__URL", i, "__")
        placeholder_text <- stringr::str_replace(placeholder_text, stringr::fixed(urls[i]), placeholder)

      }
    }
    list(text = placeholder_text, urls = urls)
  }

  restore_urls_from_placeholders <- function(translated, urls) {
    restored_text <- translated
    for (i in seq_along(urls)) {
      placeholder <- paste0("__URL", i, "__")
      restored_text <- stringr::str_replace(restored_text, stringr::fixed(placeholder), urls[i])

      restored_text <- stringr::str_replace(restored_text, stringr::fixed(stringr::str_to_lower(placeholder)), urls[i])
    }
    restored_text
  }

  is_vector <- is.vector(text) && length(text) > 1

  if (is_vector) {
    translations <- purrr::map_chr(text, function(t) {
      replaced <- replace_urls_with_placeholders(t)
      encoded <- urltools::url_encode(replaced$text)
      link <- paste0("https://translate.google.com/m?tl=", target_language,
                     "&sl=", source_language, "&q=", encoded)

      translated <- rvest::read_html(link) %>%
        rvest::html_nodes("div.result-container") %>%
        rvest::html_text() %>%
        urltools::url_decode() %>%
        gsub("\n", "", .)

      restore_urls_from_placeholders(translated, replaced$urls)
    })
    return(translations)
  } else {
    replaced <- replace_urls_with_placeholders(text)
    encoded <- urltools::url_encode(replaced$text)
    link <- paste0("https://translate.google.com/m?tl=", target_language,
                   "&sl=", source_language, "&q=", encoded)

    translated <- rvest::read_html(link) %>%
      rvest::html_nodes("div.result-container") %>%
      rvest::html_text() %>%
      urltools::url_decode() %>%
      gsub("\n", "", .)

    return(restore_urls_from_placeholders(translated, replaced$urls))
  }
}


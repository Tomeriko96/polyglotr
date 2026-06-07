#' Translate text using Google Translate
#'
#' Translates input text to a specified language using the Google Translate mobile web interface.
#' Automatically detects and preserves URLs by temporarily replacing them with placeholders.
#' Long texts are split on word boundaries and translated in chunks, then reassembled.
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

  if (!google_is_valid_language_code(target_language)) {
    stop("Invalid target language code.")
  }
  if (!google_is_valid_language_code(source_language)) {
    stop("Invalid source language code.")
  }

  # Split text into chunks at word boundaries, keeping each chunk under max_chars.
  split_into_chunks <- function(t, max_chars = 1000) {
    words <- strsplit(t, "\\s+")[[1]]
    chunks <- character(0)
    current <- ""
    for (word in words) {
      candidate <- if (nchar(current) == 0) word else paste(current, word)
      if (nchar(candidate) > max_chars && nchar(current) > 0) {
        chunks <- c(chunks, current)
        current <- word
      } else {
        current <- candidate
      }
    }
    if (nchar(current) > 0) chunks <- c(chunks, current)
    chunks
  }

  translate_chunk <- function(t) {
    replaced <- replace_urls_with_placeholders(t)
    encoded <- urltools::url_encode(replaced$text)
    link <- paste0("https://translate.google.com/m?tl=", target_language,
                   "&sl=", source_language, "&q=", encoded)
    result <- safe_http(
      rvest::read_html(link) %>%
        rvest::html_nodes("div.result-container") %>%
        rvest::html_text() %>%
        urltools::url_decode() %>%
        gsub("\n", "", .),
      "Google Translate"
    )
    if (is.null(result) || length(result) == 0) return(NA_character_)
    restore_urls_from_placeholders(result, replaced$urls)
  }

  translate_single <- function(t) {
    if (nchar(t) > 1000) {
      chunks <- split_into_chunks(t)
      parts <- vapply(chunks, translate_chunk, character(1))
      paste(parts[!is.na(parts)], collapse = " ")
    } else {
      translate_chunk(t)
    }
  }

  if (is.vector(text) && length(text) > 1) {
    result <- safe_http(
      purrr::map_chr(text, translate_single),
      "Google Translate"
    )
    if (is.null(result)) return(invisible(NULL))
    result
  } else {
    result <- translate_single(text)
    if (is.null(result) || is.na(result) || nchar(result) == 0) return(invisible(NULL))
    result
  }
}

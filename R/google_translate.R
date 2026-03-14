#' @keywords internal
google_translate_chunked <- function(t, target_language, source_language, chunk_size) {
  replaced <- replace_urls_with_placeholders(t)
  chunks <- split_text_at_words(replaced$text, chunk_size)
  translated_chunks <- vapply(chunks, google_scrape, character(1),
                              target_language, source_language,
                              USE.NAMES = FALSE)
  translated <- paste(translated_chunks, collapse = " ")
  restore_urls_from_placeholders(translated, replaced$urls)
}

#' Translate text using Google Translate
#'
#' Translates one or more strings using the Google Translate mobile web
#' interface. URLs in the text are preserved through the translation. Long
#' texts are automatically split into chunks at word boundaries and reassembled.
#'
#' @param text A character vector of one or more strings to translate.
#' @param target_language Language code to translate into. Default: \code{"en"}.
#' @param source_language Language code of the input text. Default: \code{"auto"}
#'   for automatic detection.
#' @param chunk_size Maximum number of characters per translation request.
#'   Default: \code{1000}. Reduce if translations return empty results.
#'
#' @return A character vector of translated strings, the same length as
#'   \code{text}.
#' @export
#'
#' @examples
#' \donttest{
#' google_translate("I love languages", target_language = "es")
#' google_translate(c("the", "quick", "brown"), target_language = "fr", source_language = "en")
#' google_translate("Visit http://example.com for more info.", target_language = "de")
#' }
google_translate <- function(text, target_language = "en", source_language = "auto",
                              chunk_size = 1000) {
  if (!google_is_valid_language_code(target_language)) {
    stop("Invalid target language code.")
  }
  if (!google_is_valid_language_code(source_language)) {
    stop("Invalid source language code.")
  }
  vapply(text, google_translate_chunked, character(1),
         target_language, source_language, chunk_size,
         USE.NAMES = FALSE)
}

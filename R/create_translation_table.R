#' Create a Translation Table
#'
#' This function generates a translation table by translating a list of words into multiple languages.
#'
#' @param words A character vector containing the words to be translated.
#' @param languages A character vector specifying the target languages for translation.
#' @return A data frame representing the translation table with original words and translations in each language.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' words <- c("Hello", "Translate", "Table", "Script")
#' languages <- c("es", "fr", "de", "nl")
#' translations <- create_translation_table(words, languages)
#' print(translations)
#' }
create_translation_table <- function(words, languages) {
  translation_table(words, languages, function(word, language) {
    google_translate(word, target_language = language)
  })
}

# Build a data frame with one column `original_word` and one column per
# language, filled by calling `fn(word, language)`. `fn` may return NULL
# (service unavailable) or a vector; the first element is used, NULL -> NA.
# Shared by create_translation_table() and create_transliteration_table().
# @noRd
translation_table <- function(words, languages, fn) {
  table <- data.frame(original_word = words)
  for (language in languages) {
    table[[language]] <- vapply(words, function(word) {
      r <- fn(word, language)
      if (is.null(r) || length(r) == 0) NA_character_ else as.character(r[1])
    }, character(1), USE.NAMES = FALSE)
  }
  table
}

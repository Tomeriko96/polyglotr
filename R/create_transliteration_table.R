#' Create a Transliteration Table
#'
#' This function generates a transliteration table by transliterating a list of words into multiple languages.
#'
#' @param words A character vector containing the words to be transliterated.
#' @param languages A character vector specifying the target languages for transliteration.
#' @return A data frame representing the transliteration table with original words and transliterations in each language.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' words <- c("Hello world", "Goodbye", "Thank you", "Please")
#' languages <- c("ar", "he", "el", "ru", "fa")
#' transliterations <- create_transliteration_table(words, languages)
#' print(transliterations)
#' }
create_transliteration_table <- function(words, languages) {
  translation_table(words, languages, function(word, language) {
    google_transliterate(word, language, num = 1)
  })
}

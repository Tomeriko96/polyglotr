#' Create a Translation Table
#'
#' This function generates a translation table by translating a list of words into multiple languages.
#'
#' @param words A character vector containing the words to be translated.
#' @param languages A character vector specifying the target languages for translation.
#' @return A data frame representing the translation table with original words and translations in each language.
#'
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' words <- c("Hello", "Translate", "Table", "Script")
#' languages <- c("es", "fr", "de", "nl")
#' translations <- create_translation_table(words, languages)
#' print(translations)
create_translation_table <- function(words, languages) {
  Original_word <- NULL
  translations <- data.frame(Original_word = words)

  for (language in languages) {
    column_name <- language
    translations <- translations %>%
      dplyr::mutate("{column_name}" := purrr::map_chr(
        Original_word,
        ~ google_translate(., target_language = language)
      ))
  }

  return(translations)
}

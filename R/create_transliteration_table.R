#' Create a Transliteration Table
#'
#' This function generates a transliteration table by transliterating a list of words into multiple languages.
#'
#' @param words A character vector containing the words to be transliterated.
#' @param languages A character vector specifying the target languages for transliteration.
#' @return A data frame representing the transliteration table with original words and transliterations in each language.
#'
#' @importFrom rlang :=
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
  original_word <- NULL
  transliterations <- data.frame(original_word = words)

  for (language in languages) {
    column_name <- language
    transliterations <- transliterations %>%
      dplyr::mutate("{column_name}" := purrr::map_chr(
        original_word,
        ~ google_transliterate(., language, num=1)
      ))
  }

  return(transliterations)
}

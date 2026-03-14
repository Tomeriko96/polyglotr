#' Build a translation or transliteration table
#'
#' Applies a translation or transliteration function to a set of words across
#' multiple target languages and returns the results as a data frame.
#'
#' This is the unified replacement for the deprecated
#' \code{\link{create_translation_table}} and
#' \code{\link{create_transliteration_table}} functions.
#'
#' @param words A character vector of words or phrases to process.
#' @param languages A character vector of target language codes.
#' @param fn A function with the signature \code{fn(text, target_language)}.
#'   Defaults to \code{\link{google_translate}}. Pass
#'   \code{function(text, target_language) google_transliterate(text, target_language, num = 1)}
#'   for transliteration.
#'
#' @return A data frame with an \code{original_word} column followed by one
#'   column per language containing the processed output.
#' @export
#'
#' @examples
#' \dontrun{
#' create_table(c("Hello", "Goodbye"), c("es", "fr"))
#'
#' # Transliteration variant
#' create_table(
#'   c("Hello", "Goodbye"),
#'   c("ar", "el"),
#'   fn = function(text, target_language) {
#'     google_transliterate(text, target_language, num = 1)
#'   }
#' )
#' }
create_table <- function(words, languages, fn = google_translate) {
  result <- data.frame(original_word = words, stringsAsFactors = FALSE)
  for (lang in languages) {
    result[[lang]] <- vapply(
      words, fn, character(1),
      target_language = lang
    )
  }
  result
}

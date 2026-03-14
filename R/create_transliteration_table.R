#' Create a Transliteration Table
#'
#' @description
#' **Deprecated.** Use \code{\link{create_table}} with a custom \code{fn}
#' argument instead.
#'
#' @param words A character vector of words to transliterate.
#' @param languages A character vector of target language codes.
#'
#' @return A data frame with an \code{original_word} column and one column per
#'   language.
#' @export
#'
#' @examples
#' \dontrun{
#' # Deprecated — use create_table() instead:
#' create_table(
#'   c("Hello", "Goodbye"),
#'   c("ar", "el"),
#'   fn = function(text, target_language) {
#'     google_transliterate(text, target_language, num = 1)
#'   }
#' )
#' }
create_transliteration_table <- function(words, languages) {
  .Deprecated(
    "create_table",
    msg = paste(
      "create_transliteration_table() is deprecated.",
      "Use create_table() with fn = function(text, target_language)",
      "google_transliterate(text, target_language, num = 1) instead."
    )
  )
  fn <- function(text, target_language) {
    google_transliterate(text, language_tag = target_language, num = 1)
  }
  create_table(words, languages, fn = fn)
}

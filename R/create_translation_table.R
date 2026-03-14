#' Create a Translation Table
#'
#' @description
#' **Deprecated.** Use \code{\link{create_table}} instead.
#'
#' @param words A character vector of words to translate.
#' @param languages A character vector of target language codes.
#'
#' @return A data frame with an \code{original_word} column and one column per
#'   language.
#' @export
#'
#' @examples
#' \dontrun{
#' # Deprecated — use create_table() instead:
#' create_table(c("Hello", "Goodbye"), c("es", "fr"))
#' }
create_translation_table <- function(words, languages) {
  .Deprecated(
    "create_table",
    msg = "create_translation_table() is deprecated. Use create_table() instead."
  )
  create_table(words, languages, fn = google_translate)
}

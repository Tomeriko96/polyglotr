#' Provide translation examples using the Linguee API
#'
#' @param query The word or phrase to look up.
#' @param source_language Source language code (e.g. \code{"en"}).
#' @param target_language Target language code (e.g. \code{"es"}).
#' @param guess_direction Whether the API should guess translation direction.
#'   Default: \code{FALSE}.
#' @param follow_corrections How to handle "did you mean" responses:
#'   \code{"always"}, \code{"never"}, or \code{"on_empty_translations"}.
#'   Default: \code{"always"}.
#'
#' @return A data frame with columns \code{source}, \code{target}, \code{pos}.
#'
#' @seealso \code{\link{linguee_word_translation}}
#' @export
#'
#' @examples
#' \donttest{
#' linguee_translation_examples(query = "hello", source_language = "en", target_language = "es")
#' }
linguee_translation_examples <- function(query, source_language, target_language,
                                          guess_direction = FALSE,
                                          follow_corrections = "always") {
  translation_examples <- http_get_json(
    "https://linguee-api.fly.dev/api/v2/examples",
    query = list(
      query              = query,
      src                = source_language,
      dst                = target_language,
      guess_direction    = guess_direction,
      follow_corrections = follow_corrections
    )
  )

  do.call(rbind, lapply(translation_examples, function(example) {
    data.frame(
      source = example$text,
      target = example$translations[[1]]$text,
      pos    = example$translations[[1]]$pos,
      stringsAsFactors = FALSE
    )
  }))
}

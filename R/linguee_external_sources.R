#' Retrieve external sources using the Linguee API
#'
#' @param query The word or phrase to look up.
#' @param source_language Source language code (e.g. \code{"en"}).
#' @param target_language Target language code (e.g. \code{"es"}).
#' @param limit Maximum number of sources to return. Default: \code{5}.
#'
#' @return A data frame with columns \code{src}, \code{dst}, \code{src_url},
#'   \code{dst_url}.
#'
#' @seealso \code{\link{linguee_word_translation}},
#'   \code{\link{linguee_translation_examples}}
#' @export
#'
#' @examples
#' \donttest{
#' linguee_external_sources(query = "hello", source_language = "en", target_language = "es")
#' }
linguee_external_sources <- function(query, source_language, target_language, limit = 5) {
  external_sources <- http_get_json(
    "https://linguee-api.fly.dev/api/v2/external_sources",
    query = list(
      query = query,
      src   = source_language,
      dst   = target_language,
      limit = limit
    )
  )

  do.call(rbind, lapply(external_sources, as.data.frame, stringsAsFactors = FALSE))
}

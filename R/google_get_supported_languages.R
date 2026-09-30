#' Get Supported Languages
#'
#' Returns the languages that polyglotr's Google Translate functions accept,
#' i.e. the bundled [google_supported_languages] snapshot. This is the same
#' table [google_is_valid_language_code()] validates against, so a code listed
#' here is always accepted by the validator and vice versa.
#'
#' Up to polyglotr 1.7.5 this function scraped the Google Cloud Translation
#' documentation page and returned its first table. That page now carries
#' several tables and the first one is no longer the language list, so the
#' scrape returned a different set of languages with different columns.
#'
#' @return A tibble with the columns `Language` and `ISO-639 code`.
#' @seealso [google_supported_languages], [google_is_valid_language_code()]
#' @export
#'
#' @examples
#' head(google_get_supported_languages())
google_get_supported_languages <- function() {
  polyglotr::google_supported_languages
}

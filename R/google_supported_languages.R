#' Google Supported Languages
#'
#' A snapshot of the language names and ISO-639 codes of languages supported by
#' Google Translate. [google_is_valid_language_code()] validates language codes
#' against this table and [google_get_supported_languages()] returns it.
#'
#' The snapshot is static and is not refreshed automatically. Google's live
#' translation service accepts more codes than are listed here, so a code that
#' is missing from this table may still be translatable by Google, but will be
#' rejected by polyglotr's validator.
#'
#' @format A tibble with two columns:
#' \describe{
#'   \item{Language}{Language name in English.}
#'   \item{ISO-639 code}{Language code as used by Google Translate.}
#' }
#' @source Google Cloud Translation documentation, "Language support"
#'   (<https://cloud.google.com/translate/docs/languages>); snapshot bundled
#'   with polyglotr and last updated before version 1.7.5.
#' @keywords data
"google_supported_languages"

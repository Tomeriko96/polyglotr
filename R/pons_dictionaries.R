#' Get the list of available dictionaries from PONS API
#'
#' @param language The language of the output (ISO 639-1 - two-letter codes). Supported languages are de, el, en, es, fr, it, pl, pt, ru, sl, tr, zh.
#' @return A list of available dictionaries in the specified language.
#' @export
#' @examples
#' \dontrun{
#' pons_dictionaries(language = "es")
#' }
pons_dictionaries <- function(language = "en") {
  supported_languages <- c("de", "el", "en", "es", "fr", "it", "pl", "pt", "ru", "sl", "tr", "zh")
  if (!(language %in% supported_languages)) {
    stop(
      "Unsupported language code. Must be one of: ",
      paste(supported_languages, collapse = ", ")
    )
  }

  http_get_json(
    "https://api.pons.com/v1/dictionaries",
    query = list(language = language)
  )
}


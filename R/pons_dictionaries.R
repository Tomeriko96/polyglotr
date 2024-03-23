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
  # Check if the language is supported
  supported_languages <- c("de", "el", "en", "es", "fr", "it", "pl", "pt", "ru", "sl", "tr", "zh")
  if (!(language %in% supported_languages)) {
    stop("Unsupported language code. Please use one of the supported languages: ", paste(supported_languages, collapse = ", "))
  }

  # Construct the API URL with the language parameter
  url <- paste0("https://api.pons.com/v1/dictionaries?language=", language)

  # Make the GET request
  response <- httr::GET(url)

  # Check if the request was successful
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve dictionaries. Status code: ", httr::status_code(response))
  }

  # Parse the JSON response
  dictionaries <- jsonlite::fromJSON(httr::content(response, "text"))

  return(dictionaries)
}


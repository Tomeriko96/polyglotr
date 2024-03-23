#' Get the set of languages currently supported by the Microsoft Translator API
#'
#' @param scope (optional) A comma-separated list of names defining the group of languages to return. Allowed group names are: translation, transliteration, and dictionary. If no scope is given, then all groups are returned.
#' @return A list of supported languages for the specified groups.
#' @export
#' @examples
#' \dontrun{
#' microsoft_supported_languages(scope = "translation,transliteration,dictionary")
#' }
microsoft_supported_languages <- function(scope = NULL) {
  # Construct the API URL with the required parameters
  url <- "https://api.cognitive.microsofttranslator.com/languages?api-version=3.0"

  # Add the scope parameter to the URL if provided
  if (!is.null(scope)) {
    url <- paste0(url, "&scope=", scope)
  }

  # Make the GET request
  response <- httr::GET(url)

  # Check if the request was successful
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve supported languages. Status code: ", httr::status_code(response))
  }

  # Parse the JSON response
  supported_languages <- jsonlite::fromJSON(httr::content(response, "text"))

  return(supported_languages)
}

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
  query <- list(`api-version` = "3.0")
  if (!is.null(scope)) query$scope <- scope
  http_get_json("https://api.cognitive.microsofttranslator.com/languages", query = query)
}

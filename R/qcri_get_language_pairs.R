#' QCRI Get Language Pairs
#'
#' This function retrieves the supported language pairs from the QCRI Multiterm API.
#'
#' @param api_key The API key associated with the user account being used. If not provided, the function will attempt to retrieve it from the QCRI_API_KEY environment variable. You can register for an API key at https://mt.qcri.org/api/register
#'
#' @return Language pairs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' qcri_get_language_pairs(api_key = "YourApiKey")
#' qcri_get_language_pairs()
#' }
qcri_get_language_pairs <- function(api_key = qcri_api_key()) {
  http_get_json("https://mt.qcri.org/api/v1/getLanguagePairs", query = list(key = api_key))
}

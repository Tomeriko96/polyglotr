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
  # Get the API key
  if (is.null(api_key)) {
    api_key <- qcri_api_key()
  }

  # Set up the URL parameters
  url_params <- list(key = api_key)

  # Make the request
  response <- httr::GET(
    url = "https://mt.qcri.org/api/v1/getLanguagePairs",
    query = url_params
  )

  # Parse the response
  content <- httr::content(response, "parsed")

  # Return the response
  return(content)
}

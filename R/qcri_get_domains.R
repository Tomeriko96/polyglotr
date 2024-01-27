#' QCRI Get Domains
#'
#' This function retrieves the supported domains from the QCRI Multiterm API.
#'
#' @param api_key The API key associated with the user account being used. If not provided, the function will attempt to retrieve it from the QCRI_API_KEY environment variable.
#'
#' @return A list with keys:
#' - `success`: Boolean indicating whether the request succeeded.
#' - `domains`: Array of supported domains, such as news, tedtalks etc. Only present if success is true.
#' - `error`: Error message in case success is false.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' qcri_get_domains(api_key = "YourApiKey")
#' qcri_get_domains()
#' }
qcri_get_domains <- function(api_key = qcri_api_key()) {
  # Get the API key
  if (is.null(api_key)) {
    api_key <- qcri_api_key()
  }

  # Set up the URL parameters
  url_params <- list(key = api_key)

  # Make the request
  response <- httr::GET(
    url = "https://mt.qcri.org/api/v1/getDomains",
    query = url_params
  )

  # Check the status code
  if (httr::status_code(response) >= 400) {
    stop("Request failed with status code ", httr::status_code(response))
  }

  # Parse the response
  content <- httr::content(response, "parsed")

  # Return the response
  return(content)
}

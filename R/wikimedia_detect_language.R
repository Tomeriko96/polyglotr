#' Detect the language of a text
#'
#' This function sends a POST request to the Wikimedia Language ID API with the specified text,
#' parses the JSON response, and returns the detected language.
#'
#' @param text The text whose language is to be detected.
#'
#' @return The detected language.
#'
#' @examples
#' # Detect the language of a text
#' wikimedia_detect_language("Hallo, wereld")
#'
#' @export
wikimedia_detect_language <- function(text) {
  # Define the URL of the API
  url <- "https://api.wikimedia.org/service/lw/inference/v1/models/langid:predict"

  # Create a list of parameters to send in the POST request
  body <- list(
    text = text
  )

  # Convert the list to a JSON string
  json_body <- jsonlite::toJSON(body, auto_unbox = TRUE)

  # Set the content type of the request to 'application/json'
  headers <- c("Content-Type" = "application/json")

  # Send the POST request and get the response
  response <- httr::POST(url, body = json_body, httr::add_headers(headers))

  # Check if the request was successful
  if (httr::status_code(response) != 200) {
    stop("Request failed with status ", httr::status_code(response))
  }

  # Parse the response
  result <- httr::content(response, "parsed")

  # Extract the detected language from the response
  language <- result$wikicode

  return(language)
}

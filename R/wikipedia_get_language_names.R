#' Get language names
#'
#' This function sends a GET request to the Wikipedia API and returns the language names as a dataframe.
#'
#' @return A dataframe of language names.
#'
#' @examples
#' # Get language names
#' wikipedia_get_language_names()
#'
#' @export
wikipedia_get_language_names <- function() {
  # Define the URL of the API
  url <- "https://en.wikipedia.org/w/api.php?action=query&liprop=autonym|name&meta=languageinfo&uselang=en&format=json&origin=*"

  # Send the GET request and get the response
  response <- httr::GET(url)

  # Check if the request was successful
  if (httr::status_code(response) != 200) {
    stop("Request failed with status ", httr::status_code(response))
  }

  # Parse the response
  result <- httr::content(response, "parsed")

  # Extract the language names from the response
  language_names <- result$query$languageinfo

  # Get the keys of the language_names list
  language_tags <- names(language_names)

  # Combine the language_tags, names, and autonyms into a dataframe
  df <- data.frame(
    language_tag = language_tags,
    name = sapply(language_names, function(x) x$name),
    autonym = sapply(language_names, function(x) x$autonym),
    row.names = NULL
  )

  return(df)
}

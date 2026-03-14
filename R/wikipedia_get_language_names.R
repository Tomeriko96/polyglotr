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
  result <- http_get_json(
    "https://en.wikipedia.org/w/api.php",
    query = list(
      action  = "query",
      liprop  = "autonym|name",
      meta    = "languageinfo",
      uselang = "en",
      format  = "json",
      origin  = "*"
    )
  )

  language_names <- result$query$languageinfo
  language_tags  <- names(language_names)

  data.frame(
    language_tag = language_tags,
    name         = vapply(language_names, function(x) x$name,   character(1)),
    autonym      = vapply(language_names, function(x) x$autonym, character(1)),
    row.names    = NULL,
    stringsAsFactors = FALSE
  )
}

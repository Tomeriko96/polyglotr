#' Check if a language code is valid
#'
#' This function checks if a given language code is in the `google_supported_languages` dataset.
#'
#' @param language_code The language code to check.
#' @return A logical value indicating if the language code is valid.
#' @export
#'
#' @examples
#' \dontrun{
#' google_is_valid_language_code("en") # TRUE
#' google_is_valid_language_code("fr") # TRUE
#' google_is_valid_language_code("xx") # FALSE
#' }
google_is_valid_language_code <- function(language_code) {
  if (language_code == "auto") {
    return(TRUE)
  }

  if (language_code %in% polyglotr::google_supported_languages$`ISO-639 code`) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

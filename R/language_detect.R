#' Detect Language using Google Translate API
#'
#' This function detects the language of a given text using the Google Translate API.
#'
#' @param text The text for which the language needs to be detected.
#' @return A character string representing the detected language.
#' @export
language_detect <- function(text) {
  url <- "https://translate.googleapis.com/translate_a/single"
  params <- list(
    client = "gtx",
    sl = "auto",
    tl = "en",
    dt = "t",
    q = text
  )

  response <- safe_http(httr::GET(url, query = params), "Google Translate API")
  if (is.null(response)) return(invisible(NULL))

  if (httr::status_code(response) == 200) {
    result <- httr::content(response, "parsed")
    language <- purrr::keep(result, is.character) %>% as.character()
    return(language)
  } else {
    message("Language detection failed. Please check your connection and try again.")
    return(invisible(NULL))
  }
}

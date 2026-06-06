#' Translate Text to Morse Code using the FunTranslations API
#'
#' This function takes a string of text as input and translates it to Morse code using the FunTranslations API.
#'
#' @param text A character string containing the text to be translated to Morse code.
#' @param api_key (optional) Your FunTranslations API key, if you have a paid subscription.
#' @return A list containing the translated Morse code text and other metadata.
#' @export
translate_to_morse <- function(text, api_key = NULL) {
  url <- "https://api.funtranslations.com/translate/morse.json"
  payload <- list(text = text)

  # Add API key if provided
  if (!is.null(api_key)) {
    headers <- c("X-FunTranslations-Api-Secret" = api_key)
  } else {
    headers <- NULL
  }

  # Make the API request
  response <- safe_http(httr::POST(url, body = payload, httr::add_headers(headers)), "FunTranslations API")
  if (is.null(response)) return(invisible(NULL))

  # Check the response status
  if (httr::status_code(response) == 200) {
    result <- jsonlite::fromJSON(httr::content(response, "text"))
    return(list(
      morse_code = result$contents$translated,
      text_translated = result$contents$text,
      translation_type = result$contents$translation
    ))
  } else {
    message("FunTranslations API request failed with status ", httr::status_code(response))
    return(invisible(NULL))
  }
}

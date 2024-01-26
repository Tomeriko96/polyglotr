#' Translate text using PONS
#'
#' @param text This is the text that you want to translate. Can be a single string or a vector of strings.
#' @param target_language This is the language that you want to translate the text into.
#' The default value for this argument is "pt" for Portuguese.
#' @param source_language This is the language of the text that you want to translate.
#' The default value for this argument is "en" for English.
#'
#' @return Translated text. If the input is a vector, it returns a character vector of translated strings.
#' @export
#'
#' @examples
#' \dontrun{
#' pons_translate("I love languages!", target_language = "pt", source_language = "en")
#' text_to_translate <- c("The", "Greatest", "Language")
#' pons_translate(text_to_translate, "pt", "en")
#' }
pons_translate <- function(text, target_language = "pt", source_language = "en") {
  # Create the data payload and headers once
  payload <- list(
    targetLanguage = target_language,
    text = text,
    sourceLanguage = source_language
  )

  headers <- httr::add_headers(
    `authority` = "api.pons.com",
    `accept` = "*/*",
    `cache-control` = "no-cache",
    `content-type` = "application/json",
    `origin` = "https://en.pons.com",
    `pragma` = "no-cache",
    `referer` = "https://en.pons.com/",
    `sec-ch-ua-mobile` = "?0",
    `sec-fetch-dest` = "empty",
    `sec-fetch-mode` = "cors",
    `sec-fetch-site` = "same-site"
  )

  # Check if the input is a vector
  if (is.vector(text)) {
    # If it is, translate each string separately
    purrr::map_chr(text, function(single_text) {
      # Update the payload with the current single text
      payload$text <- single_text
      # Make the request
      response <- httr::POST(
        url = "https://api.pons.com/text-translation-web/v4/translate?locale=en",
        body = payload,
        encode = "json",
        headers = headers
      )
      # Parse the response
      content <- httr::content(response, "parsed")
      return(content$text)
    })
  } else {
    # If it's not a vector, translate the single string
    response <- httr::POST(
      url = "https://api.pons.com/text-translation-web/v4/translate?locale=en",
      body = payload,
      encode = "json",
      headers = headers
    )
    # Parse the response
    content <- httr::content(response, "parsed")
    return(content$text)
  }
}

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

  pons_url <- "https://api.pons.com/text-translation-web/v4/translate?locale=en"

  # Check if the input is a vector
  if (length(text) > 1) {
    result <- safe_http(
      purrr::map_chr(text, function(single_text) {
        payload$text <- single_text
        response <- httr::POST(url = pons_url, body = payload, encode = "json", headers = headers)
        httr::content(response, "parsed")$text
      }),
      "PONS API"
    )
    return(result)
  } else {
    response <- safe_http(
      httr::POST(url = pons_url, body = payload, encode = "json", headers = headers),
      "PONS API"
    )
    if (is.null(response)) return(invisible(NULL))
    return(httr::content(response, "parsed")$text)
  }
}

#' Translate Text to Morse Code using the FunTranslations API
#'
#' This function takes a string of text as input and translates it to Morse code using the FunTranslations API.
#'
#' @param text A character string containing the text to be translated to Morse code.
#' @param api_key (optional) Your FunTranslations API key, if you have a paid subscription.
#' @return A list containing the translated Morse code text and other metadata.
#' @export
translate_to_morse <- function(text, api_key = NULL) {
  headers <- if (!is.null(api_key)) c("X-FunTranslations-Api-Secret" = api_key) else character()

  result <- http_post_json(
    "https://api.funtranslations.com/translate/morse.json",
    body = list(text = text),
    headers = headers
  )

  list(
    morse_code       = result$contents$translated,
    text_translated  = result$contents$text,
    translation_type = result$contents$translation
  )
}

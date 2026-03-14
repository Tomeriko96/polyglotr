#' Translate English Text to Morse Code with Audio
#'
#' This function takes an English text string as input and translates it to Morse code with an audio output using the FunTranslations API.
#'
#' @param text A character string containing the English text to be translated.
#' @param api_key (optional) Your FunTranslations API key, if you have a paid subscription.
#' @return A list containing the translated Morse code text, the Morse code audio as a base64-encoded string, and other metadata.
#' @export
translate_to_morse_audio <- function(text, api_key = NULL) {
  headers <- if (!is.null(api_key)) {
    c("X-FunTranslations-Api-Secret" = api_key)
  } else {
    character()
  }

  result <- http_get_json(
    "https://api.funtranslations.com/translate/morse/audio.json",
    query   = list(text = text),
    headers = headers
  )

  list(
    morse_code_text  = result$contents$text,
    morse_code_audio = result$contents$translated$audio,
    speed            = result$contents$speed,
    tone             = result$contents$tone,
    translation_info = result$contents$translation
  )
}

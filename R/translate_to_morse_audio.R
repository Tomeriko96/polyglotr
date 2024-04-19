#' Translate English Text to Morse Code with Audio
#'
#' This function takes an English text string as input and translates it to Morse code with an audio output using the FunTranslations API.
#'
#' @param text A character string containing the English text to be translated.
#' @param api_key (optional) Your FunTranslations API key, if you have a paid subscription.
#' @return A list containing the translated Morse code text, the Morse code audio as a base64-encoded string, and other metadata.
#' @export
translate_to_morse_audio <- function(text, api_key = NULL) {
  # Construct the API endpoint URL
  url <- paste0("http://api.funtranslations.com/translate/morse/audio.json?text=",
                utils::URLencode(text))

  # Add API key if provided
  if (!is.null(api_key)) {
    url <- paste0(url, "&X-FunTranslations-Api-Secret=", api_key)
  }

  # Make the API request
  response <- httr::GET(url)

  # Check if the request was successful
  if (httr::status_code(response) == 200) {
    # Parse the JSON response
    result <- jsonlite::fromJSON(httr::content(response, "text"))

    # Extract the relevant information
    morse_code_text <- result$contents$text
    morse_code_audio <- result$contents$translated$audio
    speed_output <- result$contents$speed
    tone_output <- result$contents$tone
    translation_info <- result$contents$translation

    # Return the results as a list
    return(list(
      morse_code_text = morse_code_text,
      morse_code_audio = morse_code_audio,
      speed = speed_output,
      tone = tone_output,
      translation_info = translation_info
    ))
  } else {
    # Handle error cases
    stop(paste("Error:", httr::status_code(response), httr::content(response, "text")))
  }
}

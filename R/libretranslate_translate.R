#' Translate text using LibreTranslate
#'
#' Translates input text to a specified language using LibreTranslate's free API.
#' This function uses the public LibreTranslate instance hosted at libretranslate.com.
#'
#' @param text This is the text that you want to translate. Can be a single string or a vector of strings.
#' @param target_language This is the language that you want to translate the text into.
#' The default value is "en" (English).
#' @param source_language This is the language of the text to be translated.
#' The default value is "auto", which attempts automatic language detection.
#'
#' @return A translated string or vector of translated strings, matching the length of the input.
#' @export
#'
#' @examples
#' \donttest{
#' # Translate a simple sentence
#' libretranslate_translate("Hello world", target_language = "es")
#'
#' # Translate a vector of words
#' text_to_translate <- c("the", "quick", "brown")
#' libretranslate_translate(text_to_translate, "fr", "en")
#' }
libretranslate_translate <- function(text, target_language = "en", source_language = "auto") {
  
  # Check if input is a vector of strings
  is_vector <- is.vector(text) && length(text) > 1
  
  if (is_vector) {
    # Translate each sentence in the vector
    translations <- purrr::map_chr(text, function(single_text) {
      libretranslate_translate_single(single_text, target_language, source_language)
    })
    return(translations)
  } else {
    # Single input string case
    return(libretranslate_translate_single(text, target_language, source_language))
  }
}

#' Helper function to translate a single text using LibreTranslate
#'
#' @param text Single text string to translate
#' @param target_language Target language code
#' @param source_language Source language code
#'
#' @return Translated text
#' @keywords internal
libretranslate_translate_single <- function(text, target_language, source_language) {
  # Prepare the request body
  body <- list(
    q = text,
    source = source_language,
    target = target_language
  )
  
  # Set headers
  headers <- httr::add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
    `Accept` = "application/json",
    `Content-Type` = "application/json"
  )
  
  tryCatch({
    # Make the request to LibreTranslate's API endpoint
    response <- httr::POST(
      "https://libretranslate.com/translate",
      body = body,
      encode = "json",
      headers
    )
    
    # Check if request was successful
    if (httr::http_error(response)) {
      warning("LibreTranslate API request failed with status: ", httr::status_code(response))
      return(text)  # Return original text if translation fails
    }
    
    # Parse the response
    content <- httr::content(response, "parsed")
    
    # Extract translation from response
    if (is.list(content) && !is.null(content$translatedText)) {
      translation <- content$translatedText
      return(translation)
    } else {
      warning("Unexpected response format from LibreTranslate")
      return(text)
    }
    
  }, error = function(e) {
    warning("Error in LibreTranslate: ", e$message)
    return(text)
  })
}
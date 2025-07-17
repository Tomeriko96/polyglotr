#' Translate text using Reverso Context
#'
#' Translates input text to a specified language using Reverso Context's free web interface.
#' This function scrapes the web interface without requiring API keys.
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
#' reverso_translate("Hello world", target_language = "es")
#'
#' # Translate a vector of words
#' text_to_translate <- c("the", "quick", "brown")
#' reverso_translate(text_to_translate, "fr", "en")
#' }
reverso_translate <- function(text, target_language = "en", source_language = "auto") {
  
  # Check if input is a vector of strings
  is_vector <- is.vector(text) && length(text) > 1
  
  if (is_vector) {
    # Translate each sentence in the vector
    translations <- purrr::map_chr(text, function(single_text) {
      reverso_translate_single(single_text, target_language, source_language)
    })
    return(translations)
  } else {
    # Single input string case
    return(reverso_translate_single(text, target_language, source_language))
  }
}

#' Helper function to translate a single text using Reverso Context
#'
#' @param text Single text string to translate
#' @param target_language Target language code
#' @param source_language Source language code
#'
#' @return Translated text
#' @keywords internal
reverso_translate_single <- function(text, target_language, source_language) {
  # Map common language codes to Reverso's format
  lang_map <- list(
    "en" = "english",
    "fr" = "french",
    "es" = "spanish",
    "de" = "german",
    "it" = "italian",
    "pt" = "portuguese",
    "ru" = "russian",
    "ar" = "arabic",
    "nl" = "dutch",
    "pl" = "polish",
    "tr" = "turkish",
    "ja" = "japanese",
    "ko" = "korean",
    "zh" = "chinese",
    "he" = "hebrew"
  )
  
  # Convert language codes to Reverso format
  target_lang <- if (is.null(lang_map[[target_language]])) target_language else lang_map[[target_language]]
  source_lang <- if (source_language == "auto") "auto" else (if (is.null(lang_map[[source_language]])) source_language else lang_map[[source_language]])
  
  # Prepare the request body
  body <- list(
    input = text,
    from = source_lang,
    to = target_lang,
    format = "text"
  )
  
  # Set headers to mimic browser request
  headers <- httr::add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
    `Accept` = "application/json",
    `Content-Type` = "application/json",
    `Origin` = "https://context.reverso.net",
    `Referer` = "https://context.reverso.net/translation/"
  )
  
  tryCatch({
    # Make the request to Reverso's API endpoint
    response <- httr::POST(
      "https://api.reverso.net/translate/v1/translation",
      body = body,
      encode = "json",
      headers
    )
    
    # Check if request was successful
    if (httr::http_error(response)) {
      warning("Reverso API request failed with status: ", httr::status_code(response))
      return(text)  # Return original text if translation fails
    }
    
    # Parse the response
    content <- httr::content(response, "parsed")
    
    # Extract translation from response
    if (is.list(content) && !is.null(content$translation) && length(content$translation) > 0) {
      translation <- content$translation[[1]]
      return(translation)
    } else {
      warning("Unexpected response format from Reverso")
      return(text)
    }
    
  }, error = function(e) {
    warning("Error in Reverso translate: ", e$message)
    return(text)
  })
}
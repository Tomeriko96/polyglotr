#' Translate text using Microsoft Translator
#'
#' Translates input text to a specified language using Microsoft Translator's free web interface.
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
#' microsoft_translate("Hello world", target_language = "es")
#'
#' # Translate a vector of words
#' text_to_translate <- c("the", "quick", "brown")
#' microsoft_translate(text_to_translate, "fr", "en")
#' }
microsoft_translate <- function(text, target_language = "en", source_language = "auto") {
  
  # Check if input is a vector of strings
  is_vector <- is.vector(text) && length(text) > 1
  
  if (is_vector) {
    # Translate each sentence in the vector
    translations <- purrr::map_chr(text, function(single_text) {
      microsoft_translate_single(single_text, target_language, source_language)
    })
    return(translations)
  } else {
    # Single input string case
    return(microsoft_translate_single(text, target_language, source_language))
  }
}

#' Helper function to translate a single text using Microsoft Translator
#'
#' @param text Single text string to translate
#' @param target_language Target language code
#' @param source_language Source language code
#'
#' @return Translated text
#' @keywords internal
microsoft_translate_single <- function(text, target_language, source_language) {
  # URL encode the text
  encoded_text <- urltools::url_encode(text)
  
  # Handle auto-detection
  from_lang <- if (source_language == "auto") "auto-detect" else source_language
  
  # Construct the Microsoft Translator URL
  url <- paste0(
    "https://www.bing.com/translator/api/translate/v1?",
    "text=", encoded_text,
    "&from=", from_lang,
    "&to=", target_language
  )
  
  # Set headers to mimic browser request
  headers <- httr::add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
    `Accept` = "application/json",
    `Accept-Language` = "en-US,en;q=0.9",
    `Referer` = "https://www.bing.com/translator/"
  )
  
  tryCatch({
    # Make the request
    response <- httr::GET(url, headers)
    
    # Check if request was successful
    if (httr::http_error(response)) {
      warning("Microsoft Translator API request failed with status: ", httr::status_code(response))
      return(text)  # Return original text if translation fails
    }
    
    # Parse the response
    content <- httr::content(response, "parsed")
    
    # Extract translation from response
    if (is.list(content) && length(content) > 0 && !is.null(content[[1]]$translations)) {
      translation <- content[[1]]$translations[[1]]$text
      return(translation)
    } else {
      warning("Unexpected response format from Microsoft Translator")
      return(text)
    }
    
  }, error = function(e) {
    warning("Error in Microsoft Translator: ", e$message)
    return(text)
  })
}
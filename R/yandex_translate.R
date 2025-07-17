#' Translate text using Yandex Translate
#'
#' Translates input text to a specified language using Yandex Translate's free web interface.
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
#' yandex_translate("Hello world", target_language = "es")
#'
#' # Translate a vector of words
#' text_to_translate <- c("the", "quick", "brown")
#' yandex_translate(text_to_translate, "fr", "en")
#' }
yandex_translate <- function(text, target_language = "en", source_language = "auto") {
  
  # Check if input is a vector of strings
  is_vector <- is.vector(text) && length(text) > 1
  
  if (is_vector) {
    # Translate each sentence in the vector
    translations <- purrr::map_chr(text, function(single_text) {
      yandex_translate_single(single_text, target_language, source_language)
    })
    return(translations)
  } else {
    # Single input string case
    return(yandex_translate_single(text, target_language, source_language))
  }
}

#' Helper function to translate a single text using Yandex Translate
#'
#' @param text Single text string to translate
#' @param target_language Target language code
#' @param source_language Source language code
#'
#' @return Translated text
#' @keywords internal
yandex_translate_single <- function(text, target_language, source_language) {
  # URL encode the text
  encoded_text <- urltools::url_encode(text)
  
  # Construct the Yandex Translate URL
  url <- paste0(
    "https://translate.yandex.com/api/v1/tr.json/translate?",
    "text=", encoded_text,
    "&lang=", if (source_language == "auto") target_language else paste0(source_language, "-", target_language)
  )
  
  # Set headers to mimic browser request
  headers <- httr::add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
    `Accept` = "application/json",
    `Accept-Language` = "en-US,en;q=0.9",
    `Referer` = "https://translate.yandex.com/"
  )
  
  tryCatch({
    # Make the request
    response <- httr::GET(url, headers)
    
    # Check if request was successful
    if (httr::http_error(response)) {
      warning("Yandex Translate API request failed with status: ", httr::status_code(response))
      return(text)  # Return original text if translation fails
    }
    
    # Parse the response
    content <- httr::content(response, "parsed")
    
    # Extract translation from response
    if (is.list(content) && !is.null(content$text) && length(content$text) > 0) {
      translation <- content$text[[1]]
      return(translation)
    } else {
      warning("Unexpected response format from Yandex Translate")
      return(text)
    }
    
  }, error = function(e) {
    warning("Error in Yandex Translate: ", e$message)
    return(text)
  })
}
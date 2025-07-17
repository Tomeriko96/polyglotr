#' Translate text using Glosbe
#'
#' Translates input text to a specified language using Glosbe's free API.
#' This function uses Glosbe's dictionary and translation services.
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
#' glosbe_translate("Hello world", target_language = "es")
#'
#' # Translate a vector of words
#' text_to_translate <- c("the", "quick", "brown")
#' glosbe_translate(text_to_translate, "fr", "en")
#' }
glosbe_translate <- function(text, target_language = "en", source_language = "auto") {
  
  # Check if input is a vector of strings
  is_vector <- is.vector(text) && length(text) > 1
  
  if (is_vector) {
    # Translate each sentence in the vector
    translations <- purrr::map_chr(text, function(single_text) {
      glosbe_translate_single(single_text, target_language, source_language)
    })
    return(translations)
  } else {
    # Single input string case
    return(glosbe_translate_single(text, target_language, source_language))
  }
}

#' Helper function to translate a single text using Glosbe
#'
#' @param text Single text string to translate
#' @param target_language Target language code
#' @param source_language Source language code
#'
#' @return Translated text
#' @keywords internal
glosbe_translate_single <- function(text, target_language, source_language) {
  # For auto-detection, we'll use English as default source
  if (source_language == "auto") {
    source_language <- "en"
  }
  
  # URL encode the text
  encoded_text <- urltools::url_encode(text)
  
  # Construct the Glosbe API URL
  url <- paste0(
    "https://glosbe.com/gapi/translate?",
    "from=", source_language,
    "&dest=", target_language,
    "&format=json",
    "&phrase=", encoded_text
  )
  
  # Set headers
  headers <- httr::add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
    `Accept` = "application/json",
    `Accept-Language` = "en-US,en;q=0.9"
  )
  
  tryCatch({
    # Make the request to Glosbe's API endpoint
    response <- httr::GET(url, headers)
    
    # Check if request was successful
    if (httr::http_error(response)) {
      warning("Glosbe API request failed with status: ", httr::status_code(response))
      return(text)  # Return original text if translation fails
    }
    
    # Parse the response
    content <- httr::content(response, "parsed")
    
    # Extract translation from response
    if (is.list(content) && !is.null(content$tuc) && length(content$tuc) > 0) {
      # Try to get the first translation
      first_translation <- content$tuc[[1]]
      
      if (!is.null(first_translation$phrase) && !is.null(first_translation$phrase$text)) {
        translation <- first_translation$phrase$text
        return(translation)
      } else if (!is.null(first_translation$meanings) && length(first_translation$meanings) > 0) {
        # If no direct translation, try to get meaning
        meaning <- first_translation$meanings[[1]]$text
        return(meaning)
      } else {
        warning("No translation found in Glosbe response")
        return(text)
      }
    } else {
      warning("Unexpected response format from Glosbe")
      return(text)
    }
    
  }, error = function(e) {
    warning("Error in Glosbe translate: ", e$message)
    return(text)
  })
}
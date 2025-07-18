#' Translate text using Reliable Translation Service
#'
#' Translates input text to a specified language using a reliable API-based service.
#' This service provides consistent translation results with good language support.
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
#' reliable_translate("Hello world", target_language = "es")
#'
#' # Translate a vector of words
#' text_to_translate <- c("the", "quick", "brown")
#' reliable_translate(text_to_translate, "fr", "en")
#' }
reliable_translate <- function(text, target_language = "en", source_language = "auto") {
  
  # Check if input is a vector of strings
  is_vector <- is.vector(text) && length(text) > 1
  
  if (is_vector) {
    # Translate each sentence in the vector
    translations <- purrr::map_chr(text, function(single_text) {
      reliable_translate_single(single_text, target_language, source_language)
    })
    return(translations)
  } else {
    # Single input string case
    return(reliable_translate_single(text, target_language, source_language))
  }
}

#' Helper function to translate a single text using Reliable Translation Service
#'
#' @param text Single text string to translate
#' @param target_language Target language code
#' @param source_language Source language code
#'
#' @return Translated text
#' @keywords internal
reliable_translate_single <- function(text, target_language, source_language) {
  # This uses a combination of existing reliable services
  tryCatch({
    # Try multiple services in order of preference
    # First try mymemory
    result <- mymemory_translate(text, target_language, source_language)
    if (!is.null(result) && result != text) {
      return(result)
    }
    
    # If that fails, try google translate
    result <- google_translate(text, target_language, source_language)
    return(result)
    
  }, error = function(e) {
    warning("Reliable translate failed, returning original text: ", e$message)
    return(text)
  })
}
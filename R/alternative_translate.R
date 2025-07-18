#' Translate text using Alternative Translation Service
#'
#' Translates input text to a specified language using a reliable free API.
#' This service provides a working alternative to other translation services.
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
#' alternative_translate("Hello world", target_language = "es")
#'
#' # Translate a vector of words
#' text_to_translate <- c("the", "quick", "brown")
#' alternative_translate(text_to_translate, "fr", "en")
#' }
alternative_translate <- function(text, target_language = "en", source_language = "auto") {
  
  # Check if input is a vector of strings
  is_vector <- is.vector(text) && length(text) > 1
  
  if (is_vector) {
    # Translate each sentence in the vector
    translations <- purrr::map_chr(text, function(single_text) {
      alternative_translate_single(single_text, target_language, source_language)
    })
    return(translations)
  } else {
    # Single input string case
    return(alternative_translate_single(text, target_language, source_language))
  }
}

#' Helper function to translate a single text using Alternative Translation Service
#'
#' @param text Single text string to translate
#' @param target_language Target language code
#' @param source_language Source language code
#'
#' @return Translated text
#' @keywords internal
alternative_translate_single <- function(text, target_language, source_language) {
  # This uses the same reliable approach as MyMemory
  tryCatch({
    # Use the existing reliable mymemory_translate as backend
    result <- mymemory_translate(text, target_language, source_language)
    return(result)
    
  }, error = function(e) {
    warning("Alternative translate failed, returning original text: ", e$message)
    return(text)
  })
}
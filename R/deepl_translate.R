#' Translate text using DeepL
#'
#' Translates input text to a specified language using DeepL's free web interface.
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
#' deepl_translate("Hello world", target_language = "es")
#'
#' # Translate a vector of words
#' text_to_translate <- c("the", "quick", "brown")
#' deepl_translate(text_to_translate, "fr", "en")
#' }
deepl_translate <- function(text, target_language = "en", source_language = "auto") {
  
  # Check if input is a vector of strings
  is_vector <- is.vector(text) && length(text) > 1
  
  if (is_vector) {
    # Translate each sentence in the vector
    translations <- purrr::map_chr(text, function(single_text) {
      deepl_translate_single(single_text, target_language, source_language)
    })
    return(translations)
  } else {
    # Single input string case
    return(deepl_translate_single(text, target_language, source_language))
  }
}

#' Helper function to translate a single text using DeepL
#'
#' @param text Single text string to translate
#' @param target_language Target language code
#' @param source_language Source language code
#'
#' @return Translated text
#' @keywords internal
deepl_translate_single <- function(text, target_language, source_language) {
  # DeepL uses uppercase language codes
  target_lang <- toupper(target_language)
  source_lang <- if (source_language == "auto") "AUTO" else toupper(source_language)
  
  # Prepare the request body
  body <- list(
    jsonrpc = "2.0",
    method = "LMT_handle_texts",
    params = list(
      texts = list(list(text = text)),
      splitting = "newlines",
      lang = list(
        source_lang_user_selected = source_lang,
        target_lang = target_lang
      )
    ),
    id = as.integer(runif(1, 1, 1000000))
  )
  
  # Set headers to mimic browser request
  headers <- httr::add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
    `Accept` = "*/*",
    `Accept-Language` = "en-US,en;q=0.9",
    `Accept-Encoding` = "gzip, deflate, br",
    `Content-Type` = "application/json",
    `Origin` = "https://www.deepl.com",
    `Referer` = "https://www.deepl.com/translator"
  )
  
  tryCatch({
    # Make the request to DeepL's API endpoint
    response <- httr::POST(
      "https://www2.deepl.com/jsonrpc?method=LMT_handle_texts",
      body = body,
      encode = "json",
      headers
    )
    
    # Check if request was successful
    if (httr::http_error(response)) {
      warning("DeepL API request failed with status: ", httr::status_code(response))
      return(text)  # Return original text if translation fails
    }
    
    # Parse the response
    content <- httr::content(response, "parsed")
    
    # Extract translation from response
    if (is.list(content) && !is.null(content$result) && !is.null(content$result$texts) && length(content$result$texts) > 0) {
      translation <- content$result$texts[[1]]$text
      return(translation)
    } else {
      warning("Unexpected response format from DeepL")
      return(text)
    }
    
  }, error = function(e) {
    warning("Error in DeepL translate: ", e$message)
    return(text)
  })
}
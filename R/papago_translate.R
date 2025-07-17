#' Translate text using Naver Papago
#'
#' Translates input text to a specified language using Naver Papago's free web interface.
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
#' papago_translate("Hello world", target_language = "ko")
#'
#' # Translate a vector of words
#' text_to_translate <- c("the", "quick", "brown")
#' papago_translate(text_to_translate, "ja", "en")
#' }
papago_translate <- function(text, target_language = "en", source_language = "auto") {
  
  # Check if input is a vector of strings
  is_vector <- is.vector(text) && length(text) > 1
  
  if (is_vector) {
    # Translate each sentence in the vector
    translations <- purrr::map_chr(text, function(single_text) {
      papago_translate_single(single_text, target_language, source_language)
    })
    return(translations)
  } else {
    # Single input string case
    return(papago_translate_single(text, target_language, source_language))
  }
}

#' Helper function to translate a single text using Naver Papago
#'
#' @param text Single text string to translate
#' @param target_language Target language code
#' @param source_language Source language code
#'
#' @return Translated text
#' @keywords internal
papago_translate_single <- function(text, target_language, source_language) {
  # Papago uses specific language codes
  lang_map <- list(
    "en" = "en",
    "ko" = "ko",
    "ja" = "ja",
    "zh" = "zh-cn",
    "zh-tw" = "zh-tw",
    "es" = "es",
    "fr" = "fr",
    "de" = "de",
    "ru" = "ru",
    "pt" = "pt",
    "it" = "it",
    "vi" = "vi",
    "th" = "th",
    "id" = "id",
    "hi" = "hi"
  )
  
  # Convert language codes to Papago format
  target_lang <- if (is.null(lang_map[[target_language]])) target_language else lang_map[[target_language]]
  source_lang <- if (source_language == "auto") "auto" else (if (is.null(lang_map[[source_language]])) source_language else lang_map[[source_language]])
  
  # Prepare the request body
  body <- list(
    source = source_lang,
    target = target_lang,
    text = text
  )
  
  # Set headers to mimic browser request
  headers <- httr::add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
    `Accept` = "application/json",
    `Content-Type` = "application/x-www-form-urlencoded; charset=UTF-8",
    `Origin` = "https://papago.naver.com",
    `Referer` = "https://papago.naver.com/",
    `X-Requested-With` = "XMLHttpRequest"
  )
  
  tryCatch({
    # Make the request to Papago's API endpoint
    response <- httr::POST(
      "https://papago.naver.com/apis/n2mt/translate",
      body = body,
      encode = "form",
      headers
    )
    
    # Check if request was successful
    if (httr::http_error(response)) {
      warning("Papago API request failed with status: ", httr::status_code(response))
      return(text)  # Return original text if translation fails
    }
    
    # Parse the response
    content <- httr::content(response, "parsed")
    
    # Extract translation from response
    if (is.list(content) && !is.null(content$translatedText)) {
      translation <- content$translatedText
      return(translation)
    } else if (is.list(content) && !is.null(content$message) && !is.null(content$message$result) && !is.null(content$message$result$translatedText)) {
      translation <- content$message$result$translatedText
      return(translation)
    } else {
      warning("Unexpected response format from Papago")
      return(text)
    }
    
  }, error = function(e) {
    warning("Error in Papago translate: ", e$message)
    return(text)
  })
}
#' Translate text using Oxford Dictionaries
#'
#' Translates input text to a specified language using Oxford Dictionaries API.
#' Note: This function requires an API key from Oxford Dictionaries.
#'
#' @param text This is the text that you want to translate. Can be a single string or a vector of strings.
#' @param target_language This is the language that you want to translate the text into.
#' The default value is "es" (Spanish).
#' @param source_language This is the language of the text to be translated.
#' The default value is "en" (English).
#' @param api_key Your Oxford Dictionaries API key. If not provided, will attempt to get from OXFORD_API_KEY environment variable.
#' @param app_id Your Oxford Dictionaries App ID. If not provided, will attempt to get from OXFORD_APP_ID environment variable.
#'
#' @return A translated string or vector of translated strings, matching the length of the input.
#' @export
#'
#' @examples
#' \dontrun{
#' # Set your API credentials first
#' Sys.setenv(OXFORD_API_KEY = "your_api_key")
#' Sys.setenv(OXFORD_APP_ID = "your_app_id")
#' 
#' # Translate a simple sentence
#' oxford_translate("Hello world", target_language = "es")
#'
#' # Translate a vector of words
#' text_to_translate <- c("hello", "world")
#' oxford_translate(text_to_translate, "fr", "en")
#' }
oxford_translate <- function(text, target_language = "es", source_language = "en", api_key = NULL, app_id = NULL) {
  
  # Get API credentials
  if (is.null(api_key)) {
    api_key <- Sys.getenv("OXFORD_API_KEY")
  }
  
  if (is.null(app_id)) {
    app_id <- Sys.getenv("OXFORD_APP_ID")
  }
  
  # Check if credentials are provided
  if (api_key == "" || app_id == "") {
    stop("Oxford Dictionaries API key and App ID are required. Please set OXFORD_API_KEY and OXFORD_APP_ID environment variables or provide them as parameters.")
  }
  
  # Check if input is a vector of strings
  is_vector <- is.vector(text) && length(text) > 1
  
  if (is_vector) {
    # Translate each sentence in the vector
    translations <- purrr::map_chr(text, function(single_text) {
      oxford_translate_single(single_text, target_language, source_language, api_key, app_id)
    })
    return(translations)
  } else {
    # Single input string case
    return(oxford_translate_single(text, target_language, source_language, api_key, app_id))
  }
}

#' Helper function to translate a single text using Oxford Dictionaries
#'
#' @param text Single text string to translate
#' @param target_language Target language code
#' @param source_language Source language code
#' @param api_key Oxford API key
#' @param app_id Oxford App ID
#'
#' @return Translated text
#' @keywords internal
oxford_translate_single <- function(text, target_language, source_language, api_key, app_id) {
  
  # Clean and encode the text
  word <- tolower(trimws(text))
  encoded_word <- urltools::url_encode(word)
  
  # Construct the Oxford API URL for translations
  url <- paste0(
    "https://od-api.oxforddictionaries.com/api/v2/translations/",
    source_language, "/", target_language, "/", encoded_word
  )
  
  # Set headers
  headers <- httr::add_headers(
    `app_id` = app_id,
    `app_key` = api_key,
    `Accept` = "application/json"
  )
  
  tryCatch({
    # Make the request
    response <- httr::GET(url, headers)
    
    # Check if request was successful
    if (httr::http_error(response)) {
      warning("Oxford Dictionaries API request failed with status: ", httr::status_code(response))
      return(text)  # Return original text if translation fails
    }
    
    # Parse the response
    content <- httr::content(response, "parsed")
    
    # Extract translation from response
    if (is.list(content) && !is.null(content$results) && length(content$results) > 0) {
      result <- content$results[[1]]
      
      if (!is.null(result$lexicalEntries) && length(result$lexicalEntries) > 0) {
        lexical_entry <- result$lexicalEntries[[1]]
        
        if (!is.null(lexical_entry$entries) && length(lexical_entry$entries) > 0) {
          entry <- lexical_entry$entries[[1]]
          
          if (!is.null(entry$senses) && length(entry$senses) > 0) {
            sense <- entry$senses[[1]]
            
            if (!is.null(sense$translations) && length(sense$translations) > 0) {
              translation <- sense$translations[[1]]$text
              return(translation)
            }
          }
        }
      }
    }
    
    warning("No translation found in Oxford Dictionaries response")
    return(text)
    
  }, error = function(e) {
    warning("Error in Oxford Dictionaries translate: ", e$message)
    return(text)
  })
}
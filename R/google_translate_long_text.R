#' Translate Long Text Using Google Translate
#'
#' Translates long text from one language to another using Google Translate by splitting the input into manageable chunks if necessary.
#'
#' @param text A single character string with the text to translate.
#' @param target_language The language code to translate the text into (default: "en" for English).
#' @param source_language The language code of the input text (default: "auto" for automatic detection).
#' @param chunk_size Maximum number of characters per translation request (default: 1000).
#' @param preserve_newlines Logical; if TRUE, preserves newlines between chunks in the output. If FALSE (default), newlines are replaced with spaces.
#'
#' @return A single character string containing the translated text.
#' @export
#'
#' @examples
#' \dontrun{
#' long_text <- paste(rep("This is a long text to translate.", 100), collapse = " ")
#' google_translate_long_text(
#'   long_text,
#'   target_language = "de",
#'   source_language = "en",
#'   chunk_size = 500,
#'   preserve_newlines = TRUE
#' )
#' }
google_translate_long_text <- function(text, 
                                       target_language = "en", 
                                       source_language = "auto", 
                                       chunk_size = 1000,
                                       preserve_newlines = FALSE) {
  
  # Validation checks
  if (!google_is_valid_language_code(target_language)) {
    stop("Invalid target language code: ", target_language)
  }
  if (!google_is_valid_language_code(source_language)) {
    stop("Invalid source language code: ", source_language)
  }
  
  # Chunk splitting logic
  split_text <- function(text, chunk_size) {
    indices <- seq(1, nchar(text), by = chunk_size)
    sapply(indices, function(i) substr(text, i, i + chunk_size - 1))
  }
  
  text_chunks <- if (nchar(text) > chunk_size) {
    split_text(text, chunk_size)
  } else {
    list(text)
  }
  
  # Translation processing
  translations <- sapply(text_chunks, function(chunk) {
    encoded_text <- urltools::url_encode(chunk)
    api_url <- sprintf(
      "https://translate.google.com/m?tl=%s&sl=%s&q=%s",
      target_language, 
      source_language,
      encoded_text
    )
    
    response <- httr::GET(api_url)
    translated <- httr::content(response) %>%
      rvest::html_nodes("div.result-container") %>%
      rvest::html_text()
    
    decoded <- urltools::url_decode(translated)
    
    # Conditional newline handling
    if (!preserve_newlines) {
      decoded <- gsub("\n", " ", decoded)
    }
    decoded
  })
  
  # Final assembly
  collapse_char <- ifelse(preserve_newlines, "\n", " ")
  paste(translations, collapse = collapse_char)
}

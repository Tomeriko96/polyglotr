#' Translate long text using Google Translate
#'
#' This function translates long text from one language to another using Google Translate.
#' It splits the text into smaller chunks if necessary to handle large inputs.
#'
#' @param text The long text to translate. Should be a single string.
#' @param target_language The language to translate the text into. Default is "en" for English.
#' @param source_language The language of the input text. Default is "auto" for automatic detection.
#' @param chunk_size The maximum number of characters to send in a single translation request. Default is 1000.
#'
#' @return A single string containing the translated text.
#' @export
#'
#' @examples
#' \donttest{
#' long_text <- paste(rep("This is a long text to translate.", 100), collapse = " ")
#' google_translate_long_text(
#' long_text, target_language = "de",
#' source_language = "en",
#' chunk_size = 500)
#' }
google_translate_long_text <- function(text, target_language = "en", source_language = "auto", chunk_size = 1000) {
  if (!google_is_valid_language_code(target_language)) {
    stop("Invalid target language code.")
  }
  if (!google_is_valid_language_code(source_language)) {
    stop("Invalid source language code.")
  }

  # Function to split text into chunks
  split_text <- function(text, chunk_size) {
    split_indices <- seq(1, nchar(text), by = chunk_size)
    sapply(split_indices, function(i) substr(text, i, i + chunk_size - 1))
  }

  # Split text into chunks if it's too long
  if (nchar(text) > chunk_size) {
    text_chunks <- split_text(text, chunk_size)
  } else {
    text_chunks <- list(text)
  }

  # Translate each chunk
  translations <- sapply(text_chunks, function(chunk) {
    formatted_text <- urltools::url_encode(chunk)
    formatted_link <- paste0(
      "https://translate.google.com/m?tl=",
      target_language, "&sl=", source_language,
      "&q=", formatted_text
    )

    response <- httr::GET(formatted_link)
    translation <- httr::content(response) %>%
      rvest::html_nodes("div.result-container") %>%
      rvest::html_text()

    translation <- urltools::url_decode(translation)
    gsub("\n", "", translation)
  })

  # Combine translated chunks
  paste(translations, collapse = " ")
}

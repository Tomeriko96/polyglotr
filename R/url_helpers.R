#' Replace URLs in a sentence with placeholders
#'
#' @param sentence A character string potentially containing one or more URLs.
#' @return A list with \code{text} (placeholder-replaced string) and
#'   \code{urls} (character vector of extracted URLs).
#' @keywords internal
replace_urls_with_placeholders <- function(sentence) {
  pattern <- "[a-zA-Z]{3,10}://[^\\s]*"
  urls <- regmatches(sentence, gregexpr(pattern, sentence, perl = TRUE))[[1]]
  placeholder_text <- sentence

  if (length(urls) > 0) {
    for (i in seq_along(urls)) {
      placeholder <- paste0("__URL", i, "__")
      placeholder_text <- sub(urls[i], placeholder, placeholder_text, fixed = TRUE)
    }
  }

  list(text = placeholder_text, urls = urls)
}

#' Restore URLs from placeholders in a translated text
#'
#' @param translated A character string containing placeholders.
#' @param urls A character vector of the original URLs to restore.
#' @return A character string with placeholders replaced by the original URLs.
#' @keywords internal
restore_urls_from_placeholders <- function(translated, urls) {
  restored_text <- translated
  for (i in seq_along(urls)) {
    pattern <- paste0("(?i)__url", i, "__")
    restored_text <- sub(pattern, urls[i], restored_text, perl = TRUE)
  }
  restored_text
}

#' Split text into word-boundary-aligned chunks
#'
#' Splits a character string into chunks of at most \code{chunk_size}
#' characters, always breaking at whitespace so words are never cut mid-token.
#'
#' @param text A single character string.
#' @param chunk_size Maximum number of characters per chunk. Default: 1000.
#' @return A character vector of chunks.
#' @keywords internal
split_text_at_words <- function(text, chunk_size = 1000) {
  if (nchar(text) <= chunk_size) return(text)

  words <- strsplit(text, "(?<=\\s)", perl = TRUE)[[1]]
  chunks <- character()
  current <- ""

  for (word in words) {
    if (nchar(current) + nchar(word) > chunk_size && nchar(current) > 0) {
      chunks <- c(chunks, trimws(current, which = "right"))
      current <- word
    } else {
      current <- paste0(current, word)
    }
  }

  if (nchar(trimws(current)) > 0) {
    chunks <- c(chunks, trimws(current, which = "right"))
  }

  chunks
}

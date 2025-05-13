#' Replace URLs in a sentence with placeholders
#'
#' Detects and replaces protocol-style links (e.g., `http://`, `https://`, `ftp://`) in a given sentence
#' with unique placeholders like `__URL1__`, `__URL2__`, etc. This is useful for preparing text for
#' translation or further processing while preserving original URLs.
#'
#' @param sentence A character string potentially containing one or more URLs.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{text}{The input sentence with URLs replaced by placeholders.}
#'   \item{urls}{A character vector of the extracted URLs.}
#' }
#'
replace_urls_with_placeholders <- function(sentence) {
  ## Match any protocol-style pattern like http://, https://, ftp://, htt:// etc.
  urls <- stringr::str_extract_all(sentence, "[a-zA-Z]{3,10}://[^\\s]*")[[1]]
  placeholder_text <- sentence

  ## Replace each detected URL with a unique placeholder like __URL1__, __URL2__, ...
  if (length(urls) > 0) {
    for (i in seq_along(urls)) {
      placeholder <- paste0("__URL", i, "__")
      placeholder_text <- stringr::str_replace(placeholder_text, stringr::fixed(urls[i]), placeholder)
    }
  }

  ## Return the placeholder-replaced sentence and the original URLs
  list(text = placeholder_text, urls = urls)
}

#' Restore URLs from placeholders in a translated text
#'
#' Replaces placeholders like `__URL1__`, `__URL2__`, etc. in a translated sentence back with their original URLs.
#' Handles both original and lowercased versions of the placeholder (to account for translation artifacts).
#'
#' @param translated A character string where placeholders should be replaced with original URLs.
#' @param urls A character vector of the original URLs to restore.
#'
#' @return A character string with the placeholders replaced by the corresponding URLs.
#'
restore_urls_from_placeholders <- function(translated, urls) {
  restored_text <- translated
  for (i in seq_along(urls)) {
    placeholder <- paste0("(?i)__url", i, "__")

    ## Replace exact placeholder
    restored_text <- stringr::str_replace(restored_text, placeholder, urls[i])

    ## Also try replacing lowercase version in case translation changed casing
    restored_text <- stringr::str_replace(restored_text, stringr::fixed(stringr::str_to_lower(placeholder)), urls[i])
  }
  restored_text
}

## Helper function: Replace all protocol-like links (even malformed) with placeholders
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

## Helper function: Restore URLs from placeholders back into the translated text
restore_urls_from_placeholders <- function(translated, urls) {
  restored_text <- translated
  for (i in seq_along(urls)) {
    placeholder <- paste0("__URL", i, "__")

    ## Replace exact placeholder
    restored_text <- stringr::str_replace(restored_text, stringr::fixed(placeholder), urls[i])

    ## Also try replacing lowercase version in case translation changed casing
    restored_text <- stringr::str_replace(restored_text, stringr::fixed(stringr::str_to_lower(placeholder)), urls[i])
  }
  restored_text
}

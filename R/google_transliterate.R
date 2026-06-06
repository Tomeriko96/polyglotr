#' Transliterate a single word or a sentence to the required language.
#'
#' @param text The word or sentence to transliterate from Latin/Roman (English) script.
#' @param language_tag The target language's ISO639 code. The default value for this argument is "el" for Greek.
#' @param num The maximum number of suggestions to fetch. The default value for this argument is 5.
#'
#' @return Character vector of transliterated sentences or larger pieces of text.
#' @export
#'
#' @examples
#' \dontrun{
#' google_transliterate("Hello world", "fr", 10)
#' google_transliterate("hello", "el", 10)
#' }
google_transliterate <- function(text, language_tag = "el", num = 5) {
  # Check if language code is valid
  if (!google_is_valid_language_code(language_tag)) {
    stop("Invalid language code.")
  }

  # Check if the input is a single word or a sentence
  if (grepl("\\s+", text)) {
    # It's a sentence
    # Split the text into words
    words <- strsplit(text, " ")[[1]]

    # Transliterate each word
    transliterated_words <- safe_http(
      lapply(words, function(word) {
        api_url <- paste0('https://inputtools.google.com/request?text=', word, '&itc=', language_tag, '-t-i0-und&num=', num, '&cp=0&cs=1&ie=utf-8&oe=utf-8&app=test')
        response <- httr::GET(api_url)
        json_content <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = TRUE)
        json_content[[2]][[1]][[2]]
      }),
      "Google Input Tools API"
    )
    if (is.null(transliterated_words)) return(invisible(NULL))

    # Merge the transliterated words into a single string for each suggestion
    merged_transliterated_words <- do.call(mapply, c(function(...) paste(..., sep = " "), transliterated_words, SIMPLIFY = TRUE, USE.NAMES = FALSE))

    return(merged_transliterated_words)
  } else {
    # It's a single word
    api_url <- paste0('https://inputtools.google.com/request?text=', text, '&itc=', language_tag, '-t-i0-und&num=', num, '&cp=0&cs=1&ie=utf-8&oe=utf-8&app=test')
    response <- safe_http(httr::GET(api_url), "Google Input Tools API")
    if (is.null(response)) return(invisible(NULL))
    json_content <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = TRUE)
    return(json_content[[2]][[1]][[2]])
  }
}

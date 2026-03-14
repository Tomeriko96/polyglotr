#' Transliterate a word or sentence to a target script
#'
#' Uses the Google Input Tools API to produce romanisation suggestions for a
#' word or sentence. Multi-word input is handled by transliterating each word
#' individually and merging the suggestion lists.
#'
#' @param text The word or sentence to transliterate from Latin/Roman script.
#' @param language_tag The target language ISO 639 code. Default: \code{"el"}
#'   (Greek).
#' @param num Maximum number of transliteration suggestions to return. Default:
#'   \code{5}.
#'
#' @return A character vector of up to \code{num} transliterated suggestions.
#' @export
#'
#' @examples
#' \dontrun{
#' google_transliterate("hello", "el", 5)
#' google_transliterate("Hello world", "ar", 3)
#' }
google_transliterate_word <- function(word, language_tag, num) {
  api_url <- sprintf(
    "https://inputtools.google.com/request?text=%s&itc=%s-t-i0-und&num=%d&cp=0&cs=1&ie=utf-8&oe=utf-8&app=test",
    utils::URLencode(word, repeated = TRUE),
    language_tag,
    num
  )
  json_content <- httr2::request(api_url) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
  json_content[[2]][[1]][[2]]
}

google_transliterate <- function(text, language_tag = "el", num = 5) {
  if (!google_is_valid_language_code(language_tag)) {
    stop("Invalid language code.")
  }

  words <- strsplit(text, " ")[[1]]

  if (length(words) == 1) {
    return(google_transliterate_word(words[[1]], language_tag, num))
  }

  transliterated_words <- lapply(words, google_transliterate_word, language_tag, num)
  do.call(
    mapply,
    c(list(FUN = function(...) paste(..., sep = " ")),
      transliterated_words,
      list(SIMPLIFY = TRUE, USE.NAMES = FALSE))
  )
}

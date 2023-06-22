#' Translate word using Linguee Translation API
#'
#' @param word This is the word that you want to translate.
#' @param target_language This is the language that you want to translate the word into.
#' @param source_language This is the language of the word that you want to translate.
#' @param guess_direction Specifies whether the API should guess the translation direction when the source language is set to "auto".
#' The default value is FALSE.
#' @param follow_corrections Specifies whether the API should include translations that have been marked as corrections.
#' The default value is "always" to include corrections.
#'
#' @return Translated word options.
#' @export
#'
#' @examples
#'\donttest{
#'linguee_word_translation("hello", target_language = "es", source_language = "en")
#'}
linguee_word_translation <- function(word, target_language, source_language, guess_direction = FALSE, follow_corrections = "always") {
  api_root <- "https://linguee-api.fly.dev/api/v2"
  endpoint <- paste0(api_root, "/translations")

  params <- list(
    query = word,
    src = source_language,
    dst = target_language,
    guess_direction = tolower(guess_direction),
    follow_corrections = follow_corrections
  )

  response <- httr::GET(url = endpoint, query = params)
  if (response$status_code != 200) {
    stop("Error: API request failed with status code ", response$status_code)
  }

  translation_data <- httr::content(response, "parsed")

  translated_options <- character()

  for (lemma in translation_data) {
    for (translation in lemma$translations) {
      translated_options <- append(translated_options, translation$text)
    }
  }

  return(translated_options)
}

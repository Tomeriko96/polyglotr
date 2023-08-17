#' Provide translation examples using Linguee Translation API
#'
#' @param query The word or phrase for which you want translation examples.
#' @param src The source language of the word or phrase. Accepts language codes such as "en", "es", "fr", etc.
#' @param dst The target language for the translation examples. Accepts language codes such as "en", "es", "fr", etc.
#' @param guess_direction A boolean flag that determines whether the API should guess the translation direction.
#'                        The default value is FALSE.
#' @param follow_corrections Specifies how to treat responses with a "did you mean" link.
#'                           Possible values are "always", "never", or "on_empty_translations".
#'                           The default value is "always".
#'
#' @return A dataframe of translation examples with columns: source, target, pos.
#'
#' @examples
#' \donttest{
#' linguee_translation_examples(query = "hello", src = "en", dst = "es")
#' }
#'
#' @seealso linguee_word_translation
#'
#' @export
linguee_translation_examples <- function(query, src, dst, guess_direction = FALSE, follow_corrections = "always") {
  api_root <- "https://linguee-api.fly.dev/api/v2/examples"
  endpoint <- api_root

  params <- list(
    query = query,
    src = src,
    dst = dst,
    guess_direction = guess_direction,
    follow_corrections = follow_corrections
  )

  response <- httr::GET(url = endpoint, query = params)
  if (response$status_code != 200) {
    stop("Error: API request failed with status code ", response$status_code)
  }

  translation_examples <- httr::content(response, "parsed")

  # Extract the relevant information from the translation examples
  examples_df <- purrr::map_dfr(translation_examples, function(example) {
    tibble::tibble(
      source = example$text,
      target = example$translations[[1]]$text,
      pos = example$translations[[1]]$pos
    )
  })

  return(examples_df)
}

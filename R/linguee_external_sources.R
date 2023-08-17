#' Retrieve external sources using Linguee Translation API
#'
#' @param query The word or phrase for which you want to retrieve external sources.
#' @param src The source language of the word or phrase. Accepts language codes such as "en", "es", "fr", etc.
#' @param dst The target language for the external source retrieval. Accepts language codes such as "en", "es", "fr", etc.
#' @param limit The maximum number of external sources to retrieve. Defaults to 5.
#'
#' @return A dataframe of external sources with columns: src, dst, src_url, dst_url.
#'
#' @examples
#' \donttest{
#' linguee_external_sources(query = "hello", src = "en", dst = "es")
#' }
#'
#' @seealso linguee_word_translation, linguee_translation_examples
#'
#'
#' @export
linguee_external_sources <- function(query, src, dst, limit = 5) {
  api_root <- "https://linguee-api.fly.dev/api/v2/external_sources"
  endpoint <- api_root

  params <- list(
    query = query,
    src = src,
    dst = dst,
    limit = limit
  )

  response <- httr::GET(url = endpoint, query = params)

  if (response$status_code != 200) {
    stop("Error: API request failed with status code ", response$status_code)
  }

  external_sources <- httr::content(response, "parsed")

  # Combine the individual lists into a single dataframe
  sources_df <- dplyr::bind_rows(external_sources)

  return(sources_df)
}

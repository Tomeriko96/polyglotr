#' Get Apertium Language Pairs
#'
#' This function retrieves the supported language pairs from the Apertium API.
#'
#' @param host Host URL for the Apertium API (default is "https://apertium.org/apy").
#'
#' @return A list of language pairs. Each element contains sourceLanguage and targetLanguage.
#' @export
#'
#' @examples
#' \donttest{
#' pairs <- apertium_get_language_pairs()
#' head(pairs, 5)
#'
#' # Using custom host
#' pairs <- apertium_get_language_pairs(host = "https://your-custom-apertium-server.com/apy")
#' }
apertium_get_language_pairs <- function(host = "https://apertium.org/apy") {

  formatted_link <- paste0(host, "/listPairs")

  response <- httr::GET(formatted_link) %>%
    httr::content()

  df_pairs <- purrr::map_dfr(response$responseData, tibble::as_tibble)

  return(df_pairs)
}

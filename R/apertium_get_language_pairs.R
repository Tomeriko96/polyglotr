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
#' }
apertium_get_language_pairs <- function(host = "https://apertium.org/apy") {
  result <- http_get_json(paste0(host, "/listPairs"))
  do.call(rbind, lapply(result$responseData, as.data.frame, stringsAsFactors = FALSE))
}

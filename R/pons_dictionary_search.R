#' Search for words in PONS dictionaries
#'
#' @param query The word or phrase to search for in the dictionary.
#' @param source_language The source language code (ISO 639-1 - two-letter codes).
#' @param target_language The target language code (ISO 639-1 - two-letter codes).
#' @param fuzzy Whether to enable fuzzy search (TRUE/FALSE). Default is FALSE.
#' @param limit Maximum number of results to return. Default is 10.
#'
#' @return A data frame with search results containing word information, translations, and definitions.
#' @export
#'
#' @examples
#' \dontrun{
#' pons_dictionary_search("hello", "en", "es")
#' pons_dictionary_search("casa", "es", "en", fuzzy = TRUE, limit = 5)
#' }
pons_dictionary_search <- function(query, source_language, target_language, fuzzy = FALSE, limit = 10) {
  # Validate inputs
  if (missing(query) || missing(source_language) || missing(target_language)) {
    stop("query, source_language, and target_language are required parameters")
  }
  
  # Construct the API URL for dictionary search
  url <- paste0("https://api.pons.com/v1/dictionary?l=", 
                source_language, target_language, 
                "&q=", urltools::url_encode(query),
                "&fuzzy=", tolower(fuzzy),
                "&limit=", limit)
  
  # Make the request
  response <- httr::GET(url)
  
  # Check if the request was successful
  if (httr::status_code(response) != 200) {
    stop("Failed to search dictionary. Status code: ", httr::status_code(response))
  }
  
  # Parse the response
  content <- httr::content(response, "parsed")
  
  # Check if we have results
  if (length(content) == 0) {
    message("No results found for query: ", query)
    return(data.frame())
  }
  
  # Extract results into a data frame
  results <- purrr::map_dfr(content, function(result) {
    # Extract basic information
    headword <- if (is.null(result$headword)) "" else result$headword
    wordclass <- if (is.null(result$wordclass)) "" else result$wordclass
    
    # Extract translations
    translations <- character()
    if (!is.null(result$hits)) {
      for (hit in result$hits) {
        if (!is.null(hit$roms)) {
          for (rom in hit$roms) {
            if (!is.null(rom$arabs)) {
              for (arab in rom$arabs) {
                if (!is.null(arab$translations)) {
                  for (translation in arab$translations) {
                    translations <- c(translations, if (is.null(translation$target)) "" else translation$target)
                  }
                }
              }
            }
          }
        }
      }
    }
    
    # Create result row
    tibble::tibble(
      source_word = headword,
      word_class = wordclass,
      translations = paste(translations, collapse = "; "),
      source_language = source_language,
      target_language = target_language
    )
  })
  
  return(results)
}
#' Get synonyms for words from PONS dictionaries
#'
#' @param word The word to get synonyms for.
#' @param source_language The source language code (ISO 639-1 - two-letter codes).
#' @param target_language The target language code (ISO 639-1 - two-letter codes).
#' @param limit Maximum number of synonyms to return. Default is 5.
#'
#' @return A data frame with synonyms and related words.
#' @export
#'
#' @examples
#' \dontrun{
#' pons_word_synonyms("hello", "en", "es")
#' pons_word_synonyms("casa", "es", "en", limit = 3)
#' }
pons_word_synonyms <- function(word, source_language, target_language, limit = 5) {
  # Validate inputs
  if (missing(word) || missing(source_language) || missing(target_language)) {
    stop("word, source_language, and target_language are required parameters")
  }
  
  # Use dictionary search to find synonyms
  search_result <- pons_dictionary_search(word, source_language, target_language, fuzzy = TRUE, limit = limit * 2)
  
  # If we have results, extract synonyms
  if (nrow(search_result) > 0) {
    # Extract unique translations as potential synonyms
    synonyms <- unique(unlist(strsplit(search_result$translations, "; ")))
    
    # Remove empty strings
    synonyms <- synonyms[synonyms != ""]
    
    # Limit results
    if (length(synonyms) > limit) {
      synonyms <- synonyms[1:limit]
    }
    
    # Create data frame
    synonyms_df <- tibble::tibble(
      word = word,
      synonym = synonyms,
      source_language = source_language,
      target_language = target_language
    )
    
    return(synonyms_df)
  } else {
    message("No synonyms found for word: ", word)
    return(data.frame())
  }
}
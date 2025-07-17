#' Get example sentences for words from PONS dictionaries
#'
#' @param word The word or phrase to get examples for.
#' @param source_language The source language code (ISO 639-1 - two-letter codes).
#' @param target_language The target language code (ISO 639-1 - two-letter codes).
#' @param limit Maximum number of examples to return. Default is 5.
#'
#' @return A data frame with example sentences in source and target languages.
#' @export
#'
#' @examples
#' \dontrun{
#' pons_word_examples("hello", "en", "es")
#' pons_word_examples("casa", "es", "en", limit = 3)
#' }
pons_word_examples <- function(word, source_language, target_language, limit = 5) {
  # Validate inputs
  if (missing(word) || missing(source_language) || missing(target_language)) {
    stop("word, source_language, and target_language are required parameters")
  }
  
  # First, search for the word in the dictionary to get detailed information
  search_url <- paste0("https://api.pons.com/v1/dictionary?l=", 
                      source_language, target_language, 
                      "&q=", urltools::url_encode(word))
  
  # Make the request
  response <- httr::GET(search_url)
  
  # Check if the request was successful
  if (httr::status_code(response) != 200) {
    stop("Failed to get word examples. Status code: ", httr::status_code(response))
  }
  
  # Parse the response
  content <- httr::content(response, "parsed")
  
  # Check if we have results
  if (length(content) == 0) {
    message("No examples found for word: ", word)
    return(data.frame())
  }
  
  # Extract examples from the response
  examples <- list()
  example_count <- 0
  
  for (result in content) {
    if (example_count >= limit) break
    
    if (!is.null(result$hits)) {
      for (hit in result$hits) {
        if (example_count >= limit) break
        
        if (!is.null(hit$roms)) {
          for (rom in hit$roms) {
            if (example_count >= limit) break
            
            if (!is.null(rom$arabs)) {
              for (arab in rom$arabs) {
                if (example_count >= limit) break
                
                if (!is.null(arab$translations)) {
                  for (translation in arab$translations) {
                    if (example_count >= limit) break
                    
                    # Look for example sentences in the translation
                    source_example <- if (is.null(translation$source)) "" else translation$source
                    target_example <- if (is.null(translation$target)) "" else translation$target
                    
                    if (nchar(source_example) > 0 && nchar(target_example) > 0) {
                      examples[[length(examples) + 1]] <- list(
                        source_example = source_example,
                        target_example = target_example,
                        source_language = source_language,
                        target_language = target_language
                      )
                      example_count <- example_count + 1
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  # Convert to data frame
  if (length(examples) > 0) {
    examples_df <- purrr::map_dfr(examples, function(example) {
      tibble::tibble(
        source_example = example$source_example,
        target_example = example$target_example,
        source_language = example$source_language,
        target_language = example$target_language
      )
    })
    return(examples_df)
  } else {
    message("No examples found for word: ", word)
    return(data.frame())
  }
}
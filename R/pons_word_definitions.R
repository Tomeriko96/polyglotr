#' Get definitions for words from PONS dictionaries
#'
#' @param word The word to get definitions for.
#' @param source_language The source language code (ISO 639-1 - two-letter codes).
#' @param target_language The target language code (ISO 639-1 - two-letter codes).
#' @param limit Maximum number of definitions to return. Default is 5.
#'
#' @return A data frame with word definitions and grammatical information.
#' @export
#'
#' @examples
#' \dontrun{
#' pons_word_definitions("hello", "en", "es")
#' pons_word_definitions("casa", "es", "en", limit = 3)
#' }
pons_word_definitions <- function(word, source_language, target_language, limit = 5) {
  # Validate inputs
  if (missing(word) || missing(source_language) || missing(target_language)) {
    stop("word, source_language, and target_language are required parameters")
  }
  
  # Construct the API URL for dictionary search
  url <- paste0("https://api.pons.com/v1/dictionary?l=", 
                source_language, target_language, 
                "&q=", urltools::url_encode(word))
  
  # Make the request
  response <- httr::GET(url)
  
  # Check if the request was successful
  if (httr::status_code(response) != 200) {
    stop("Failed to get word definitions. Status code: ", httr::status_code(response))
  }
  
  # Parse the response
  content <- httr::content(response, "parsed")
  
  # Check if we have results
  if (length(content) == 0) {
    message("No definitions found for word: ", word)
    return(data.frame())
  }
  
  # Extract definitions from the response
  definitions <- list()
  definition_count <- 0
  
  for (result in content) {
    if (definition_count >= limit) break
    
    headword <- if (is.null(result$headword)) word else result$headword
    wordclass <- if (is.null(result$wordclass)) "" else result$wordclass
    
    if (!is.null(result$hits)) {
      for (hit in result$hits) {
        if (definition_count >= limit) break
        
        if (!is.null(hit$roms)) {
          for (rom in hit$roms) {
            if (definition_count >= limit) break
            
            # Extract headword and part of speech from rom
            rom_headword <- if (is.null(rom$headword)) headword else rom$headword
            rom_wordclass <- if (is.null(rom$wordclass)) wordclass else rom$wordclass
            
            if (!is.null(rom$arabs)) {
              for (arab in rom$arabs) {
                if (definition_count >= limit) break
                
                # Extract definition information
                header <- if (is.null(arab$header)) "" else arab$header
                
                if (!is.null(arab$translations)) {
                  for (translation in arab$translations) {
                    if (definition_count >= limit) break
                    
                    target_text <- if (is.null(translation$target)) "" else translation$target
                    source_text <- if (is.null(translation$source)) rom_headword else translation$source
                    
                    if (nchar(target_text) > 0) {
                      definitions[[length(definitions) + 1]] <- list(
                        word = rom_headword,
                        part_of_speech = rom_wordclass,
                        definition = target_text,
                        context = header,
                        source_language = source_language,
                        target_language = target_language
                      )
                      definition_count <- definition_count + 1
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
  if (length(definitions) > 0) {
    definitions_df <- purrr::map_dfr(definitions, function(definition) {
      tibble::tibble(
        word = definition$word,
        part_of_speech = definition$part_of_speech,
        definition = definition$definition,
        context = definition$context,
        source_language = definition$source_language,
        target_language = definition$target_language
      )
    })
    return(definitions_df)
  } else {
    message("No definitions found for word: ", word)
    return(data.frame())
  }
}
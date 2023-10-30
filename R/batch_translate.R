#' Batch Translation Function
#'
#' This function translates a file into each target language using the polyglotr package's translate_file function, and saves the translated files.
#'
#' @param input_file A character string indicating the path to the input file.
#' @param source_language A character string indicating the source language.
#' @param target_languages A character vector indicating the target languages.
#' @return Nothing is returned.
#' @examples
#' \dontrun{
#' batch_translate("README.md", "nl", c("fr", "es", "de"))
#' }
batch_translate <- function(input_file, source_language, target_languages) {
  # Generate the base file name from the input file name
  base_file_name <- basename(input_file)

  # Remove the file extension from the base file name
  base_file_name <- sub("\\.[^.]+$", "", base_file_name)

  # Generate the output file name for each target language
  for (target_language in target_languages) {
    # Generate the output file name
    # Translate the file into the target language
    google_translate(input_file, source_language = source_language, target_language = target_language)
  }
}

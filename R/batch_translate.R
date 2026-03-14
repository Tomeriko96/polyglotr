#' Batch Translation Function
#'
#' This function translates a file into each target language using the polyglotr package's translate_file function, and saves the translated files.
#'
#' @param input_file A character string indicating the path to the input file.
#' @param source_language A character string indicating the source language.
#' @param target_languages A character vector indicating the target languages.
#' @return Nothing is returned.
#' @export
#' @examples
#' \dontrun{
#' batch_translate("README.md", "nl", c("fr", "es", "de"))
#' }
batch_translate <- function(input_file, source_language, target_languages) {
  for (target_language in target_languages) {
    translate_file(input_file,
      source_language = source_language,
      target_language = target_language
    )
  }
  invisible(NULL)
}

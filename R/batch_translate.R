#' Batch Translation Function
#'
#' This function translates a file into each target language using the polyglotr package's translate_file function, and saves the translated files.
#'
#' One file is written per target language, next to the input file, named
#' `<name>_<target_language>_translated.<ext>`. The input file is not modified.
#'
#' @param input_file A character string indicating the path to the input file.
#' @param source_language A character string indicating the source language.
#' @param target_languages A character vector indicating the target languages.
#' @return A character vector with the paths of the translated files, named by
#'   target language, invisibly.
#' @export
#' @examples
#' \dontrun{
#' batch_translate("README.md", "nl", c("fr", "es", "de"))
#' }
batch_translate <- function(input_file, source_language, target_languages) {
  if (!file.exists(input_file)) {
    stop("Input file does not exist: ", input_file)
  }
  # Validate every language up front so a bad code does not leave a partial set of files.
  invalid <- target_languages[!vapply(target_languages, google_is_valid_language_code, logical(1))]
  if (!google_is_valid_language_code(source_language)) invalid <- c(source_language, invalid)
  if (length(invalid) > 0) {
    stop("Invalid language code(s): ", paste(invalid, collapse = ", "))
  }
  paths <- vapply(target_languages, function(target_language) {
    translate_file(
      input_file,
      target_language = target_language,
      source_language = source_language,
      overwrite = FALSE
    )
  }, character(1))
  names(paths) <- target_languages
  invisible(paths)
}

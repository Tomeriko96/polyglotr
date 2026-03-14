#' Translate File
#'
#' Translates the content of a file using Google Translate API.
#'
#' @param file_path The path to the file to be translated.
#' @param target_language The target language to translate the file content to. Default is "en".
#' @param source_language The source language of the file content. Default is "auto".
#' @param overwrite Logical indicating whether to overwrite the original file with the translated content. Default is FALSE.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' translate_file("path/to/file.txt", target_language = "fr", source_language = "en", overwrite = TRUE)
#' }
#' @export
translate_file <- function(file_path, target_language = "en", source_language = "auto",
                           overwrite = FALSE) {
  lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")

  translate_line <- function(line) {
    if (startsWith(line, "#'")) {
      paste0("#' ", google_translate(
        substr(line, 3, nchar(line)),
        target_language = target_language,
        source_language = source_language
      ))
    } else {
      google_translate(line,
        target_language = target_language,
        source_language = source_language
      )
    }
  }

  translated_lines <- vapply(lines, translate_line, character(1), USE.NAMES = FALSE)

  combined_lines <- mapply(function(original, translated) {
    leading_ws <- sub("^([ \t]*).*$", "\\1", original)
    paste0(leading_ws, translated)
  }, lines, translated_lines, SIMPLIFY = TRUE, USE.NAMES = FALSE)

  if (overwrite) {
    writeLines(combined_lines, con = file_path, sep = "\n", useBytes = FALSE)
    invisible(file_path)
  } else {
    file_extension <- tools::file_ext(file_path)
    new_file_path  <- paste0(
      tools::file_path_sans_ext(file_path), "_", target_language, "_translated.",
      file_extension
    )
    writeLines(combined_lines, con = new_file_path, sep = "\n", useBytes = FALSE)
    invisible(new_file_path)
  }
}


#' Translate File
#'
#' Translates the content of a file line by line using Google Translate.
#'
#' Leading indentation is preserved and is not sent to the translation
#' service. Blank lines are kept as they are and are not sent either. Lines
#' starting with a roxygen comment marker (`#'`) keep the marker and only the
#' text after it is translated. A line that cannot be translated (for example
#' because the service is unavailable) is kept in its original form, and a
#' warning reports how many lines were left untranslated.
#'
#' @param file_path The path to the file to be translated.
#' @param target_language The target language to translate the file content to. Default is "en".
#' @param source_language The source language of the file content. Default is "auto".
#' @param overwrite Logical indicating whether to overwrite the original file with the translated content. Default is FALSE.
#'
#' @return The path of the file that was written, invisibly. With
#'   `overwrite = FALSE` this is a new file next to the input named
#'   `<name>_<target_language>_translated.<ext>`.
#'
#' @examples
#' \dontrun{
#' translate_file("path/to/file.txt", target_language = "fr", source_language = "en", overwrite = TRUE)
#' }
#' @export
translate_file <- function(file_path, target_language = "en", source_language = "auto", overwrite = FALSE) {
  lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")

  translate_text <- function(text) {
    result <- google_translate(text, target_language = target_language, source_language = source_language)
    if (is.null(result) || length(result) != 1 || is.na(result) || !nzchar(result)) {
      return(NULL)
    }
    result
  }

  skipped <- 0L
  translate_line <- function(line) {
    if (!nzchar(trimws(line))) return(line)

    indent <- sub("^([ \t]*).*$", "\\1", line)
    body <- substr(line, nchar(indent) + 1, nchar(line))

    prefix <- ""
    if (startsWith(body, "#'")) {
      rest <- substr(body, 3, nchar(body))
      if (!nzchar(trimws(rest))) return(line)
      # keep the marker and the whitespace after it (e.g. indented @examples code)
      prefix <- paste0("#'", sub("^([ \t]*).*$", "\\1", rest))
      body <- substr(rest, nchar(prefix) - 1, nchar(rest))
    }

    translated <- translate_text(body)
    if (is.null(translated)) {
      skipped <<- skipped + 1L
      return(line)
    }
    paste0(indent, prefix, translated)
  }

  translated_lines <- vapply(lines, translate_line, character(1), USE.NAMES = FALSE)

  if (skipped > 0) {
    warning(
      skipped, " line(s) could not be translated and were kept in their original form.",
      call. = FALSE
    )
  }

  if (overwrite) {
    out_path <- file_path
  } else {
    file_extension <- tools::file_ext(file_path)
    out_path <- paste0(tools::file_path_sans_ext(file_path), "_", target_language, "_translated.", file_extension)
  }
  writeLines(translated_lines, con = out_path, sep = "\n", useBytes = FALSE)

  invisible(out_path)
}

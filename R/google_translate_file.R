translate_file <- function(file_path, target_language = "en", source_language = "auto", overwrite = FALSE) {

  lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")

  translate_line <- function(line) {
    if (startsWith(line, "#'")) {
      return(paste0("#' ", google_translate(substr(line, 3, nchar(line)), target_language = target_language, source_language = source_language)))
    } else {
      return(google_translate(line, target_language = target_language, source_language = source_language))
    }
  }

  translated_lines <- sapply(lines, translate_line)

  combined_lines <- mapply(function(original, translated) {
    return(paste0(substr(original, 1, regexpr("[^ ]", original) - 1), translated))
  }, lines, translated_lines, SIMPLIFY = FALSE)

  if (overwrite) {
    writeLines(combined_lines, con = file_path, sep = "\n", useBytes = FALSE)
  } else {
    file_extension <- tools::file_ext(file_path)
    new_file_path <- paste0(tools::file_path_sans_ext(file_path), "_translated.", file_extension)
    writeLines(as.character(combined_lines), con = new_file_path, sep = "\n", useBytes = FALSE)
  }

  return(NULL)
}

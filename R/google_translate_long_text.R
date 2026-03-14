#' Translate Long Text Using Google Translate
#'
#' @description
#' **Deprecated.** This function is deprecated. Use \code{\link{google_translate}} instead,
#' which now handles long texts automatically via the \code{chunk_size}
#' parameter.
#'
#' @param text A character vector of strings to translate.
#' @param target_language The language code to translate into (default:
#'   \code{"en"}).
#' @param source_language The language code of the input (default:
#'   \code{"auto"}).
#' @param chunk_size Maximum characters per request (default: \code{1000}).
#' @param preserve_newlines Logical; if \code{TRUE}, newlines are preserved
#'   between reassembled chunks in the output.
#'
#' @return A character vector of translated strings.
#' @export
#'
#' @examples
#' \dontrun{
#' # Deprecated — use google_translate() instead:
#' google_translate("Some very long text...", target_language = "de")
#' }
google_translate_long_text <- function(text,
                                       target_language = "en",
                                       source_language = "auto",
                                       chunk_size = 1000,
                                       preserve_newlines = FALSE) {
  .Deprecated(
    "google_translate",
    msg = paste(
      "google_translate_long_text() is deprecated.",
      "Use google_translate() instead, which now handles long texts",
      "automatically via the chunk_size parameter."
    )
  )

  result <- google_translate(text,
    target_language = target_language,
    source_language = source_language,
    chunk_size = chunk_size
  )

  if (!preserve_newlines) {
    result <- gsub("\n", " ", result)
  }
  result
}

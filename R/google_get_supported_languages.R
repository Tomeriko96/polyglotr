#' Get Supported Languages
#'
#' Fetches the supported languages from the Google Cloud Translate documentation
#' page.
#'
#' @return A data frame containing the supported languages and their
#'   corresponding ISO 639-1 codes.
#' @export
google_get_supported_languages <- function() {
  url <- "https://cloud.google.com/translate/docs/languages"
  webpage <- rvest::read_html(url)
  table <- webpage |> rvest::html_nodes("table")
  df <- table |> rvest::html_table()
  as.data.frame(df[[1]])
}

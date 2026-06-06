#' Get Supported Languages
#'
#' This function fetches the supported languages from the Google Cloud Translate documentation page.
#'
#' @return A data frame containing the supported languages and their corresponding ISO 639-1 codes.
#' @export
google_get_supported_languages <- function() {
  # Define the URL of the webpage
  url <- "https://cloud.google.com/translate/docs/languages"

  # Check if the URL is available
  if (!RCurl::url.exists(url)) {
    message("The Google Cloud Translate documentation page is not available.")
    return(invisible(NULL))
  }

  # Read the HTML of the webpage
  webpage <- safe_http(rvest::read_html(url), "Google Cloud Translate documentation")
  if (is.null(webpage)) return(invisible(NULL))

  # Select the table from the webpage
  table <- webpage %>% rvest::html_nodes('table')

  # Convert the HTML table into a data frame
  df <- table %>% rvest::html_table()

  # Convert the first table in the list into a data frame
  df_lang <- df[[1]] %>% tibble::as_tibble()

  return(df_lang)
}

#' Translate text using WMCloud
#'
#' This function sends a POST request to the WMCloud translation API with the specified parameters,
#' parses the JSON response, and returns the translated text.
#'
#' @param text The text to translate.
#' @param target_language The target language for the translation (default is "en").
#' @param source_language The source language of the text (default is "en").
#' @param format The format of the content ("html", "json", "markdown", "text", "svg", "webpage").
#' @param model The model to use for the translation (only "nllb200-600M" is currently known to work).
#'
#' @return The translated text.
#' @export
wmcloud_translate <- function(text,
                              target_language = "en",
                              source_language = "en",
                              format = "text",
                              model = "nllb200-600M") {
  # Define the URL of the API
  url <- "https://translate.wmcloud.org/api/translate"

  # List of valid formats
  valid_formats <- c("html", "json", "markdown", "text", "svg", "webpage")

  # List of valid models
  valid_models <- c("nllb200-600M") # Add more models here

  # Check if format and model are valid
  if (!format %in% valid_formats) {
    stop(paste("Invalid format. Must be one of:", paste(valid_formats, collapse = ", ")))
  }
  if (!model %in% valid_models) {
    stop(paste("Invalid model. Must be one of:", paste(valid_models, collapse = ", ")))
  }

  # Create a list of parameters to send in the POST request
  body <- list(
    source_language = source_language,
    target_language = target_language,
    format = format,
    model = model,
    content = text
  )

  # Convert the list to a JSON string
  json_body <- jsonlite::toJSON(body, auto_unbox = TRUE)

  # Set the content type of the request to 'application/json'
  headers <- c("Content-Type" = "application/json")

  # Send the POST request and get the response
  response <- httr::POST(url, body = json_body, httr::add_headers(headers))

  # Check if the request was successful
  if (httr::status_code(response) != 200) {
    stop("Request failed with status ", httr::status_code(response))
  }

  # Parse the response
  result <- httr::content(response, "parsed")

  # Extract the translated text from the response
  translation <- result$translation

  return(translation)
}




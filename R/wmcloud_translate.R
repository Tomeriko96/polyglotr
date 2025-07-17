#' Translate content using WMCloud
#'
#' This function sends a POST request to the WMCloud translation API with the specified parameters,
#' parses the JSON response, and returns the translated content.
#'
#' @param content The content to translate. Can be plain text, a URL (for a webpage), a JSON string, a Markdown string, HTML content, or SVG content.
#' @param target_language The target language for the translation (default is "en").
#' @param source_language The source language of the content (default is "en").
#' @param format The format of the content ("html", "json", "markdown", "svg", "text", "webpage").
#' @param model The model to use for the translation (only "nllb200-600M" is currently known to work).
#'
#' @return The translated content.
#'
#' @examples
#' \dontrun{
#' # Translate plain text
#' wmcloud_translate("rijst",
#' target_language = "es",
#' source_language = "nl", format = "text")
#'
#' # Translate a webpage
#' wmcloud_translate("https://en.m.wikivoyage.org/wiki/Goes",
#' target_language = "es",
#' source_language = "en", format = "webpage")
#'
#' # Translate JSON content
#' wmcloud_translate('{
#'     "id": 1,
#'     "title": "Chicken Biryani",
#'     "description": "Chicken Biryani is a savory chicken and rice dish",
#'     "ingredients": [ "Vegetable oil", "Garlic", "Ginger" ,"Rice"]
#' }
#'                ', target_language = "es", source_language = "en", format = "json")
#'
#' # Translate Markdown content
#' wmcloud_translate('# Heading
#'
#' This is a [link to Wikipedia](https://wikipedia.org)
#'                 ', target_language = "es", source_language = "en", format = "markdown")
#'
#' # Translate HTML content
#' wmcloud_translate('<h1>Hello World</h1>
#' <p>This is a paragraph with <a href="https://example.com">a link</a>.</p>
#'                 ', target_language = "es", source_language = "en", format = "html")
#'
#' # Translate SVG content
#' wmcloud_translate('<svg><text x="10" y="20">Hello World</text></svg>
#'                 ', target_language = "es", source_language = "en", format = "svg")
#' }
#' @export
wmcloud_translate <- function(content,
                              target_language = "en",
                              source_language = "en",
                              format = "text",
                              model = "nllb200-600M") {
  # Define the URL of the API
  url <- "https://translate.wmcloud.org/api/translate"

  # List of valid formats
  valid_formats <- c("html", "json", "markdown", "svg", "text", "webpage")

  # List of valid models
  valid_models <- c("nllb200-600M", "nllb-wikipedia", "opusmt-en-bi", "opusmt-en-bcl", "opusmt-en-to", "opusmt-en-chr", "opusmt-en-guw", "opusmt-en-srn", "opusmt-en-ty", "opusmt-en-ve", "opusmt-sv-fi", "softcatala", "indictrans2-indic-en", "indictrans2-en-indic", "indictrans2-indic-indic", "madlad-400")

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
    content = content
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

  # Extract the translated content from the response
  translation <- result$translation

  return(translation)
}

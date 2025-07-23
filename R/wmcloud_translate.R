#' Translate content using WMCloud
#'
#' This function sends a POST request to the WMCloud translation API with the specified parameters,
#' parses the JSON response, and returns the translated content. For HTML and SVG formats, it extracts
#' translatable text, translates it, and reconstructs the original structure.
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

  # Handle HTML format with special processing
  if (format == "html") {
    return(translate_html_content(content, target_language, source_language, model, url))
  }
  
  # Handle SVG format with special processing
  if (format == "svg") {
    return(translate_svg_content(content, target_language, source_language, model, url))
  }

  # For other formats, use the API directly
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

# Helper function to translate HTML content
translate_html_content <- function(content, target_language, source_language, model, url) {
  # Parse HTML content
  doc <- rvest::read_html(content)
  
  # Extract all text content from HTML elements
  text_elements <- rvest::html_nodes(doc, "*")
  text_contents <- rvest::html_text(text_elements, trim = TRUE)
  
  # Filter out empty text content
  non_empty_texts <- text_contents[stringr::str_trim(text_contents) != ""]
  
  # If there's no text content, return original
  if (length(non_empty_texts) == 0) {
    return(content)
  }
  
  # Get unique text contents for translation
  unique_texts <- unique(non_empty_texts)
  
  # Build translation mapping
  translation_map <- list()
  for (text_content in unique_texts) {
    # Skip very short or meaningless content
    if (nchar(stringr::str_trim(text_content)) < 2) {
      translation_map[[text_content]] <- text_content
      next
    }
    
    # Translate the text content
    body <- list(
      source_language = source_language,
      target_language = target_language,
      format = "text",
      model = model,
      content = text_content
    )
    
    json_body <- jsonlite::toJSON(body, auto_unbox = TRUE)
    headers <- c("Content-Type" = "application/json")
    
    response <- httr::POST(url, body = json_body, httr::add_headers(headers))
    
    if (httr::status_code(response) != 200) {
      stop("Request failed with status ", httr::status_code(response))
    }
    
    result <- httr::content(response, "parsed")
    translated_text <- result$translation
    
    translation_map[[text_content]] <- translated_text
  }
  
  # Replace text content in HTML
  translated_html <- content
  for (original_text in names(translation_map)) {
    translated_text <- translation_map[[original_text]]
    if (original_text != translated_text) {
      # Create regex pattern to match text content between tags
      pattern <- paste0("(>\\s*)", stringr::str_escape(original_text), "(\\s*<)")
      replacement <- paste0("\\1", translated_text, "\\2")
      translated_html <- stringr::str_replace_all(translated_html, pattern, replacement)
    }
  }
  
  return(translated_html)
}

# Helper function to translate SVG content  
translate_svg_content <- function(content, target_language, source_language, model, url) {
  # Parse SVG content using rvest
  doc <- rvest::read_html(content)
  
  # Extract text elements from SVG
  text_elements <- rvest::html_nodes(doc, "text")
  
  # If no text elements found, return original
  if (length(text_elements) == 0) {
    return(content)
  }
  
  # Extract all text contents
  text_contents <- rvest::html_text(text_elements)
  text_contents <- text_contents[stringr::str_trim(text_contents) != ""]
  
  # If no meaningful text content, return original
  if (length(text_contents) == 0) {
    return(content)
  }
  
  # Translate each text content and build replacement mapping
  translated_svg <- content
  for (text_content in text_contents) {
    # Skip empty text
    if (is.null(text_content) || stringr::str_trim(text_content) == "") {
      next
    }
    
    # Translate the text content
    body <- list(
      source_language = source_language,
      target_language = target_language,
      format = "text",
      model = model,
      content = text_content
    )
    
    json_body <- jsonlite::toJSON(body, auto_unbox = TRUE)
    headers <- c("Content-Type" = "application/json")
    
    response <- httr::POST(url, body = json_body, httr::add_headers(headers))
    
    if (httr::status_code(response) != 200) {
      stop("Request failed with status ", httr::status_code(response))
    }
    
    result <- httr::content(response, "parsed")
    translated_text <- result$translation
    
    # Replace the text content in the SVG
    pattern <- paste0("(<text[^>]*>)", stringr::str_escape(text_content), "(</text>)")
    replacement <- paste0("\\1", translated_text, "\\2")
    translated_svg <- stringr::str_replace(translated_svg, pattern, replacement)
  }
  
  return(translated_svg)
}

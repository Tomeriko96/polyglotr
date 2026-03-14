#' Translate content using WMCloud
#'
#' This function sends a POST request to the WMCloud translation API with the specified parameters,
#' parses the JSON response, and returns the translated content.
#'
#' @param content The content to translate. Can be plain text, a URL (for a webpage), a JSON string, or a Markdown string.
#' @param target_language The target language for the translation (default is "en").
#' @param source_language The source language of the content (default is "en").
#' @param format The format of the content ("json", "markdown", "text", "webpage").
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
#' }
#' @export
wmcloud_translate <- function(content,
                              target_language = "en",
                              source_language = "en",
                              format = "text",
                              model = "nllb200-600M") {
  valid_formats <- c("json", "markdown", "text", "webpage")
  valid_models  <- c(
    "nllb200-600M", "nllb-wikipedia", "opusmt-en-bi", "opusmt-en-bcl",
    "opusmt-en-to", "opusmt-en-chr", "opusmt-en-guw", "opusmt-en-srn",
    "opusmt-en-ty", "opusmt-en-ve", "opusmt-sv-fi", "softcatala",
    "indictrans2-indic-en", "indictrans2-en-indic", "indictrans2-indic-indic",
    "madlad-400"
  )

  if (!format %in% valid_formats) {
    stop("Invalid format. Must be one of: ", paste(valid_formats, collapse = ", "))
  }
  if (!model %in% valid_models) {
    stop("Invalid model. Must be one of: ", paste(valid_models, collapse = ", "))
  }

  result <- http_post_json(
    "https://translate.wmcloud.org/api/translate",
    body = list(
      source_language = source_language,
      target_language = target_language,
      format          = format,
      model           = model,
      content         = content
    )
  )

  result$translation
}


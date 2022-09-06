file_translate <- function(file, target_language = "en", source_language = "auto") {
  text <- readr::read_file(file)
  # text <- gsub("#", "%23 ", text)
  # text <- gsub("\r\n", "%5Cr%5Cn ", text)
  # text <- gsub("\n", "%5Cn", text)
  # text <- gsub("\r", "%5Cr", text)


  text <- gsub("\r", " ", text)
  # formatted_text <- stringr::str_replace_all(text, " ", "%20")
  # formatted_text <- paste(formatted_text, collapse = " ")
  #

  formatted_link <- paste0("https://translate.google.com/m?tl=",
                           target_language, "&sl=", source_language,
                           "&q=",
                           gsub('\\s+', '+', enc2utf8(text)))
  #
  #
  #
  # return(formatted_link)
  response <- httr::GET(formatted_link)
  #
  translation <- httr::content(response) %>%
    rvest::html_nodes("div.result-container") %>%
    rvest::html_text()
  #
  translation <- paste(translation, collapse = "\n")
  fileConn <- file("R/testos.R")
  cat(text, file = "R/testos.R")
  close(fileConn)
  #
  return(translation)



}


# file_translate("R/test.txt", target_language = "iw")

# file_translate("R/google_translate.R", target_language = "es")
#
# file.edit("R/testos.R")
#
# file.remove("R/testos.R")


fileConn <- file("R/testos.R")
text <- readr::read_file("R/google_translate.R")
cat(text)

text <- gsub("\r", " ", text)

target_language = "es"
source_language = "en"

cat(enc2utf8(text))
text <- gsub("&", "+%26", text)
text <- gsub("#", "%23", text)
text <- gsub("\r\n", "%5Cr%5Cn ", text)
text <- gsub("\n", "%5Cn", text)
text <- gsub("\r", "%5Cr", text)
# text <- stringr::str_replace_all(text, ",", "")


cat(text)
# text2 <- gsub("&", "+\%26", text)
# cat(text2)

formatted_link <- paste0("https://translate.google.com/m?tl=",
                         target_language, "&sl=", source_language,
                         "&q=",
                         gsub('\\s+', '+', enc2utf8(text)))



formatted_link

response <- httr::GET(formatted_link)

translation <- httr::content(response) %>%
  rvest::html_nodes("div.result-container") %>%
  rvest::html_text()

translation <- gsub("+%26", "&", translation)
translation <- gsub("%23", "#", translation)
translation <- gsub("%5Cr%5Cn ", "\r\n", translation)
translation <- gsub("%5Cn", "\n", translation)
translation <- gsub("%5Cr", "\r", translation)

translation <- gsub('\\+', ' ', translation)
# translation <- stringr::str_replace_all(translation, "+", "")

cat(translation)

# text <- paste(text, collapse = "")
cat(translation, file = fileConn)
# writeLines("# ' Traducir texto usando google translate\r\n # '\r\n # ' @param text Texto a traducir.\r\n # ' @param target_language Idioma al que traducir el texto.\r\n # ' @param source_language Idioma para traducir el texto de\r\n # '\r\n # ' @return Texto traducido.\r\n # ' @export\r\n # '\r\n # ' @examples\r\n # ' \\dontrun{\r\n # 'google_translate(\"Me encantan los idiomas\", target_language = \"es\")\r\n # '}\r\n google_translate <- function(text, target_language = \" en\", source_language = \"auto\") {\r\n formatted_text <- stringr::str_replace_all(text, \" \", \" \")\r\n \r\n formatted_link <- paste0( \"https://translate.google.com/m?tl=\",\r\n target_language, \"",
#            fileConn)
close(fileConn)
file.edit("R/testos.R")
# %23%27+Translate+text+using+google+translate++%23%27++%23%27+%40param+text+Text+to+translate.++%23%27+%40param+target_language+Language+to+translate+text+to.++%23%27+%40param+source_language+Language+to+translate+text+from++%23%27++%23%27+%40return+Translated+text.++%23%27+%40export++%23%27++%23%27+%40examples++%23%27%5Cdontrun%7B++%23%27google_translate%28%22I+love+languages%22%2C+target_language+%3D+%22es%22%29++%23%27%7D++google_translate+%3C-+function%28text%2C+target_language+%3D+%22en%22%2C+source_language+%3D+%22auto%22%29+%7B++++formatted_text+%3C-+stringr%3A%3Astr_replace_all%28text%2C+%22+%22%2C+%22%2520%22%29++++++formatted_link+%3C-+paste0%28%22https%3A%2F%2Ftranslate.google.com%2Fm%3Ftl%3D%22%2C+++++++++++++++++++++++++++++target_language%2C+%22%26sl%3D%22%2C+source_language%2C+++++++++++++++++++++++++++++%22%26q%3D%22%2C+++++++++++++++++++++++++++++formatted_text%29++++++response+%3C-+httr%3A%3AGET%28formatted_link%29++++++translation+%3C-+httr%3A%3Acontent%28response%29+%25%3E%25++++++rvest%3A%3Ahtml_nodes%28%22div.result-container%22%29+%25%3E%25++++++rvest%3A%3Ahtml_text%28%29++++++return%28translation%29++++%7D+
file.remove("R/testos.R")


# https://stackoverflow.com/questions/28805981/avoid-url-encoding-in-r
# address <- "Søholmen 9, 4500 Denmark"
# u <- sprintf("http://maps.googleapis.com/maps/api/geocode/json?address=%s",
#              gsub('\\s+', '+', enc2utf8(address)))


text <- readr::read_file("R/google_translate.R")

text <- gsub("#", "%23", text)
text
u <- paste0("https://translate.google.com/m?tl=es&sl=es&q=", gsub('\\s+', '+', enc2utf8(text)))

gsub('\\s+', '+', enc2utf8(text))
u
response <- httr::GET(u)

translation <- httr::content(response) %>%
  rvest::html_nodes("div.result-container") %>%
  rvest::html_text()

translation

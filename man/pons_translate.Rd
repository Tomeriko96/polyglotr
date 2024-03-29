% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pons_translate.R
\name{pons_translate}
\alias{pons_translate}
\title{Translate text using PONS}
\usage{
pons_translate(text, target_language = "pt", source_language = "en")
}
\arguments{
\item{text}{This is the text that you want to translate. Can be a single string or a vector of strings.}

\item{target_language}{This is the language that you want to translate the text into.
The default value for this argument is "pt" for Portuguese.}

\item{source_language}{This is the language of the text that you want to translate.
The default value for this argument is "en" for English.}
}
\value{
Translated text. If the input is a vector, it returns a character vector of translated strings.
}
\description{
Translate text using PONS
}
\examples{
\dontrun{
pons_translate("I love languages!", target_language = "pt", source_language = "en")
text_to_translate <- c("The", "Greatest", "Language")
pons_translate(text_to_translate, "pt", "en")
}
}

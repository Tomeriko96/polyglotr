# polyglotr <a href='https://github.com/Tomeriko96/polyglotr'><img src="man/figures/hex-polyglotr.png" style="float:right; height:200px;" height="200" align="right"/></a>

[![CodeFactor](https://www.codefactor.io/repository/github/Tomeriko96/polyglotr/badge)](https://www.codefactor.io/repository/github/Tomeriko96/polyglotr)
[![Codecov test coverage](https://codecov.io/gh/tomeriko96/polyglotr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tomeriko96/polyglotr?branch=main)
[![R-CMD-check](https://github.com/Tomeriko96/polyglotr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Tomeriko96/polyglotr/actions/workflows/R-CMD-check.yaml) [![CRAN status](https://www.r-pkg.org/badges/version/polyglotr)](https://CRAN.R-project.org/package=polyglotr/) <a href="https://diffify.com/R/polyglotr" target="_blank"><img src="https://diffify.com/diffify-badge.svg" alt="The diffify page for the R package polyglotr" style="width: 100px; max-width: 100%;"/></a> [![CRAN last month downloads](https://cranlogs.r-pkg.org/badges/last-month/polyglotr?color=green/)](https://cran.r-project.org/package=polyglotr/) [![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/polyglotr?color=green/)](https://cran.r-project.org/package=polyglotr/)

R package to translate text.

# Overview

The `polyglotr` package is a language translation tool for the R programming language. It provides convenient functions to translate text using different (free) translation services. This vignette will guide you through the usage of the package and demonstrate how to translate text and files in various languages.

Currently, the package has functions to communicate with the following services:

-   [Google Translate](https://translate.google.com/m) API
-   [Mymemory](https://mymemory.translated.net/) API
-   [Linguee](https://www.linguee.com/) API
-   [Pons](https://en.pons.com/translate) API
-   [QCRI](https://mt.qcri.org/api/) API
-   [Wikimedia Translation](https://translate.wmcloud.org/) API

# Features
To see which functions are available, please refer to the reference page of the `polyglotr` package. The [reference](https://Tomeriko96.github.io/polyglotr/reference/index.html) page provides a comprehensive list of functions available in the package, organized by category.

Additionally, the package includes vignettes that provide more detailed information on how to use the functions in the package.

# Installation

To install the package:

```{r}
# The easiest way is to get polyglotr from CRAN
install.packages("polyglotr")

# Alternatively, install the development version from GitHub
# install.packages("remotes")
remotes::install_github("Tomeriko96/polyglotr")
```

# Usage

To demonstrate the usage of the `polyglotr` package, let's consider an example where we translate a list of texts into multiple languages using the `create_translation_table` function.

```{r}
texts <- c("Hello, how are you?", 
           "I love programming!", 
           "This is a test.")

languages <- c("es", 
              "fr", 
              "de")


create_translation_table(texts, languages)


#>        Original_word                     es
#> 1 Hello, how are you?     ¿Hola, cómo estás?
#> 2 I love programming! ¡Me encanta programar!
#> 3     This is a test.    Esto es una prueba.
#>                           fr                       de
#> 1 Bonjour comment allez-vous?   Hallo, wie geht's dir?
#> 2        J'adore programmer ! Ich liebe Programmieren!
#> 3              C'est un test.        Das ist ein Test.

```

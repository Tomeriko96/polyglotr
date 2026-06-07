# polyglotr \| Multilingual Text Translation

![polyglotr](reference/figures/hex-polyglotr.png)

**Versatile R Package for Text Translation**  
Seamless integration with multiple free translation services for R
users.  
_(Ideal for translating text, files, and creating multilingual datasets.)

### [CRAN](https://CRAN.R-project.org/package=polyglotr/)  \|  [Features](#features)  \|  [Installation](#installation)  \|  [Usage](#usage)  \|  [Contribution](#contribution)

_(Built by [Tomeriko96](https://github.com/Tomeriko96) and [contributors](https://github.com/Tomeriko96/polyglotr/graphs/contributors).)

[![Codecov test
coverage](https://codecov.io/gh/tomeriko96/polyglotr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tomeriko96/polyglotr?branch=main)
[![R-CMD-check](https://github.com/Tomeriko96/polyglotr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Tomeriko96/polyglotr/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/polyglotr)](https://CRAN.R-project.org/package=polyglotr/)
[![CRAN last month
downloads](https://cranlogs.r-pkg.org/badges/last-month/polyglotr?color=green/)](https://cran.r-project.org/package=polyglotr/)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/polyglotr?color=green/)](https://cran.r-project.org/package=polyglotr/)
[![The diffify page for the R package
polyglotr](https://diffify.com/diffify-badge.svg)](https://diffify.com/R/polyglotr)
[![Shiny App
Deployed](https://img.shields.io/badge/Shiny%20App-Live-blue?logo=R)](https://edulytics.shinyapps.io/polyglotr/)

## Table of Contents

- [Overview](#overview-)
- [Features](#features-)
- [Installation](#installation-)
- [Quick Start](#quick-start-)
- [Shiny Web App](#shiny-web-app-)
- [Usage](#usage-)
- [Troubleshooting](#troubleshooting-)
- [Contribution](#contribution-)
- [License](#license-%EF%B8%8F)
- [Citation](#citation-)
- [Related Projects](#related-projects-)

# Overview

The `polyglotr` package is a language translation tool for the R
programming language. It provides convenient functions to translate text
using different (free) translation services. This vignette will guide
you through the usage of the package and demonstrate how to translate
text and files in various languages.

Currently, the package has functions to communicate with the following
services:

- [Google Translate](https://translate.google.com/m) API
- [Apertium](https://apertium.org/apy/) API
- [Mymemory](https://mymemory.translated.net/) API
- [Linguee](https://www.linguee.com/) API
- [Pons](https://en.pons.com/translate) API
- [QCRI](https://www.qcri.org/) API
- [Wikimedia Translation](https://translate.wmcloud.org/) API

## Why polyglotr?

`polyglotr` offers a streamlined translation experience for R users:

- **No API Keys Required** - Start translating immediately after
  installation
- **Multiple Services** - Access 7 translation services through one
  interface
- **R-Native Integration** - Seamlessly incorporate translation into
  data workflows
- **Flexible Usage** - Translate single phrases, datasets, or entire
  files
- **Open Source** - Community-driven with comprehensive documentation

# Features

- Translate text using multiple free translation services
- Batch translation capabilities
- Language detection
- Create translation tables for multiple languages
- File translation
- Easy-to-use interface for different translation APIs
- **NEW: Interactive Shiny web application for non-R users**

To see which functions are available, please refer to the reference page
of the `polyglotr` package. The
[reference](https://Tomeriko96.github.io/polyglotr/reference/index.html)
page provides a comprehensive list of functions available in the
package, organized by category.

Additionally, the package includes vignettes that provide more detailed
information on how to use the functions in the package.

# Installation

## System Requirements

- R (\>= 3.6.0)
- Internet connection for translation services
- Optional for Shiny app: `shiny`, `shinydashboard`, `DT`

## Install Package

\`\`\`{r} \# The easiest way is to get polyglotr from CRAN
install.packages(“polyglotr”)

# Alternatively, install the development version from GitHub

# install.packages(“remotes”)

remotes::install_github(“Tomeriko96/polyglotr”)




    # Quick Start

    Here's a minimal example to get you started:

    ```{r}
    library(polyglotr)

    # Translate a simple phrase using Google Translate
    text <- "Hello, world!"
    translation <- google_translate(text, target_language = "fr")
    print(translation)

    # Translate using Apertium (no API key required)
    translation_apertium <- apertium_translate(text, target_language = "es", source_language = "en")
    print(translation_apertium)

# Shiny Web App

[![Shiny App
Deployed](https://img.shields.io/badge/Shiny%20App-Live-blue?logo=R)](https://edulytics.shinyapps.io/polyglotr/)

`polyglotr` includes a web application for translation services,
accessible to users without R programming knowledge.

## Launching the App

``` r

library(polyglotr)
launch_polyglotr_app()
# Or with custom settings
launch_polyglotr_app(port = 3838, launch.browser = TRUE)
```

## App Features

- Multiple translation services: Google Translate, MyMemory, PONS,
  Linguee, QCRI, Apertium, Wikimedia Cloud
- Language detection
- Dynamic language selection based on service
- User-friendly dashboard interface
- No coding required

## Installation Requirements

To use the Shiny app, install these additional packages:

``` r

install.packages(c("shiny", "shinydashboard", "DT"))
```

# Usage

To demonstrate the usage of the `polyglotr` package, let’s consider an
example where we translate a list of texts into multiple languages using
the `create_translation_table` function.

\`\`\`{r} texts \<- c(“Hello, how are you?”, “I love programming!”,
“This is a test.”)

languages \<- c(“es”, “fr”, “de”)

create_translation_table(texts, languages)

\#\> Original_word es \#\> 1 Hello, how are you? ¿Hola, cómo estás? \#\>
2 I love programming! ¡Me encanta programar! \#\> 3 This is a test. Esto
es una prueba. \#\> fr de \#\> 1 Bonjour comment allez-vous? Hallo, wie
geht’s dir? \#\> 2 J’adore programmer ! Ich liebe Programmieren! \#\> 3
C’est un test. Das ist ein Test.



    # Troubleshooting

    ## Common Issues

    **Translation fails or returns empty results**
    - Check your internet connection
    - Verify the target language is supported by the chosen service
    - Some services have rate limits - try again later

    **Shiny app won't launch**
    - Ensure required packages are installed: `install.packages(c("shiny", "shinydashboard", "DT"))`
    - Check if the specified port is available

    **Language detection not working**
    - Not all services support language detection
    - Try using a different translation service

    ## Getting Help

    - [Package Website](https://Tomeriko96.github.io/polyglotr/)
    - [Reference Manual](https://Tomeriko96.github.io/polyglotr/reference/index.html)
    - [Vignettes](https://Tomeriko96.github.io/polyglotr/articles/)
    - [GitHub Issues](https://github.com/Tomeriko96/polyglotr/issues)



    # Contribution

    Contributions are welcome! If you'd like to contribute, please:

    1. Fork the repository
    2. Create your feature branch (git checkout -b feature/AmazingFeature)
    3. Commit your changes (git commit -m 'Add some AmazingFeature')
    4. Push to the branch (git push origin feature/AmazingFeature)
    5. Open a Pull Request


    # License

    This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.



    # Citation

    If you use polyglotr in your research, please cite it as follows:

Iwan, T. (2023). polyglotr: Multilingual Text Translation in R. R
package version 1.0.0. <https://github.com/Tomeriko96/polyglotr> \`\`\`

# Related Projects

- [googleLanguageR](https://github.com/ropensci/googleLanguageR): R
  client for the Google Translation API, Cloud Natural Language API,
  Cloud Speech API, and Cloud Text-to-Speech API

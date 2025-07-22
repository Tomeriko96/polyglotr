# polyglotr Shiny App

This directory contains a Shiny web application that provides a user-friendly interface to the `polyglotr` package translation services.

## Features

- **Multiple Translation Services**: Access to Google Translate, MyMemory, PONS, Linguee, QCRI, Apertium, and Wikimedia Cloud
- **Language Detection**: Automatic detection of source language
- **Dynamic Language Selection**: Language options update based on selected service
- **User-Friendly Interface**: Clean dashboard design with intuitive controls
- **Error Handling**: Helpful error messages and service information

## Running the App

### Option 1: Using the helper function (recommended)

From R, after installing the `polyglotr` package:

```r
library(polyglotr)
launch_polyglotr_app()
```

### Option 2: Running directly

Navigate to this directory and run:

```r
shiny::runApp(".")
```

### Option 3: Using app.R

From this directory:

```r
source("app.R")
```

## Requirements

The following R packages are required:
- `shiny`
- `shinydashboard` 
- `shinyjs`
- `DT`
- `polyglotr`

Install missing packages with:
```r
install.packages(c("shiny", "shinydashboard", "DT", "shinyjs"))
```

## Usage

1. **Select Translation Service**: Choose from the dropdown menu
2. **Enter Text**: Type or paste text in the input area
3. **Select Languages**: Choose source and target languages
4. **Detect Language** (optional): Use auto-detection for source language
5. **Translate**: Click the translate button to get results

## API Keys

Some services require API keys:
- **QCRI**: Register at https://mt.qcri.org/api/register

## Service Information

Each translation service has different capabilities:

- **Google Translate**: Best overall coverage and quality
- **MyMemory**: Free service, good for basic translations
- **PONS**: Excellent for dictionary-style translations
- **Linguee**: Shows multiple translation options with context
- **QCRI**: Research-quality, requires API key
- **Apertium**: Open-source, good for specific language pairs
- **Wikimedia Cloud**: Community-driven translations

## File Structure

- `app.R` - Main application entry point
- `ui.R` - User interface definition
- `server.R` - Server logic and translation handling
- `README.md` - This file

## Customization

The app can be customized by:
- Adding more language options in `server.R`
- Modifying the UI layout in `ui.R`
- Adding new translation services (requires corresponding `polyglotr` functions)
- Enhancing error handling and user feedback

# Main Shiny App Entry Point for polyglotr Translation Services
# 
# This file serves as the main entry point for the polyglotr Shiny application.
# It loads the required libraries and starts the application.

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(polyglotr)

# Source the UI and Server files
source("ui.R")
source("server.R")

# Run the application 
shinyApp(ui = ui, server = server)
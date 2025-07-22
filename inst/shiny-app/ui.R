# Shiny App UI for polyglotr Translation Services
# This app provides a user-friendly interface to the polyglotr package

library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)

# Define UI for the polyglotr translation app
ui <- dashboardPage(
  dashboardHeader(title = "polyglotr Translation Hub"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Translator", tabName = "translator", icon = icon("language")),
      menuItem("History", tabName = "history", icon = icon("history")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),  # Enable shinyjs for copy functionality
    
    # Custom CSS for better styling
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .fa-spin {
          animation: fa-spin 2s infinite linear;
        }
        .history-entry:hover {
          background-color: #f0f0f0;
        }
      "))
    ),
    tabItems(
      # Main translator tab
      tabItem(tabName = "translator",
        fluidRow(
          box(
            title = "Translation Settings", status = "primary", solidHeader = TRUE,
            width = 4,
            
            # Service selection
            selectInput("service", 
                       "Translation Service:",
                       choices = list(
                         "Google Translate" = "google",
                         "MyMemory" = "mymemory", 
                         "PONS Dictionary" = "pons",
                         "Linguee" = "linguee",
                         "QCRI" = "qcri",
                         "Apertium" = "apertium",
                         "Wikimedia Cloud" = "wmcloud"
                       ),
                       selected = "google"),
            
            # Source language
            selectInput("source_lang",
                       "Source Language:",
                       choices = list("Auto-detect" = "auto"),
                       selected = "auto"),
            
            # Target language  
            selectInput("target_lang",
                       "Target Language:",
                       choices = list("English" = "en"),
                       selected = "en"),
            
            # API Key input (for services that need it)
            conditionalPanel(
              condition = "input.service == 'qcri'",
              textInput("api_key", "API Key:", placeholder = "Enter your QCRI API key")
            ),
            
            # Language detection button
            actionButton("detect_lang", "Detect Source Language", 
                        class = "btn-info", width = "100%"),
            
            br(), br(),
            
            # Translate button
            actionButton("translate", "Translate", 
                        class = "btn-success", width = "100%")
          ),
          
          box(
            title = "Translation Input/Output", status = "success", solidHeader = TRUE,
            width = 8,
            
            # Input text area
            h4("Source Text:"),
            div(style = "display: flex; align-items: center; margin-bottom: 10px;",
                actionButton("load_sample", "Try Sample", 
                           class = "btn-info btn-sm", style = "margin-right: 10px;"),
                actionButton("copy_input", "Copy Input", 
                           class = "btn-default btn-sm")),
            
            textAreaInput("input_text", 
                         label = NULL,
                         placeholder = "Enter text to translate...",
                         height = "150px",
                         width = "100%"),
            
            # Language detection result
            conditionalPanel(
              condition = "output.detected_lang",
              div(id = "detection_result",
                  h5("Detected Language:"),
                  verbatimTextOutput("detected_lang"))
            ),
            
            hr(),
            
            # Translation output
            div(style = "display: flex; align-items: center; margin-bottom: 10px;",
                h4("Translation:", style = "margin: 0; margin-right: 10px;"),
                actionButton("copy_output", "Copy Translation", 
                           class = "btn-success btn-sm")),
            
            div(id = "translation_output",
                style = "min-height: 150px; border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9;",
                uiOutput("translation_result"))
          )
        ),
        
        # Service information box
        fluidRow(
          box(
            title = "Service Information & polyglotr Documentation", status = "info", solidHeader = TRUE,
            width = 8, collapsible = TRUE, collapsed = TRUE,
            
            uiOutput("service_info"),
            
            hr(),
            
            h4("Using polyglotr in R:"),
            p("This app is powered by the", strong("polyglotr"), "R package. Here are some code examples:"),
            
            h5("Basic Translation:"),
            tags$pre(tags$code('
library(polyglotr)

# Google Translate
google_translate("Hello world", target_language = "es")

# MyMemory
mymemory_translate("Hello world", target_language = "fr", source_language = "en")

# PONS Dictionary
pons_translate("house", target_language = "de", source_language = "en")
            ')),
            
            h5("Language Detection:"),
            tags$pre(tags$code('
# Detect language
language_detect("Bonjour le monde")
            ')),
            
            p("For complete documentation, visit:", 
              tags$a("polyglotr GitHub repository", 
                     href = "https://github.com/Tomeriko96/polyglotr", 
                     target = "_blank"))
          ),
          
          box(
            title = "Quick Actions", status = "warning", solidHeader = TRUE,
            width = 4, collapsible = TRUE, collapsed = TRUE,
            
            h5("Common Language Pairs:"),
            actionButton("preset_en_es", "EN → ES", class = "btn-default btn-sm", style = "margin: 2px;"),
            actionButton("preset_en_fr", "EN → FR", class = "btn-default btn-sm", style = "margin: 2px;"),
            actionButton("preset_en_de", "EN → DE", class = "btn-default btn-sm", style = "margin: 2px;"),
            actionButton("preset_es_en", "ES → EN", class = "btn-default btn-sm", style = "margin: 2px;"),
            actionButton("preset_fr_en", "FR → EN", class = "btn-default btn-sm", style = "margin: 2px;"),
            actionButton("preset_de_en", "DE → EN", class = "btn-default btn-sm", style = "margin: 2px;"),
            
            hr(),
            
            h5("Feedback:"),
            p("Found a bug or have a suggestion?"),
            tags$a("Report Issue", href = "https://github.com/Tomeriko96/polyglotr/issues", 
                   target = "_blank", class = "btn btn-primary btn-sm")
          )
        )
      ),
      
      # History tab
      tabItem(tabName = "history",
        fluidRow(
          box(
            title = "Recent Translations", status = "primary", solidHeader = TRUE,
            width = 12,
            
            p("Your recent translations are saved here for easy access."),
            
            uiOutput("history_table")
          )
        )
      ),
      
      # About tab
      tabItem(tabName = "about",
        fluidRow(
          box(
            title = "About polyglotr Shiny App", status = "primary", solidHeader = TRUE,
            width = 12,
            
            h3("Welcome to the polyglotr Translation Hub!"),
            
            p("This Shiny application provides a user-friendly web interface to the", 
              strong("polyglotr"), "R package, which offers access to multiple translation services."),
            
            h4("Available Translation Services:"),
            tags$ul(
              tags$li(strong("Google Translate:"), "Fast and accurate translations using Google's service"),
              tags$li(strong("MyMemory:"), "Free translation service with good coverage"),
              tags$li(strong("PONS Dictionary:"), "Dictionary-based translations with high quality"),
              tags$li(strong("Linguee:"), "Context-aware translations with multiple options"),
              tags$li(strong("QCRI:"), "Research-quality translations (requires API key)"),
              tags$li(strong("Apertium:"), "Open-source rule-based translation"),
              tags$li(strong("Wikimedia Cloud:"), "Community-driven translations")
            ),
            
            h4("Features:"),
            tags$ul(
              tags$li("Multiple translation service support"),
              tags$li("Automatic language detection"), 
              tags$li("Dynamic language pair selection"),
              tags$li("User-friendly web interface"),
              tags$li("No R coding required")
            ),
            
            hr(),
            
            p("Built with", em("polyglotr"), "package by Tomer Iwan."),
            p("For more information, visit:", 
              tags$a("https://github.com/Tomeriko96/polyglotr", 
                     href = "https://github.com/Tomeriko96/polyglotr", 
                     target = "_blank"))
          )
        )
      )
    )
  )
)
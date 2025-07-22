# Shiny App Server for polyglotr Translation Services

library(shiny)
library(polyglotr)
library(shinyjs)

# Define server logic for the polyglotr translation app
server <- function(input, output, session) {
  
  # Reactive values to store translation results
  values <- reactiveValues(
    translation = NULL,
    detected_language = NULL,
    error_message = NULL,
    is_translating = FALSE,
    history = list()
  )
  
  # Language options for different services
  get_language_options <- function(service) {
    switch(service,
      "google" = list(
        # Common language codes - could be enhanced with actual Google supported languages
        "Auto-detect" = "auto", "English" = "en", "Spanish" = "es", "French" = "fr", 
        "German" = "de", "Italian" = "it", "Portuguese" = "pt", "Russian" = "ru",
        "Chinese" = "zh", "Japanese" = "ja", "Korean" = "ko", "Arabic" = "ar",
        "Dutch" = "nl", "Swedish" = "sv", "Norwegian" = "no", "Danish" = "da"
      ),
      "mymemory" = list(
        # MyMemory doesn't support auto-detect, removed "auto" option
        "English" = "en", "Spanish" = "es", "French" = "fr",
        "German" = "de", "Italian" = "it", "Portuguese" = "pt", "Russian" = "ru"
      ),
      "pons" = list(
        "English" = "en", "German" = "de", "French" = "fr", "Spanish" = "es",
        "Italian" = "it", "Portuguese" = "pt", "Russian" = "ru", "Polish" = "pl"
      ),
      "linguee" = list(
        "Auto-detect" = "auto", "English" = "en", "German" = "de", "French" = "fr", "Spanish" = "es",
        "Italian" = "it", "Portuguese" = "pt", "Russian" = "ru", "Chinese" = "zh"
      ),
      "qcri" = list(
        "English" = "en", "Arabic" = "ar", "Spanish" = "es", "French" = "fr"
      ),
      "apertium" = list(
        # Apertium has limited language pairs - using conservative set
        "English" = "en", "Spanish" = "es", "French" = "fr", 
        "Catalan" = "ca", "Galician" = "gl", "Portuguese" = "pt"
      ),
      "wmcloud" = list(
        "Auto-detect" = "auto", "English" = "en", "Spanish" = "es", "French" = "fr", "German" = "de",
        "Italian" = "it", "Portuguese" = "pt"
      )
    )
  }
  
  # Update language choices when service changes
  observeEvent(input$service, {
    # Prevent processing if service is NULL or empty
    if (is.null(input$service) || input$service == "") {
      return()
    }
    
    lang_options <- get_language_options(input$service)
    
    # Update source language options
    if (input$service %in% c("pons", "mymemory", "qcri", "apertium")) {
      # These services don't support auto-detect
      source_options <- lang_options[names(lang_options) != "Auto-detect"]
      selected_source <- "en"
    } else {
      source_options <- lang_options
      selected_source <- "auto"
    }
    
    updateSelectInput(session, "source_lang",
                      choices = source_options,
                      selected = selected_source)
    
    # Update target language options (exclude source language from target options)
    target_options <- lang_options[names(lang_options) != "Auto-detect"]
    updateSelectInput(session, "target_lang",
                      choices = target_options,
                      selected = "en")
  })
  
  # Language detection
  observeEvent(input$detect_lang, {
    req(input$input_text)
    
    tryCatch({
      detected <- language_detect(input$input_text)
      values$detected_language <- detected
      
      # Update source language if detection is successful
      lang_options <- get_language_options(input$service)
      if (detected %in% lang_options) {
        updateSelectInput(session, "source_lang", selected = detected)
      }
      
    }, error = function(e) {
      values$detected_language <- paste("Error:", e$message)
    })
  })
  
  # Translation function
  translate_text <- function(text, service, source_lang, target_lang, api_key = NULL) {
    switch(service,
      "google" = google_translate(text, target_language = target_lang, source_language = source_lang),
      "mymemory" = mymemory_translate(text, target_language = target_lang, source_language = source_lang),
      "pons" = pons_translate(text, target_language = target_lang, source_language = source_lang),
      "linguee" = {
        # Linguee returns multiple options, we'll take the first one
        result <- linguee_word_translation(text, target_language = target_lang, source_language = source_lang)
        if (length(result) > 0 && !is.na(result[1]) && result[1] != "") {
          result[1] 
        } else {
          "No translation found. Try a single word or different language pair."
        }
      },
      "qcri" = {
        if (is.null(api_key) || api_key == "") {
          stop("API key is required for QCRI service")
        }
        langpair <- paste0(source_lang, "-", target_lang)
        qcri_translate_text(text, langpair = langpair, domain = "general", api_key = api_key)
      },
      "apertium" = {
        tryCatch({
          apertium_translate(text, target_language = target_lang, source_language = source_lang)
        }, error = function(e) {
          paste0("Apertium translation failed. This might be due to unsupported language pair (", 
                 source_lang, " → ", target_lang, ") or service unavailability. Error: ", e$message)
        })
      },
      "wmcloud" = wmcloud_translate(text, target_language = target_lang, source_language = source_lang, format = "text"),
      stop("Unknown service")
    )
  }
  
  # Sample text options
  sample_texts <- list(
    "Hello, world! This is a demonstration of the polyglotr Shiny app.",
    "The quick brown fox jumps over the lazy dog.",
    "Good morning! How are you today?",
    "Technology is transforming our world.",
    "Welcome to the polyglotr translation service."
  )
  
  # Load sample text
  observeEvent(input$load_sample, {
    sample_text <- sample(sample_texts, 1)[[1]]
    updateTextAreaInput(session, "input_text", value = sample_text)
  })
  
  # Copy input text
  observeEvent(input$copy_input, {
    runjs(paste0("navigator.clipboard.writeText('", gsub("'", "\\'", input$input_text), "');"))
    showNotification("Input text copied to clipboard!", type = "message")
  })
  
  # Copy translation
  observeEvent(input$copy_output, {
    if (!is.null(values$translation) && values$translation != "Translating...") {
      runjs(paste0("navigator.clipboard.writeText('", gsub("'", "\\'", values$translation), "');"))
      showNotification("Translation copied to clipboard!", type = "message")
    }
  })
  
  # Main translation action
  observeEvent(input$translate, {
    req(input$input_text)
    
    # Clear previous results and set loading state
    values$translation <- NULL
    values$error_message <- NULL
    values$is_translating <- TRUE
    
    tryCatch({
      result <- translate_text(
        text = input$input_text,
        service = input$service,
        source_lang = input$source_lang,
        target_lang = input$target_lang,
        api_key = input$api_key
      )
      
      values$translation <- result
      values$is_translating <- FALSE
      
      # Add to history
      new_entry <- list(
        timestamp = Sys.time(),
        input = input$input_text,
        output = result,
        service = input$service,
        source_lang = input$source_lang,
        target_lang = input$target_lang
      )
      
      values$history <- c(list(new_entry), values$history)
      if (length(values$history) > 10) {
        values$history <- values$history[1:10]  # Keep only last 10
      }
      
    }, error = function(e) {
      values$error_message <- paste("Translation failed:", e$message)
      values$translation <- NULL
      values$is_translating <- FALSE
    })
  })
  
  # Output for detected language
  output$detected_lang <- renderText({
    values$detected_language
  })
  
  # Output for translation result
  output$translation_result <- renderUI({
    if (values$is_translating) {
      div(style = "text-align: center; color: #2196F3; font-size: 16px;",
          icon("spinner", class = "fa-spin"),
          p("Translating...", style = "margin-top: 10px;"))
    } else if (!is.null(values$error_message)) {
      div(style = "color: red;", h5("Error:"), p(values$error_message))
    } else if (!is.null(values$translation)) {
      div(style = "color: green; font-size: 16px;", p(values$translation))
    } else {
      p("Click 'Translate' to see the translation here.", style = "color: gray;")
    }
  })
  
  # History output
  output$history_table <- renderUI({
    if (length(values$history) == 0) {
      p("No recent translations.", style = "color: gray; font-style: italic;")
    } else {
      history_items <- lapply(1:min(5, length(values$history)), function(i) {
        entry <- values$history[[i]]
        
        # Validate entry has all required fields to prevent "argument is of length zero" errors
        if (is.null(entry) || 
            is.null(entry$service) || is.null(entry$source_lang) || is.null(entry$target_lang) ||
            is.null(entry$input) || is.null(entry$output) || is.null(entry$timestamp)) {
          return(div(style = "color: red; font-style: italic;", "Invalid history entry"))
        }
        
        div(class = "history-entry",
            style = "border: 1px solid #ddd; margin: 5px 0; padding: 10px; border-radius: 5px;",
            div(style = "font-weight: bold; color: #2196F3;", 
                paste(entry$service, ":", entry$source_lang, "→", entry$target_lang)),
            div(style = "margin: 5px 0; font-size: 12px; color: gray;", 
                format(entry$timestamp, "%H:%M:%S")),
            div(style = "margin: 5px 0;", 
                strong("Input: "), substr(as.character(entry$input), 1, 50), 
                if(nchar(as.character(entry$input)) > 50) "..." else ""),
            div(style = "margin: 5px 0;", 
                strong("Output: "), substr(as.character(entry$output), 1, 50),
                if(nchar(as.character(entry$output)) > 50) "..." else ""),
            actionButton(paste0("reuse_", i), "Reuse", 
                        class = "btn-xs btn-default",
                        onclick = paste0("Shiny.setInputValue('reuse_translation', ", i, ");"))
        )
      })
      do.call(tagList, history_items)
    }
  })
  
  # Handle reuse translation
  observeEvent(input$reuse_translation, {
    # Validate the reuse_translation index
    if (input$reuse_translation > length(values$history) || input$reuse_translation < 1) {
      return()
    }
    
    entry <- values$history[[input$reuse_translation]]
    
    # Validate entry exists and has required fields
    if (is.null(entry) || is.null(entry$service) || is.null(entry$input)) {
      return()
    }
    
    # Only update if the entry is valid and different from current selections
    if (entry$service != input$service) {
      updateSelectInput(session, "service", selected = entry$service)
    }
    
    if (entry$input != input$input_text) {
      updateTextAreaInput(session, "input_text", value = entry$input)
    }
    # Language selection will be updated by the service change observer
  })
  
  # Quick language preset buttons
  observeEvent(input$preset_en_es, {
    updateSelectInput(session, "source_lang", selected = "en")
    updateSelectInput(session, "target_lang", selected = "es")
  })
  
  observeEvent(input$preset_en_fr, {
    updateSelectInput(session, "source_lang", selected = "en")
    updateSelectInput(session, "target_lang", selected = "fr")
  })
  
  observeEvent(input$preset_en_de, {
    updateSelectInput(session, "source_lang", selected = "en")
    updateSelectInput(session, "target_lang", selected = "de")
  })
  
  observeEvent(input$preset_es_en, {
    updateSelectInput(session, "source_lang", selected = "es")
    updateSelectInput(session, "target_lang", selected = "en")
  })
  
  observeEvent(input$preset_fr_en, {
    updateSelectInput(session, "source_lang", selected = "fr")
    updateSelectInput(session, "target_lang", selected = "en")
  })
  
  observeEvent(input$preset_de_en, {
    updateSelectInput(session, "source_lang", selected = "de")
    updateSelectInput(session, "target_lang", selected = "en")
  })
  
  # Service information
  output$service_info <- renderUI({
    service_descriptions <- list(
      "google" = "Google Translate provides fast, neural machine translation for over 100 languages. Supports automatic language detection.",
      "mymemory" = "MyMemory is the world's largest translation memory. Free service with good language coverage. Note: Does not support automatic language detection - please select source language manually.",
      "pons" = "PONS offers dictionary-based translations with high accuracy. Particularly good for European languages. Best for individual words and short phrases.",
      "linguee" = "Linguee provides context-aware translations by showing how words are used in real documents and websites. Works best with single words.",
      "qcri" = "QCRI (Qatar Computing Research Institute) provides research-quality translations. Requires API key registration.",
      "apertium" = "Apertium is a free, open-source rule-based machine translation platform. Limited language pairs available - works best with closely related languages (e.g., Spanish-Catalan, Spanish-Galician).",
      "wmcloud" = "Wikimedia Cloud Services provides community-driven translations leveraging Wikipedia's multilingual content."
    )
    
    api_info <- list(
      "qcri" = "Register for a free API key at: https://mt.qcri.org/api/register",
      "mymemory" = "MyMemory does not support automatic language detection. Please select both source and target languages manually.",
      "apertium" = "Apertium supports limited language pairs. If translation fails, try a different language pair or service."
    )
    
    description <- service_descriptions[[input$service]]
    extra_info <- api_info[[input$service]]
    
    div(
      p(description),
      if (!is.null(extra_info)) {
        div(style = "background-color: #fff3cd; padding: 10px; border-left: 4px solid #ffc107; margin-top: 10px;",
            strong("Important: "), extra_info)
      }
    )
  })
}
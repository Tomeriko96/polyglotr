#' Launch polyglotr Shiny App
#'
#' This function launches the Shiny web application for polyglotr,
#' providing a user-friendly interface to access multiple translation services.
#'
#' @param launch.browser Logical. Should the app be launched in the default browser?
#'   Default is TRUE.
#' @param port The port number for the Shiny app. Default is NULL (auto-assign).
#' @param host The host IP address. Default is "127.0.0.1".
#'
#' @return Starts the Shiny application. No return value.
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch the app in default browser
#' launch_polyglotr_app()
#' 
#' # Launch on specific port
#' launch_polyglotr_app(port = 3838)
#' 
#' # Launch without opening browser
#' launch_polyglotr_app(launch.browser = FALSE)
#' }
launch_polyglotr_app <- function(launch.browser = TRUE, port = NULL, host = "127.0.0.1") {
  
  # Check if required packages are available
  required_packages <- c("shiny", "shinydashboard", "DT", "shinyjs")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop("Required packages are missing: ", paste(missing_packages, collapse = ", "), 
         "\nPlease install them with: install.packages(c(", 
         paste(paste0("'", missing_packages, "'"), collapse = ", "), "))")
  }
  
  # Get the path to the Shiny app
  app_dir <- system.file("shiny-app", package = "polyglotr")
  
  if (app_dir == "") {
    stop("Shiny app not found. Please ensure the polyglotr package is properly installed.")
  }
  
  # Launch the app
  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch.browser,
    port = port,
    host = host
  )
}
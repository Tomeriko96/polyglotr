# Example: How to launch the polyglotr Shiny App
# 
# This example demonstrates how to use the new Shiny app functionality
# that was added to the polyglotr package.

# First, load the polyglotr package
library(polyglotr)

# Option 1: Launch the app with default settings (opens in browser)
# launch_polyglotr_app()

# Option 2: Launch on a specific port
# launch_polyglotr_app(port = 3838)

# Option 3: Launch without opening the browser automatically
# launch_polyglotr_app(launch.browser = FALSE)

# Option 4: Launch with custom host and port settings
# launch_polyglotr_app(host = "0.0.0.0", port = 8080, launch.browser = FALSE)

cat("To launch the polyglotr Shiny app, run:\n")
cat("library(polyglotr)\n")
cat("launch_polyglotr_app()\n\n")

cat("Features of the Shiny app:\n")
cat("- Multiple translation services (Google, MyMemory, PONS, Linguee, QCRI, Apertium, WMCloud)\n")
cat("- Language detection\n")
cat("- Dynamic language selection based on service\n")
cat("- User-friendly dashboard interface\n")
cat("- No R coding required for end users\n")
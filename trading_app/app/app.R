# ==============================================================================
# app.R -- Entry point. Open this file in RStudio and click "Run App"
# (or run shiny::runApp() from within the app/ folder) to start the app.
# ==============================================================================

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)

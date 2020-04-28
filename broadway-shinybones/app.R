# Load Libraries -----
library(shiny)
library(shinydashboard)
library(shinybones)
library(shinymetrics)

# Load Utilities -----
source_dirs('utils')
source_dirs('components')
source_dirs('pages')

# Global Data ----
# This is passed to all page modules as an argument named data_global
DATA <- readRDS("show_metrics.rds")

# Configuration
options("yaml.eval.expr" = TRUE)
CONFIG <- yaml::read_yaml('_site.yml')

# UI ----
ui <- function(request){
  dashboardPage(
    # Header ----
    dashboardHeader(title = CONFIG$name),

    # Sidebar ----
    dashboardSidebar(
      sb_create_sidebar(CONFIG, DATA)
    ),

    # Body -----
    dashboardBody(
      sb_create_tab_items(CONFIG, DATA)
    )
  )
}

# Server -----
server <- function(input, output, session){
  sb_call_modules(CONFIG, DATA)
}

# Run App ----
shinyApp(ui = ui, server = server, enableBookmarking = 'url')

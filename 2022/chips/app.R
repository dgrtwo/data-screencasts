

library(shiny)
library(tidyverse)
library(plotly)

numeric_choices_original <- raw_chips %>%
  select(-1) %>%
  select_if(is.numeric) %>%
  colnames()

numeric_choices_cleaned <- janitor::make_clean_names(numeric_choices_original)
names(numeric_choices_cleaned) <- numeric_choices_original

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectizeInput("type",
                     selected = c("CPU", "GPU"),
                     choices = c("CPU", "GPU"),
                     multi = TRUE,
                     label = "Types"),
      selectizeInput("y_axis",
                     selected = "transistors_millions",
                     choices = numeric_choices_cleaned,
                     label = "Y-Axis"),
      checkboxInput("log_y",
                    label = "Log Y-Axis",
                    value = TRUE)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("chipsPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$chipsPlot <- renderPlotly({
    y_axis_label <- numeric_choices_original[numeric_choices_cleaned == input$y_axis]
    
    g <- chips %>%
      filter(type %in% input$type) %>%
      ggplot(aes(release_date,
                 !!sym(input$y_axis),
                 text = product)) +
      geom_point() +
      geom_smooth(method = "loess") +
      labs(x = "Chip release date",
           y = y_axis_label)
    
    if (input$log_y) {
      g <- g + scale_y_log10()
    }
    
    ggplotly(g, tooltip = "text")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

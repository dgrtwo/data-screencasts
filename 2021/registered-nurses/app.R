library(tidyverse)
library(shiny)
library(plotly)

nurses <- tidytuesdayR::tt_load("2021-10-05")$nurses %>%
    janitor::clean_names()

### Pre-processing
unique_states <- fct_reorder(nurses$state,
                             nurses$total_employed_rn,
                             .desc = TRUE,
                             na.rm = TRUE) %>%
    levels()

metric_names <- c("Hourly wage (median)" = "hourly_wage_median",
                  "Hourly wage (average)" = "hourly_wage_avg",
                  "Annual salary (median)" = "annual_salary_median",
                  "Annual salary (average)" = "annual_salary_avg",
                  "Total employed nurses" = "total_employed_rn")

nurses_map_data <- nurses %>%
    mutate(state = str_to_lower(state)) %>%
    inner_join(map_data("state"), by = c(state = "region"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Registered Nurses"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("metric",
                        "Metric",
                        choices = metric_names),
            conditionalPanel(condition = "input.tabselected==1",
                             selectizeInput("states",
                                            "States",
                                            choices = unique_states,
                                            selected = c("New York", "California",
                                                         "Texas", "Pennsylvania",
                                                         "Ohio", "Washington"),
                                            multiple = TRUE)),
            conditionalPanel(condition = "input.tabselected==2",
                             sliderInput("year",
                                         "Year",
                                         min = min(nurses$year),
                                         max = max(nurses$year),
                                         value = max(nurses$year),
                                         sep = ""))
        ),
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel("Over Time", plotlyOutput("wages_plot"), value = 1),
                tabPanel("Map", plotlyOutput("map_plot"), value = 2),
                id = "tabselected"
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    label_format <- reactive({
        if (input$metric %in% c("total_employed_rn")) {
            comma_format()
        } else {
            dollar_format()
        }
    })

    output$wages_plot <- renderPlotly({
        req(input$states)
        
        # generate bins based on input$bins from ui.R
        g <- nurses %>%
            filter(state %in% input$states) %>%
            filter(!is.na(!!sym(input$metric))) %>%
            ggplot(aes(year, !!sym(input$metric), color = state)) +
            geom_line() +
            expand_limits(y = 0) +
            labs(x = "Year",
                 y = "",
                 color = "") +
            scale_y_continuous(labels = label_format())
        
        ggplotly(g)
    })
    
    output$map_plot <- renderPlotly({
        nurses_map_data %>%
            filter(year == input$year) %>%
            ggplot(aes(long,
                       lat,
                       group = group,
                       fill = !!sym(input$metric))) +
            geom_polygon() +
            coord_map() +
            scale_fill_viridis_c(labels = label_format()) +
            ggthemes::theme_map() +
            labs(fill = "")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

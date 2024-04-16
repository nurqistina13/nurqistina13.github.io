# Load required libraries
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)

# Read the dataset
combined_data <- read.csv("combined_employment_gross_monthly_mean_by_category.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Overall Employment Rate vs Gross Monthly Mean Income"),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "selected_degree",
        label = "Select Degree:",
        choices = sort(unique(combined_data$degree)),
        options = list(`actions-box` = TRUE),
        multiple = TRUE
      ),
      actionButton("select_all", "Select All", icon("check")),
      actionButton("deselect_all", "Deselect All", icon("times"))
    ),
    
    mainPanel(
      plotOutput("scatter_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observe({
    if(input$select_all > 0) {
      updatePickerInput(session, "selected_degree", selected = sort(unique(combined_data$degree)))
    }
  })
  
  observe({
    if(input$deselect_all > 0) {
      updatePickerInput(session, "selected_degree", selected = character(0))
    }
  })
  
  filtered_data <- reactive({
    req(combined_data)  # Require that combined_data is available
    
    if(is.null(input$selected_degree) || "All" %in% input$selected_degree) {
      combined_data %>% 
        filter(!is.na(employment_rate_overall) & !is.na(gross_monthly_mean))
    } else {
      combined_data %>% 
        filter(degree %in% input$selected_degree) %>% 
        filter(!is.na(employment_rate_overall) & !is.na(gross_monthly_mean))
    }
  })
  
  output$scatter_plot <- renderPlot({
    
    req(filtered_data())  # Require that filtered_data is available
    
    # Create scatter plot
    ggplot(filtered_data(), aes(x = employment_rate_overall, y = gross_monthly_mean)) +
      geom_point() +
      facet_wrap(~ degree, scales = "free", ncol = 3) +
      labs(
        title = "Overall Employment Rate vs Gross Monthly Mean Income",
        x = "Overall Employment Rate",
        y = "Gross Monthly Mean Income"
      ) +
      theme_minimal() +
      xlim(60, 100) +
      ylim(2000, 7000)
  })
}

# Run the application
shinyApp(ui = ui, server = server)


# Load required libraries
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(dplyr)

# Define combined_data by reading the CSV file
combined_data <- read.csv("employment_rate_vs_gross_monthly_mean_income_by_degree.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Employment Rate vs Gross Monthly Mean Income by Degree"),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "selected_degree",
        label = "Select Degree:",
        choices = sort(unique(combined_data$degree)),
        selected = "Arts",
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      )
    ),
    
    mainPanel(
      plotOutput("scatter_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$scatter_plot <- renderPlot({
    
    # Filter data based on selected degrees
    filtered_data <- combined_data %>% 
      filter(degree %in% input$selected_degree)
    
    # Create scatter plot
    ggplot(filtered_data, aes(x = gross_monthly_mean_income, y = employment_rate_overall)) +
      geom_point() +
      scale_x_continuous(limits = c(2000, 7500)) +
      scale_y_continuous(limits = c(60, 100)) +
      facet_wrap(~ degree, scales = "free") +
      labs(
        title = "Employment Rate vs Gross Monthly Mean Income by Degree",
        x = "Gross Monthly Mean Income",
        y = "Employment Rate Overall"
      ) +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# Load the required libraries
library(shiny)
library(plotly)

# Define UI
ui <- fluidPage(
  titlePanel("Average Gross Monthly Income by Year"),
  plotlyOutput("line_plot")
)

# Define server logic
server <- function(input, output) {
  # Read the saved CSV file
  average_of_gross_monthly_income_by_year <- read.csv("average_of_gross_monthly_income_by_year.csv")
  
  # Create the line plot
  output$line_plot <- renderPlotly({
    plot_ly(data = average_of_gross_monthly_income_by_year, x = ~year, y = ~average_of_gross_monthly_income, type = "scatter", mode = "lines+markers") %>%
      layout(title = "Average Gross Monthly Income by Year",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Average Gross Monthly Income ($)"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)


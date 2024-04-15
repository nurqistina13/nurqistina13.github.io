# Load the required libraries
library(shiny)
library(plotly)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Average Gross Monthly Income by Year"),
  plotlyOutput("line_plot")
)

# Define server logic
server <- function(input, output) {
  # Read the CSV file
  average_of_gross_monthly_income_by_year <- read.csv("average_of_gross_monthly_income_by_year.csv")
  
  # Standardize the year variable
  average_of_gross_monthly_income_by_year$standardized_year <- scale(average_of_gross_monthly_income_by_year$year)
  
  # Fit linear model using standardized year and an intercept-only term
  lm_fit <- lm(average_of_gross_monthly_income ~ 1 + standardized_year, data = average_of_gross_monthly_income_by_year)
  
  # Create the line plot
  output$line_plot <- renderPlotly({
    plot_ly(data = average_of_gross_monthly_income_by_year, x = ~year, y = ~average_of_gross_monthly_income, type = "scatter", mode = "lines+markers", name = "",
            line = list(color = "steelblue")) %>%
      layout(title = "Average Gross Monthly Income by Year",
             xaxis = list(title = "Year", tickvals = unique(average_of_gross_monthly_income_by_year$year), ticktext = unique(average_of_gross_monthly_income_by_year$year)),
             yaxis = list(title = "Average Gross Monthly Income ($)"),
             showlegend = FALSE,
             width = 800)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

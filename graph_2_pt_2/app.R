# Load required libraries
library(plotly)
library(shiny)
library(dplyr)

# Read the new CSV file
selected_data <- read.csv("gross_monthly_mean_income_2021_by_university.csv")

# Remove NA values
selected_data <- na.omit(selected_data)

# Recode university names to shortcuts
selected_data$university <- recode(selected_data$university,
                                   "National University of Singapore" = "NUS",
                                   "Nanyang Technological University" = "NTU",
                                   "Singapore University of Technology and Design" = "SUTD",
                                   "Singapore University of Social Sciences" = "SUSS",
                                   "Singapore Institute of Technology" = "SIT",
                                   "Singapore Management University" = "SMU")

# Calculate average gross_monthly_mean income for each university
average_data <- selected_data %>%
  group_by(university) %>%
  summarise(avg_gross_monthly_mean = mean(gross_monthly_mean, na.rm = TRUE))

# Define UI
ui <- fluidPage(
  titlePanel("Average Monthly Mean Income by University"),
  
  mainPanel(
    plotlyOutput("bar_plot")
  )
)

# Define server logic
server <- function(input, output) {
  
  output$bar_plot <- renderPlotly({
    
    # Create interactive bar plot
    p <- plot_ly(average_data, x = ~university, y = ~avg_gross_monthly_mean, type = 'bar') %>%
      layout(title = "Average Monthly Mean Income by University",
             xaxis = list(title = "University"),
             yaxis = list(title = "Average Monthly Mean Income",
                          range = c(3000, 5000),
                          tickvals = seq(3000, 5000, 500)),
             margin = list(b = 150)) # Adjust bottom margin to accommodate x-axis labels
    
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)

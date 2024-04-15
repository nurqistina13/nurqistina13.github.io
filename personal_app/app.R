library(shiny)
library(plotly)
library(dplyr)

mean_employment_rate_by_year <- read.csv("df_1.csv")

# Calculate average employment rate by year
avg_employment_by_year <- mean_employment_rate_by_year %>%
  group_by(year) %>%
  summarise(mean_employment_rate = mean(mean_employment_rate, na.rm = TRUE))

# Define UI
ui <- fluidPage(
  titlePanel("Average Employment Rate By Year"),
  mainPanel(
    plotlyOutput("line_plot")
  )
)

# Define server logic
server <- function(input, output) {
  
  output$line_plot <- renderPlotly({
    
    # Create a line plot with plotly
    p <- plot_ly(data = mean_employment_rate_by_year, x = ~year, y = ~mean_employment_rate, type = 'scatter', mode = 'lines+markers', color = I("steelblue"), name = "") %>%
      layout(title = "Average Employment Rate By Year",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Average Employment Rate"),
             showlegend = FALSE,
             width = 800)  # Set the width to 800 pixels
    
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)


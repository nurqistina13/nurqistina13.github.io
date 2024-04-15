library(shiny)
library(plotly)

# Define UI
ui <- fluidPage(
  titlePanel("Average Employment Rate in 2021"),
  mainPanel(
    plotlyOutput("line_plot")
  )
)

# Define server logic
server <- function(input, output) {
  # Load the dataset
  mean_employment_2021 <- read.csv("average_employment_rate_by_uni_in_2021.csv")  # Assuming the file is in the project directory
  
  output$line_plot <- renderPlotly({
    # Create a bar chart with plotly
    p <- plot_ly(data = mean_employment_2021, x = ~university, y = ~average_employment_rate_in_2021, type = 'bar') %>%
      layout(title = "Average Employment Rate in 2021",
             xaxis = list(title = "University"),
             yaxis = list(title = "Average Employment Rate", range = c(93, 97), tickvals = seq(93, 97, 0.5)))
    
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)



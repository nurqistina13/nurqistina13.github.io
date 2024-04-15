library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
library(stats)

# Read the dataset
data <- read.csv("/Users/student/Desktop/y3s2/nm2207/github/nurqistina13.github.io/GraduateEmploymentSurveyNTUNUSSITSMUSUSSSUTD.csv")

# Convert the 'year' column to Date format
data$year <- as.Date(paste0(data$year, "-01-01"))

# Define UI
ui <- fluidPage(
  titlePanel("Employment and Income Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("university", "Select University:", choices = unique(data$university)),
      selectInput("degree", "Select Degree:", choices = unique(data$degree)),
      textInput("predict_year", "Year to Predict (yyyy):", value = ""),
      actionButton("predict_button", "Predict")
    ),
    
    mainPanel(
      plotlyOutput("prediction_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  predictions <- reactiveVal(NULL)
  
  observeEvent(input$predict_button, {
    
    # Filter data based on user input
    filtered_data <- data %>%
      filter(university == input$university, degree == input$degree)
    
    # Check if there is enough data for modeling
    if (nrow(filtered_data) < 2) {
      predictions(NULL)
      return()
    }
    
    # Predict employment rate
    employment_model <- lm(employment_rate_overall ~ year, data = filtered_data)
    predicted_employment <- predict(employment_model, newdata = data.frame(year = as.Date(paste0(input$predict_year, "-01-01"))))
    
    # Predict basic monthly mean income
    income_model <- lm(basic_monthly_mean ~ year, data = filtered_data)
    predicted_income <- predict(income_model, newdata = data.frame(year = as.Date(paste0(input$predict_year, "-01-01"))))
    
    predictions(list(predicted_employment = predicted_employment, predicted_income = predicted_income))
  })
  
  output$prediction_plot <- renderPlotly({
    
    req(predictions())
    
    # Create plot
    p <- plot_ly() %>%
      add_trace(x = c(as.Date(input$predict_year, "-01-01")), y = predictions()$predicted_employment, type = 'scatter', mode = 'markers', name = 'Predicted Employment Rate') %>%
      add_trace(x = c(as.Date(input$predict_year, "-01-01")), y = predictions()$predicted_income, type = 'scatter', mode = 'markers', name = 'Predicted Basic Monthly Mean Income') %>%
      layout(title = paste("Predictions for", input$university, "-", input$degree, "in", input$predict_year),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Value"),
             showlegend = TRUE)
    
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)


# Define updated pastel colors
pastel_colors <- c("#FF3333", "#FF9966", "#FFC000", "#66CC66", "#3366FF", "#CC66CC")

# Load required libraries
library(shiny)
library(ggplot2)
library(data.table)

# Define UI
ui <- fluidPage(
  
  titlePanel("Scatter Plot of Employment Rate vs Gross Monthly Mean Income by University"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("university", "Select University:", choices = c("All", unique(data$university)), selected = "All"),
      checkboxInput("show_all", "Show All Universities", value = FALSE)
    ),
    
    mainPanel(
      plotOutput("scatterPlot", height = "650px", width = "750px")  # Set height and width
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Read the new dataset into 'data' using relative path
  new_csv_path <- "employment_rate_vs_gross_monthly_mean_income_by_university.csv"
  data <- fread(new_csv_path)
  
  # Create filtered data
  filtered_data <- reactive({
    if (input$show_all) {
      data
    } else {
      if (input$university == "All") {
        data
      } else {
        data[data$university == input$university, ]
      }
    }
  })
  
  # Create scatter plot
  output$scatterPlot <- renderPlot({
    
    ggplot(filtered_data(), aes(x = employment_rate_overall, y = gross_monthly_mean, color = university)) +
      geom_point() +
      labs(
        x = "Overall Employment Rate",
        y = "Gross Monthly Mean Income",
        title = "Scatter Plot of Employment Rate vs Gross Monthly Mean Income",
        subtitle = if (input$university == "All") "All Universities" else input$university,
        color = "University"
      ) +
      scale_color_manual(values = setNames(pastel_colors, unique(data$university))) +
      theme_minimal() +
      theme(
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),  # Legend title size
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 20, hjust = 0.5),  # Title size and center alignment
        strip.text = element_text(size = 18)  # Facet title size
      ) +
      facet_wrap(~ university, ncol = 3)  # Display all facets in one row with 3 columns
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

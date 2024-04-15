
# Define server logic
server <- function(input, output) {
  # Create the interactive scatter plot with customized facet titles
  output$scatter_plot <- renderPlotly({
    ggplotly(
      ggplot(combined_dataset, aes(x = employment_rate_overall, y = gross_monthly_mean)) +
        geom_point() +
        facet_wrap(~ university, labeller = labeller(university = c("NTU" = "Nanyang Technological University", 
                                                                    "NUS" = "National University of Singapore",
                                                                    "SIT" = "Singapore Institute of Technology",
                                                                    "SMU" = "Singapore Management University",
                                                                    "SUSS" = "Singapore University of Social Sciences",
                                                                    "SUTD"="Singapore University of Technology and Design"))) +
        labs(x = "Employment Rate Overall", y = "Gross Monthly Mean") +
        theme_minimal() +
        theme(strip.text = element_text(margin = margin(20, 0, 20, 0))) # Adjust the margin as needed
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

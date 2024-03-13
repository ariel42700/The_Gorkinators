## Shiny App Visualization

library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Digital Payments vs. Account at Financial Institution"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("year_buttons", "Select Year", choices = c(2014, 2017, 2021), selected = 2014)
    ),
    mainPanel(
      plotlyOutput("scatter_plot")
    )
  )
)

# Define the server
server <- function(input, output) {
  filtered_data <- reactive({
    merged_data[merged_data$year == input$year_buttons, ]
  })
  
  output$scatter_plot <- renderPlotly({
    plot_ly(data = filtered_data(),
            x = ~account_fin, y = ~payment_digital,
            color = ~group_income, text = ~paste(country, "<br>", year),
            size = ~gdpPercap, sizes = c(5, 20),
            type = "scatter",
            mode = "markers", marker = list(sizemode = "diameter", opacity = 0.6)) %>%
      layout(
        title = paste("Digital Payments vs. Account at Financial Institution -", input$year_buttons),
        xaxis = list(title = "Percentage with an account at a Financial Institution"),
        yaxis = list(title = "Percentage made or received a digital payment"),
        showlegend = TRUE
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)

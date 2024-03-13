# Visualization 3 - Average Percentage of Digital Payments Across Regions and Income Groups

# Add a new column with corresponding regions
library(countrycode)
merged_data$better_region <- countrycode(merged_data$country, origin = "country.name", destination = "region")

# Calculate the mean digital payment for each combination of region and income group
heatmap_data <- merged_data %>%
  group_by(better_region, group_income) %>%
  summarize(mean_payment_digital = mean(payment_digital, na.rm = TRUE), .groups = "drop")

# Define UI
ui <- fluidPage(
  titlePanel("Mean Digital Payments by Region and Income Group"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year", choices = c(2014, 2017, 2021), selected = 2014)
    ),
    mainPanel(
      plotlyOutput("heatmap")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive expression to filter data based on selected year
  filtered_data <- reactive({
    merged_data_year <- merged_data[merged_data$year == input$year, ]
    heatmap_data <- merged_data_year %>%
      group_by(better_region, group_income) %>%
      summarize(mean_payment_digital = mean(payment_digital, na.rm = TRUE), .groups = "drop")
    return(heatmap_data)
  })
  
  # Render the heatmap
  output$heatmap <- renderPlotly({
    plot_ly(filtered_data(), x =~group_income, y = ~better_region, z = ~mean_payment_digital, 
            type = "heatmap", colors = "Blues",
            hoverinfo = "x+y+z") %>%
      layout(title = paste("Mean Digital Payments by Region and Income Group -", input$year),
             xaxis = list(title = "Region"),
             yaxis = list(title = "Income Group"),
             plot_bgcolor = "darkgrey")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

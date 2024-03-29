## Shiny App Visualization
library(shiny)

# Import the two datasets: DatabankWide and df_gdpPC
df_findex <- read_excel("DatabankWide.xlsx")
df_gdp <- read.csv("df_gdpPC.csv")

# Select the variables of interest
df_findex_var <- df_findex[c("Country name", "Year", "Region", "Adult populaiton", "Income group", "Made or received a digital payment (% age 15+)", "Financial institution account (% age 15+)")]
df_gdp_var <- df_gdp[c("country", "year", "gdpPercap", "lifeExp")]

# Renmae variables
# Create a new dataframe df_rename
df_findex_new <- df_findex_var

# Rename "Made or received a digital payment (% age 15+)" to "payment_digital"
colnames(df_findex_new)[colnames(df_findex_new) == "Made or received a digital payment (% age 15+)"] <- "payment_digital"

# Rename "Financial institution account (% age 15+)" to "account_fin"
colnames(df_findex_new)[colnames(df_findex_new) == "Financial institution account (% age 15+)"] <- "account_fin"

# Rename "Country name" to "country"
colnames(df_findex_new)[colnames(df_findex_new) == "Country name"] <- "country"

# Rename "Adult populaiton" to "population"
colnames(df_findex_new)[colnames(df_findex_new) == "Adult populaiton"] <- "population"

# Rename "Income group" to "group_income"
colnames(df_findex_new)[colnames(df_findex_new) == "Income group"] <- "group_income"

# Rename "Year" to "year"
colnames(df_findex_new)[colnames(df_findex_new) == "Year"] <- "year"

# Rename "Region" to "region"
colnames(df_findex_new)[colnames(df_findex_new) == "Region"] <- "region"

# Remove N/A values
df_findex_new <- na.omit(df_findex_new)

# Keep observations from years 2014, 2017, and 2021
df_gdp_var_limit <- subset(df_gdp_var, year %in% c(2014, 2017, 2021))
df_findex_limit <- subset(df_findex_new, year %in% c(2014, 2017, 2021))

# Merge the two datasets by 'country' and 'year'
merged_data <- merge(df_findex_limit, df_gdp_var_limit, by = c("year", "country"))


# Define the UI
ui <- fluidPage(
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

# Visualization 3 - Average Percentage of Digital Payments Across Regions and Income Groups

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



# Add a new column with corresponding regions
library(countrycode)
merged_data$better_region <- countrycode(merged_data$country, origin = "country.name", destination = "region")

# Calculate the mean digital payment for each combination of region and income group
heatmap_data <- merged_data %>%
  group_by(better_region, group_income) %>%
  summarize(mean_payment_digital = mean(payment_digital, na.rm = TRUE), .groups = "drop")



# Define UI
ui <- fluidPage(
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

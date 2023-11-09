# Structure
library(flexdashboard)
library(shiny)
library(shinyWidgets)
library(shinydashboard)


# Core
library(tidyverse)
library(lubridate)
library(readxl)
library(dplyr)
# Data Visualization
library(plotly)

# Read in data
stocks_tbl <- read_excel(path = "stocks_monthly.xlsx")


# Create a Shiny app
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    # Create data picker
    dateRangeInput(
      inputId = "date_range", 
      label   = h4("Date Range"),
      start   = min(stocks_tbl$date), # start value come from data 
      end     = max(stocks_tbl$date), 
      min     = min(stocks_tbl$date), # min makes sure user can't select anything before the beginning date
      max     = max(stocks_tbl$date), 
      startview = "month"), # This is were default will start and can be changed
    
    box(
      width = 15,
      title = "Symbol",  # New title for Graph B options
      checkboxGroupButtons(
        inputId = "selected_symbol",
        label = "Choices", 
        choices = unique(stocks_tbl$symbol)
      )
    )
  ),
  
  dashboardBody(
    fluidRow(
      box(
        title = "Graph A",
        plotlyOutput("stock_bar_chart"),
        width = 12
        
      ),
      box(
        title = "Graph B",
        plotlyOutput("stock_scatter_chart"),
        width = 12
      )
    )
  )
)


### Graph A

server <- function(input, output) {
  # Define colors for different symbols
  colors <- c(AAPL = "blue", GOOG = "orange", NFLX = "red")
  
  filtered_data <- reactive({
    req(input$selected_symbol)
    selected_symbols <- input$selected_symbol
    
    # Filter data based on selected symbols
    data_filtered_by_symbols <- stocks_tbl %>%
      filter(symbol %in% selected_symbols)
    
    # Filter data based on the selected date range
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    data_filtered_by_symbols %>%
      filter(date >= start_date, date <= end_date) %>%
      mutate(symbol_color = ifelse(symbol %in% names(colors), colors[symbol], "gray"))
  })
  
  output$stock_bar_chart <- renderPlotly({
    filtered_data <- filtered_data()
    
    plot_ly(filtered_data, x = ~date, y = ~monthly.returns, type = 'bar', 
            marker = list(color = ~symbol_color)) %>%
      layout(barmode = 'group')
  })
  
  output$stock_scatter_chart <- renderPlotly({
    filtered_data <- filtered_data()
    
    plot_ly(filtered_data, x = ~date, y = ~monthly.returns, type = 'scatter', 
            marker = list(color = ~symbol_color)) %>%
      layout(barmode = 'group')
  })
}

shinyApp(ui, server)
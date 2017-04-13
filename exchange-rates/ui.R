library(shiny)

# Setup
source("helper.R")
currency_list <- unique(loadExchangeRates()$currency)
is_priority <- currency_list %in% c("USD","EUR","GBP")
currency_list <- c(currency_list[is_priority], sort(currency_list[!is_priority]))

# Main server function
shinyUI(
  fluidPage(
    
    fluidRow(
      column(12, h1("Currency Exchange Rates"))
    ),
      
    fluidRow(
      column(4, selectInput("currency_from", label="From: ", 
                            choices=currency_list, selected = 'USD')),
      column(4, dateInput("date", label = "Date:",
                          value = Sys.Date()-1)),
      column(3, verbatimTextOutput("conversion"))
    ),
      
    fluidRow(
      column(4, selectInput("currency_to", label="To: ", 
                            choices=currency_list, selected = 'GBP')),
      column(4, actionButton("reset", "Current")),
      column(3, "Placeholder")
    ),
    
    fluidRow(
        column(9, plotlyOutput("compare_plot")),
        column(3, dataTableOutput("table"))
    )
  )
)


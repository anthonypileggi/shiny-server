library(shiny)

# load functions
source("helper.R")

# load data
x <- loadExchangeRates()

shinyServer(function(input, output, session) {

  values <- reactiveValues()
  
  # Update comparison timeseries
  observe ({
    values$compare <- compareCurrency(x, 
                                      currency_to = input$currency_to, 
                                      currency_from = input$currency_from)
    
    
      
    values$compare_plot <- values$compare$plot +
                              geom_vline(xintercept=as.numeric(input$date),
                                         linetype="dashed", 
                                         color='blue')

    values$click <- event_data("plotly_click")
    
  })

  observeEvent(values$click, {
    updateDateInput(session, "date", value=values$compare$data$date[values$click$pointNumber])
  })
  
  # Plot comparison timeseries
  output$compare_plot <- renderPlotly({
    ggplotly(req(values$compare_plot), tooltip="label") %>% 
          layout(dragmode = "select")
  })
  
  # Table of conversion rates (for selected date)
  output$table <- renderDataTable(({
    tx <- subset(x, date==input$date)
    base_rate <- tx$rate[tx$currency==input$currency_from]
    tx$rate <- tx$rate / base_rate
    tx <- tx[tx$currency!=input$currency_from, ]
    tx[, c('currency','rate')] %>%
      DT::datatable(rownames=FALSE, 
                    colnames=c("Currency", paste(1,input$currency_from,"=")),
                    options=list(pageLength=nrow(tx), scrollY = "300px")) %>%
        DT::formatRound("rate", digits=4)
  }))
  
  # Action -- Reset to current date
  observeEvent(input$reset, {
    updateDateInput(session, "date", value=max(x$date))
  })
  
  # Click event
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click to clear)" else d
  })

  # Conversion results
  output$conversion <- renderPrint({
    tmp <- values$compare$data[values$compare$data$date==input$date, ]
    paste("1", input$currency_from, "=", round(tmp$rate,2), input$currency_to)
  })
})
#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

source('helper.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(formData())
  })
  
  # Show the current inventory (w/ past week)
  output$inventory_plot <- renderPlot({
    input$submit
    x <- req(loadData())
    x %>% dplyr::mutate(date = as.Date(format(as.POSIXct(date, origin='1970-01-01'), '%Y-%m-%d')),
                        quantity = as.numeric(quantity)) %>%
          dplyr::group_by(date, type) %>%
          dplyr::summarize(quantity = sum(quantity)) %>%
          ggplot(aes(x=date, y=quantity, group=type)) + 
            geom_bar(aes(fill=type), stat="identity") +
            labs(x=NULL, y="Quantity (grams)", fill=NULL) +
            theme_bw()
        
  })
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$inventory <- DT::renderDataTable({
    input$submit
    req(loadData()) %>%
      dplyr::mutate(date = as.POSIXct(date, origin='1970-01-01')) %>%
      DT::datatable(rownames=FALSE) %>%
        DT::formatCurrency("cost", digits=0) %>%
        DT::formatDate("date")
  })     
  
  # Update login credentials using URL parameters
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['username']])) {
      updateTextInput(session, "username", value = query[['username']])
    }
    if (!is.null(query[['password']])) {
      updateTextInput(session, "password", value = query[['password']])
    }
    if (!is.null(query[['host']])) {
      updateTextInput(session, "host", value = query[['host']])
    }
  })
  
})

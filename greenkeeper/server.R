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
  
  values <- reactiveValues()
  
  # Whenever a field is filled, aggregate all form data
  inventoryData <- reactive({
    data <- sapply(fields$inventory, function(x) input[[x]])
    data
  })
  consumptionData <- reactive({
    data <- sapply(fields$consumption, function(x) input[[x]])
    data
  })
  
  # When the Connect button is clicked
  # - save the credentials
  # - check if the connection worked
  observeEvent(input$connect, {
    options(mongodb = list(
      "host" = input$host,
      "username" = input$username,
      "password" = input$password
    ))
    values$mongodb$inventory <- connectMongoDB("inventory")
    values$mongodb$consumption <- connectMongoDB("consumption")
  })
  
  # When a new database connection is made, grab the data:
  #   - inventory
  #   - consumption
  observeEvent(values$mongodb$inventory, {
    values$inventory <- loadInventoryData(values$mongodb$inventory)
  })
  observeEvent(values$mongodb$consumption, {
    values$consumption <- loadConsumptionData(values$mongodb$consumption)
  })
  
  # Whenever the inventory or consumption tables change, update the avail
  updateCurrentInventory <- reactive({
    list(values$inventory, values$consumption)
  })
  observeEvent(updateCurrentInventory(), {
    values$avail <- getCurrentInventory(req(values$inventory), 
                                        values$consumption)
  })
  
  # Check mongoDB connection status
  #   - inventory db
  #   - consumption db
  output$status_inventory <- renderText({
    if (!is.null(values$mongodb$inventory)) {
      paste("Inventory: Connection successful!  Identified", 
            values$mongodb$inventory$count(), "Records")
    } else {
      paste("Inventory: No connection yet...")
    }
  })
  output$status_consumption <- renderText({
    if (!is.null(values$mongodb$consumption)) {
      paste("Consumption: Connection successful!  Identified", 
            values$mongodb$consumption$count(), "Records")
    } else {
      paste("Consumption: No connection yet...")
    }
  })
  
  # When the Submit button is clicked:
  #   - save the form data
  #   - update inventory data
  observeEvent(input$submit, {
    saveData(inventoryData(), values$mongodb$inventory)
    values$inventory <- loadInventoryData(values$mongodb$inventory)
  })
  
  # When the Consume button is clicked:
  #   - save the data to mongoDB
  #   - update consumption data
  #   - update inventory data
  observeEvent(input$consume, {
    saveData(consumptionData(), values$mongodb$consumption, id=input$item)
    values$consumption <- loadConsumptionData(values$mongodb$consumption)
  })
  
  # Plot the current inventory (w/ past week)
  output$inventory_plot <- renderPlot({
    req(values$inventory) %>%
        dplyr::mutate(date = as.Date(format(as.POSIXct(date, origin='1970-01-01'), '%Y-%m-%d'))) %>%
        dplyr::group_by(date, type) %>%
        dplyr::summarize(quantity = sum(quantity)) %>%
        ggplot(aes(x=date, y=quantity, group=type)) + 
          geom_bar(aes(fill=type), stat="identity") +
          labs(x=NULL, y="Quantity (grams)", fill=NULL) +
          theme_bw()
  })
  
  # Show the current data
  #   - inventory
  #   - consumption
  #   - avail
  output$inventory <- DT::renderDataTable({
    req(values$inventory) %>%
      dplyr::mutate(date = as.POSIXct(date, origin='1970-01-01')) %>%
      DT::datatable(rownames=FALSE) %>%
        DT::formatCurrency("cost", digits=0) %>%
        DT::formatDate("date")
  })     
  output$consumption <- DT::renderDataTable({
    req(values$consumption) %>%
      dplyr::mutate(date = as.POSIXct(date, origin='1970-01-01')) %>%
      DT::datatable(rownames=FALSE) %>%
      DT::formatDate("date")
  })  
  output$avail <- DT::renderDataTable({
    req(values$avail) %>%
      DT::datatable(rownames=FALSE)
  })  
  
  # Show some statistics:
  #   - total stash value
  output$stash_value <- renderText({
    if (is.null(values$avail)) {
      paste0("Total Stash Value:  $0")
    } else {
      paste0("Total Stash Value:  ", scales::dollar(sum(values$avail$stash_value)))
    }
  })
  
  # -- update selectInput
  observeEvent(values$avail, {
    ind <- values$avail$stash>0
    choice_list <- 
      setNames(values$avail[ind, 'id'],
               paste0(values$avail[ind, 'name'], " (#", 
                      values$avail[ind, 'id'], ") -- ",
                      values$avail[ind, 'stash'], " grams"))
    updateSelectizeInput(session, 'item', choices = choice_list)
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

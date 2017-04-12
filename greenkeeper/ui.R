#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

source('helper.R')

# Define UI for application that draws a histogram
shinyUI(
  navbarPage("Greenkeeper",
  
  tabPanel("Login Credentials",
    fluidPage(
        textInput("username", "Username:", ""),
        textInput("password", "Password:", ""),
        textInput("host", "Host:", ""),
        actionButton("connect", "Connect"),
        textOutput("status_inventory"),
        textOutput("status_consumption")
    )
  ),
  
  tabPanel("Consume",
     sidebarLayout(
       
       sidebarPanel(
         selectInput("item", "Item:", list()),
         numericInput("amount", "Amount", ""),
         actionButton("consume", "Consume")
       ),
       
       mainPanel(
         DT::dataTableOutput("consumption")
       )
     )
   ),

  tabPanel("Inventory Manager",
    sidebarLayout(
    
      # Sidebar with a slider input
      sidebarPanel(
        textInput("name", "Name", ""),
        textInput("brand", "Brand", ""),
        selectInput("type", "Type:",
                    c("Flower" = "flower",
                      "Edible" = "edible",
                      "Wax" = "wax")),
        numericInput("quantity", "Quantity", ""),
        numericInput("cost", "Cost", ""),
        div(
          actionButton("submit", "Submit"),
          span(textOutput("stash_value"), style = "color:blue")
        )
      ),
    
      # Show a plot of the generated distribution
      mainPanel(
        DT::dataTableOutput("inventory")
      )
    )
  ),
  
  tabPanel("Inventory History",
    fluidPage(
      plotOutput("inventory_plot")
    ))
))

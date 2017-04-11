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
    sidebarLayout(
      sidebarPanel(
        textInput("username", "Username:", ""),
        textInput("password", "Password:", ""),
        textInput("host", "Host:", "")
      ),
      mainPanel()
    )
  ),
  
  tabPanel("Consumption"),

  tabPanel("Inventory",
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
        actionButton("submit", "Submit")
      ),
    
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("inventory_plot"),
        DT::dataTableOutput("inventory")
      )
    )
  )
))

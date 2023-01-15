#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
  titlePanel("MPG prediction with the Linear Regression Model"),
  h4("Miles Per Gallon (MPG) prediction per chosen variable"),
  sidebarLayout(
    # Compose the sidebar layout
    sidebarPanel(
      h3("Variables"),
      selectInput("select", "Choose a variable", c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
                                                   "gear", "carb")),
      submitButton("Submit")
    ),
    # Compose the main panel layout. Two tabs will be created; the first tab is with 
    # the linear regression model with a prediction line, 
    # the second tab is with the linear regression model with a geom_smooth.
    
    mainPanel(
      h3("Linear Regression Model"),
      tabsetPanel(type = "tabs",
                  tabPanel("Simple Linear Regression", br(), plotOutput("plot1"),
                           h3("Summary of Linear Model:"),
                           verbatimTextOutput("Regression_Model")),
                  tabPanel("Linear Regression with Geom Smooth", br(), plotOutput("plot2")),
      )
    )
  )
))

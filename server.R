#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

shinyServer(function(input, output) {

# Make a reactive expression for linear model to predict MPG vs. other variables.  
formula <-reactive({paste("mpg ~", input$select)})
lm_model <- reactive({summary(lm(formula(), data = mtcars))})

# This will plot a prediction line with the linear regression model. 
# The red dots are the collected data and the blue line is the predicted line by calculating 
# an intercept and a slope per each variable.
  output$plot1 <- renderPlot({
   if(input$select == "cyl") {
     ggplot(mtcars, aes(x = cyl, y = mpg)) + geom_point(size = 5, col = "red", 
      alpha = 0.3) + geom_abline(intercept = lm_model()$coeff[1], 
                                 slope = lm_model()$coeff[2], col = "blue", lwd = 1)
    } else if(input$select == "disp") {
      ggplot(mtcars, aes(x = disp, y = mpg)) + geom_point(size = 5, col = "red", 
      alpha = 0.3) + geom_abline(intercept = lm_model()$coeff[1], 
                                 slope = lm_model()$coeff[2], col = "blue", lwd = 1)
    } else if(input$select == "hp") {
      ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point(size = 5, col = "red", 
      alpha = 0.3) + geom_abline(intercept = lm_model()$coeff[1], 
                                 slope = lm_model()$coeff[2], col = "blue", lwd = 1)
    } else if(input$select == "drat") {
      ggplot(mtcars, aes(x = drat, y = mpg)) + geom_point(size = 5, col = "red", 
      alpha = 0.3) + geom_abline(intercept = lm_model()$coeff[1], 
                                 slope = lm_model()$coeff[2], col = "blue", lwd = 1)
    } else if(input$select == "wt") {
      ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point(size = 5, col = "red", 
      alpha = 0.3) + geom_abline(intercept = lm_model()$coeff[1], 
                                 slope = lm_model()$coeff[2], col = "blue", lwd = 1)
    } else if(input$select == "qsec") {
      ggplot(mtcars, aes(x = qsec, y = mpg)) + geom_point(size = 5, col = "red", 
      alpha = 0.3) + geom_abline(intercept = lm_model()$coeff[1], 
                                slope = lm_model()$coeff[2], col = "blue", lwd = 1)
    } else if(input$select == "vs") {
      ggplot(mtcars, aes(x = vs, y = mpg)) + geom_point(size = 5, col = "red", 
      alpha = 0.3) + geom_abline(intercept = lm_model()$coeff[1], 
                               slope = lm_model()$coeff[2], col = "blue", lwd = 1)
    } else if(input$select == "am") {
      ggplot(mtcars, aes(x = am, y = mpg)) + geom_point(size = 5, col = "red", 
      alpha = 0.3) + geom_abline(intercept = lm_model()$coeff[1], 
                              slope = lm_model()$coeff[2], col = "blue", lwd = 1)
    } else if(input$select == "gear") {
      ggplot(mtcars, aes(x = gear, y = mpg)) + geom_point(size = 5, col = "red", 
      alpha = 0.3) + geom_abline(intercept = lm_model()$coeff[1], 
                              slope = lm_model()$coeff[2], col = "blue", lwd = 1)
    } else if(input$select == "carb") {
      ggplot(mtcars, aes(x = carb, y = mpg)) + geom_point(size = 5, col = "red", 
      alpha = 0.3) + geom_abline(intercept = lm_model()$coeff[1], 
                              slope = lm_model()$coeff[2], col = "blue", lwd = 1)
    }
    
  })

  # This is a lot with Geom_Smooth  
  output$plot2 <- renderPlot({
    # check for the input variable
    if (input$select == "cyl") {
      ggplot(mtcars, aes(x = cyl, y = mpg)) + geom_smooth() + geom_point(
        size = 5, col = "red", alpha = 0.3)
      } else if(input$select == "disp") {
      ggplot(mtcars, aes(x = disp, y = mpg)) + geom_smooth() + geom_point(
        size = 5, col = "red", alpha = 0.3)
    } else if(input$select == "hp") {
      ggplot(mtcars, aes(x = hp, y = mpg)) + geom_smooth() + geom_point(
        size = 5, col = "red", alpha = 0.3)
    } else if(input$select == "drat") {
      ggplot(mtcars, aes(x = drat, y = mpg)) + geom_smooth() + geom_point(
        size = 5, col = "red", alpha = 0.3)
    } else if(input$select == "wt") {
      ggplot(mtcars, aes(x = wt, y = mpg)) + geom_smooth() + geom_point(
        size = 5, col = "red", alpha = 0.3)
    } else if(input$select == "qsec") {
      ggplot(mtcars, aes(x = qsec, y = mpg)) + geom_smooth() + geom_point(
        size = 5, col = "red", alpha = 0.3)
    } else if(input$select == "vs") {
      ggplot(mtcars, aes(x = vs, y = mpg)) + geom_smooth() + geom_point(
        size = 5, col = "red", alpha = 0.3)
    } else if(input$select == "am") {
      ggplot(mtcars, aes(x = am, y = mpg)) + geom_smooth() + geom_point(
        size = 5, col = "red", alpha = 0.3)
    } else if(input$select == "gear") {
      ggplot(mtcars, aes(x = gear, y = mpg)) + geom_smooth() + geom_point(
        size = 5, col = "red", alpha = 0.3)
    } else if(input$select == "carb") {
      ggplot(mtcars, aes(x = carb, y = mpg)) + geom_smooth() + geom_point(
        size = 5, col = "red", alpha = 0.3)
    }
  })
  
  # This shows the summary of the linear regression model analysis. 
  # The summary is shown up on the "Simple Linear Regression" tab.
  output$Regression_Model <- renderPrint(lm_model())
})


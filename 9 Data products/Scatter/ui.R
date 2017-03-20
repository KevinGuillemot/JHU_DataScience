#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Plot random numbers"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("numeric","How many random numbers should be ploted?",value=1000,min=1, max=1000, step=1),
      sliderInput("sliderX","Minimum and Maximum X values:", min = -100, max = 100, value = c(-50,50)),
      sliderInput("sliderY","Minimum and Maximum Y values:", min = -100, max = 100, value = c(-50,50)),
      checkboxInput("show_xlab","Show/Hide X Axis",value=TRUE),
      checkboxInput("show_ylab","Show/Hide Y Axis",value=TRUE),
      checkboxInput("show_title","Show/Hide Title",value=TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("plot1")
    )
  )
))

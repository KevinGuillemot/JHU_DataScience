# ########################################################################################
# JHU Data Science - Course 9
# Data products
# 
# ########################################################################################

# Clear Environment
rm(list = ls())
#Close all graphs
graphics.off()

# ########################################################################################
# Libraries
# ########################################################################################

library(shiny)
library(miniUI)
library(googleVis)

###########################################################################################
# Create interactive side pannel
###########################################################################################

#Close button
myFirstGadget <- function(){
  ui<- miniPage(
    gadgetTitleBar("My First gadget")
  )
  server<-function(input,output,session){
    #Close the app
    observeEvent(input$done,{stopApp()})
  }
  runGadget(ui,server)
}

myFirstGadget()


#Text interface
multiplyNumbers <- function(numbers1,numbers2){
  ui<- miniPage(
    gadgetTitleBar("Multiply numbers"),
    miniContentPanel(
      selectInput("num1","First number",choices=numbers1),
      selectInput("num2","Second number",choices=numbers2)
    )
  )
  server<-function(input,output,session){
    #Close the app
    observeEvent(input$done,{
      num1<-as.numeric(input$num1)
      num2<-as.numeric(input$num2)
      stopApp(num1*num2)
    })
  }
  runGadget(ui,server)
}

multiplyNumbers(1:10,1:10)



#Select data frame from graph
pickTrees <- function(){
  ui<-miniPage(
    gadgetTitleBar("Select points with the mouse"),
    miniContentPanel(
      plotOutput("plot",height = "100%",brush = "brush")
    )
  )
  server<-function(input,output,session){
    output$plot <- renderPlot({
      plot(trees$Girth,trees$Volume,xlab="Grith",ylab="Volume")
    })
    observeEvent(input$done,{
      stopApp(brushedPoints(trees,input$brush,xvar="Girth",yvar="Volume"))
    })
  }
  runGadget(ui,server)
}

pickTrees()


#Libraries
install.packages("googleVis")
library(googleVis)

#Initialize DF

M<-gvisMotionChart(Fruits,"Fruit","Year",options=list(width=600,height=400))
plot(M)
#print(M) generates html that can be saved to a file
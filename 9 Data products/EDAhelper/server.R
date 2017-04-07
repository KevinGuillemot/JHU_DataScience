library(shiny)
library(datasets)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  datasetInput <- reactive({
    get(input$dataset)
  })
   
  output$datasetTitle<-renderText({input$dataset})
  
  output$featuretTitle<-renderText({input$feature})
  
  output$datasetStr <- renderPrint({
    str(datasetInput(),give.attr=FALSE)
  })
  
  output$missingTotal <- renderPrint({
    sum(!complete.cases(datasetInput()))
  })
  
  observe({
  s_options <-names(datasetInput())
  updateSelectInput(session, "feature",
                    choices = s_options
  )
  })
  
  output$missingFeature <- renderPrint({
    sum(!complete.cases(datasetInput()))
  })
  
})

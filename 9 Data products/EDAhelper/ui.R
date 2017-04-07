
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("EDA Helper"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput('dataset', 'Dataset', data(package = "datasets")$results[, "Item"]),
      selectInput('feature', 'Feature', c("Choose a dataset" = "option1"))

    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      h2("Data structure"),
      verbatimTextOutput("datasetStr"),
      h3("Missing values"),
      verbatimTextOutput("missingTotal"),
      
      h2("Feature analysis"),
      h3(textOutput("featuretTitle", container = span)),
      
      h3("Missing values"),
      verbatimTextOutput("missingFeature")
      
      h3("Distribution"),
      verbatimTextOutput("missingFeature")
      
    )
  )
))

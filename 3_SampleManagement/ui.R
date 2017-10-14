library(shiny)

shinyUI(fluidPage(

  titlePanel("Sampling Management Application"),
  
  sidebarLayout(
    sidebarPanel(
       textInput("barcode","Entry Your Barcode:")
    ),
    
    mainPanel(
       textOutput("barcode_query")
    )
  )
))
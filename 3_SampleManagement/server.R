library(openxlsx)
library(shiny)

td<-read.xlsx("F:\\Documentation\\Training\\Course_Development\\Standard_Course\\Instance\\3_SampleManagement\\td.xlsx",sheet=1)

shinyServer(function(input, output) {
   
  output$barcode_query <- renderPrint({
    if((input$barcode %in% td$id) & td$status[td$id==input$barcode]=="Approved") {
      print("This sample is approved, can be disposed")
    }
    else{
      print(paste("Pay Attention: the sample status for ", input$barcode, "is ", td$status[td$id==input$barcode]))
    } 
  })
  
})

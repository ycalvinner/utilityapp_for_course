library(shiny)
library(ggplot2)
library(openxlsx)
library(dplyr)
#WFO<-read.xlsx("E:/study/R/R_Code_myself/Drug_Quality_Management/trendapp/utilityapp/water.xlsx",sheet="WFO-Data-Input",detectDates = T)

shinyServer(function(input, output) {
  #WFO-TAMC######################
  raw_wfo<-reactive({
    inFile <- input$file_wfo
    if(is.null(inFile)) return(NULL)
    read.xlsx(inFile$datapath,sheet="WFO-Data-Input",detectDates = T) 
  })
  
  dataset_WFO<-reactive({
    if(is.null(raw_wfo())) return(NULL)
    if(input$point_wfo_ai=="Individual") {
      raw_wfo() %>%  filter(
        Date>=input$daterange_wfo[1],
        Date<=input$daterange_wfo[2],
        Point %in% input$point_wfo
      )
    }
    else{
      raw_wfo() %>%  filter(
        Date>=input$daterange_wfo[1],
        Date<=input$daterange_wfo[2]
      )
    } 
  })
  
  output$plot_wfo<-renderPlot({
    if(is.null(dataset_WFO())) return(NULL)
    if(!is.null(dataset_WFO())){
      td_wfo<-ggplot(dataset_WFO(),aes(x=Date,y=Result,color=Point))+geom_line()+geom_point()+scale_x_date(date_breaks="month",date_label="%Y-%m-%d")+theme(axis.text.x=element_text(angle=90))
    }
    ggsave("pic_wfo.png")
    td_wfo
  })
  
  output$download_wfo <- downloadHandler(
    filename = "pic_wfo.png",
    content = function(targetfile){
      file.copy("pic_wfo.png",targetfile)
    }
  )
  
  #PW-TAMC######################  
  raw_pw_tamc<-reactive({
    inFile <- input$file_pw
    if(is.null(inFile)) return(NULL)
    read.xlsx(inFile$datapath,sheet="PW-TAMC-Input",detectDates = T) 
  })
  
  dataset_PW_TAMC<-reactive({
    if(is.null(raw_pw_tamc())) return(NULL)
    if(input$point_pw_ai=="Individual") {
      raw_pw_tamc() %>%  filter(
        Date>=input$daterange_pw[1],
        Date<=input$daterange_pw[2],
        Point %in% input$point_pw
      )
    }
    else{
      raw_pw_tamc() %>%  filter(
        Date>=input$daterange_pw[1],
        Date<=input$daterange_pw[2]
      )
    } 
  })
  
  output$plot_pw_tamc<-renderPlot({
    if(is.null(dataset_PW_TAMC())) return(NULL)
    if(!is.null(dataset_PW_TAMC())) td_pw_tamc<-ggplot(dataset_PW_TAMC(),aes(x=Date,y=Result,color=Point))+geom_line()+geom_point()+scale_x_date(date_breaks="month",date_label="%Y-%m-%d")+theme(axis.text.x=element_text(angle=90))

    ggsave("pic_PW_TAMC.png")
    td_pw_tamc
  })
  
  output$download_pw_tamc <- downloadHandler(
    filename = "pic_PW_TAMC.png",
    content = function(targetfile){
      file.copy("pic_PW_TAMC.png",targetfile)
    }
  )
  #PW-Conductivity######################    
  raw_pw_conductivity<-reactive({
    inFile <- input$file_pw
    if(is.null(inFile)) return(NULL)
    read.xlsx(inFile$datapath,sheet="PW-Conductivity-Input",detectDates = T) 
  })
  
  dataset_PW_Conductivity<-reactive({
    if(is.null(raw_pw_conductivity())) return(NULL)
    if(input$point_pw_ai=="Individual") {
      raw_pw_conductivity() %>%  filter(
        Date>=input$daterange_pw[1],
        Date<=input$daterange_pw[2],
        Point %in% input$point_pw
      )
    }
    else{
      raw_pw_conductivity() %>%  filter(
        Date>=input$daterange_pw[1],
        Date<=input$daterange_pw[2]
      )
    } 
  })
  
  output$plot_pw_conductivity<-renderPlot({
    if(is.null(dataset_PW_Conductivity())) return(NULL)
    if(!is.null(dataset_PW_Conductivity())) td_pw_conductivity<-ggplot(dataset_PW_Conductivity(),aes(x=Date,y=Result,color=Point))+geom_line()+geom_point()+scale_x_date(date_breaks="month",date_label="%Y-%m-%d")+theme(axis.text.x=element_text(angle=90))

    ggsave("pic_PW_Conductivity.png")
    td_pw_conductivity
  })
  
  output$download_pw_conductivity <- downloadHandler(
    filename = "pic_PW_Conductivity.png",
    content = function(targetfile){
      file.copy("pic_PW_Conductivity.png",targetfile)
    }
  )  
  #PW-TOC######################    
  raw_pw_toc<-reactive({
    inFile <- input$file_pw
    if(is.null(inFile)) return(NULL)
    read.xlsx(inFile$datapath,sheet="PW-TOC-Input",detectDates = T) 
  })
  
  dataset_PW_toc<-reactive({
    if(is.null(raw_pw_toc())) return(NULL)
    if(input$point_pw_ai=="Individual") {
      raw_pw_toc() %>%  filter(
        Date>=input$daterange_pw[1],
        Date<=input$daterange_pw[2],
        Point %in% input$point_pw
      )
    }
    else{
      raw_pw_toc() %>%  filter(
        Date>=input$daterange_pw[1],
        Date<=input$daterange_pw[2]
      )
    } 
  })
  
  output$plot_pw_toc<-renderPlot({
    if(is.null(dataset_PW_toc())) return(NULL)
    if(!is.null(dataset_PW_toc())) td_pw_toc<-ggplot(dataset_PW_toc(),aes(x=Date,y=Result,color=Point))+geom_line()+geom_point()+scale_x_date(date_breaks="month",date_label="%Y-%m-%d")+theme(axis.text.x=element_text(angle=90))

    ggsave("pic_PW_TOC.png")
    td_pw_toc
  })
  
  output$download_pw_toc <- downloadHandler(
    filename = "pic_PW_TOC.png",
    content = function(targetfile){
      file.copy("pic_PW_TOC.png",targetfile)
    }
  )    

  
})









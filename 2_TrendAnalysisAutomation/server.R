library(shiny)
library(ggplot2)
library(openxlsx)
library(dplyr)
#WFO<-read.xlsx("E:/study/R/R_Code_myself/Drug_Quality_Management/trendapp/utilityapp/water.xlsx",sheet="WFO-Data-Input",detectDates = T)

shinyServer(function(input, output) {
  #WFO-TAMC######################
  tempd_wfo<-renderText({
    tempdir()
  })
  
  raw_wfo<-reactive({
    inFile <- input$file_wfo
    if(is.null(inFile)) return(NULL)
    read.xlsx(inFile$datapath,sheet="WFO-Data-Input",detectDates = T) 
  })
  
  output$ui_location_wfo<-renderUI({
    selectInput("point_wfo","Please select the sampling point",choices = unique(raw_wfo()$Point),multiple = TRUE)
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
    if(!is.null(dataset_WFO())) {
      #Filter viable creation.
      alertfilter<-dataset_WFO()$Alert
      alertfilter[is.na(alertfilter)]<-pi
      actionfilter<-dataset_WFO()$Action
      actionfilter[is.na(actionfilter)]<-pi
      alertund<-c(TRUE,diff(alertfilter)!=0)
      actionund<-c(TRUE,diff(actionfilter)!=0)
      #Alert limit line & blank point data process
      alertx<-as.Date(dataset_WFO()$Date[alertund])
      alerty<-dataset_WFO()$Alert[alertund]
      alertxend<-c(alertx[-1],as.Date(dataset_WFO()$Date[length(dataset_WFO()$Date)]))
      alertyend<-alerty
      alertline<-data.frame(alertx,alerty,alertxend,alertyend)
      alertline<-alertline[!is.na(alertline$alerty),]
      alertpoint<-data.frame(x=as.Date(alertx[-1]),y=alerty[-length(alerty)])
      alertpoint<-alertpoint[!is.na(alertpoint$y),]
      ##action limit line & blank point data process
      actionx<-as.Date(dataset_WFO()$Date[actionund])
      actiony<-dataset_WFO()$Action[actionund]
      actionxend<-c(actionx[-1],as.Date(dataset_WFO()$Date[length(dataset_WFO()$Date)]))
      actionyend<-actiony
      actionline<-data.frame(actionx,actiony,actionxend,actionyend)
      actionline<-actionline[!is.na(actionline$actiony),]
      actionpoint<-data.frame(x=as.Date(actionx[-1]),y=actiony[-length(actiony)])
      actionpoint<-actionpoint[!is.na(actionpoint$y),]
      ##Plot Creation
      td_wfo<-ggplot(dataset_WFO())+
        geom_point(aes(x=Date,y=Result,colour=Point,shape=Point))+
        geom_line(aes(x=Date,y=Result,colour=Point))+
        scale_x_date(date_breaks="1 month")+
        theme(axis.text.x=element_text(angle=90,vjust=0.5))+
        ylim(c(0,max(dataset_WFO()$Action)*1.1))+
        geom_segment(data=alertline,aes(x=alertx,y=alerty,xend=alertxend,yend=alertyend),
                     colour="#FFD700")+
        geom_point(data=alertpoint,aes(x,y),shape=1,color="#FFD700")+
        geom_segment(data=actionline,aes(x=actionx,y=actiony,xend=actionxend,yend=actionyend),
                     colour="#CD6600")+
        geom_point(data=actionpoint,aes(x,y),shape=1,color="#CD6600")+
        annotate("text",
                 x=as.Date(rep(alertx[1],2)),y=c(alerty[1],actiony[1]),
                 label=c("Alert Limit","Action Limit"),size=3,hjust=0,vjust=-0.2) 
    }
    ggsave("pic_wfo.png",path=tempd_wfo())
    td_wfo
  })
  
  output$report_wfo <- downloadHandler(
    filename=function(){
      paste('my-report', sep = '.', switch(
        input$format_wfo, PDF = 'pdf', HTML = 'html', WORD= 'docx'
      ))
    },
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd<-getwd()
      setwd(tempd_wfo())
      on.exit(setwd(owd))
      library(rmarkdown)
      
      file.copy(src, "report.Rmd", overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format_wfo,
        PDF = pdf_document(), HTML = html_document(), WORD = word_document()
      ))
      file.copy(out, file)
    }
  )
  
  #PW-TAMC######################  
  raw_pw_tamc<-reactive({
    inFile <- input$file_pw
    if(is.null(inFile)) return(NULL)
    read.xlsx(inFile$datapath,sheet="PW-TAMC-Input",detectDates = T) 
  })
  
  output$ui_location_pw<-renderUI({
    selectInput("point_pw","Please select the sampling point",choices = unique(raw_pw_tamc()$Point),multiple = TRUE)
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
    if(!is.null(dataset_PW_TAMC())){
      #Filter viable creation.
    alertfilter<-dataset_PW_TAMC()$Alert
    alertfilter[is.na(alertfilter)]<-pi
    actionfilter<-dataset_PW_TAMC()$Action
    actionfilter[is.na(actionfilter)]<-pi
    alertund<-c(TRUE,diff(alertfilter)!=0)
    actionund<-c(TRUE,diff(actionfilter)!=0)
    #Alert limit line & blank point data process
    alertx<-as.Date(dataset_PW_TAMC()$Date[alertund])
    alerty<-dataset_PW_TAMC()$Alert[alertund]
    alertxend<-c(alertx[-1],as.Date(dataset_PW_TAMC()$Date[length(dataset_PW_TAMC()$Date)]))
    alertyend<-alerty
    alertline<-data.frame(alertx,alerty,alertxend,alertyend)
    alertline<-alertline[!is.na(alertline$alerty),]
    alertpoint<-data.frame(x=as.Date(alertx[-1]),y=alerty[-length(alerty)])
    alertpoint<-alertpoint[!is.na(alertpoint$y),]
    ##action limit line & blank point data process
    actionx<-as.Date(dataset_PW_TAMC()$Date[actionund])
    actiony<-dataset_PW_TAMC()$Action[actionund]
    actionxend<-c(actionx[-1],as.Date(dataset_PW_TAMC()$Date[length(dataset_PW_TAMC()$Date)]))
    actionyend<-actiony
    actionline<-data.frame(actionx,actiony,actionxend,actionyend)
    actionline<-actionline[!is.na(actionline$actiony),]
    actionpoint<-data.frame(x=as.Date(actionx[-1]),y=actiony[-length(actiony)])
    actionpoint<-actionpoint[!is.na(actionpoint$y),]
    ##Plot Creation
    td_pw_tamc<-ggplot(dataset_PW_TAMC())+
      geom_point(aes(x=Date,y=Result,colour=Point,shape=Point))+
      geom_line(aes(x=Date,y=Result,colour=Point))+
      scale_x_date(date_breaks="1 month")+
      theme(axis.text.x=element_text(angle=90,vjust=0.5))+
      ylim(c(0,max(dataset_PW_TAMC()$Action)*1.1))+
      geom_segment(data=alertline,aes(x=alertx,y=alerty,xend=alertxend,yend=alertyend),
                   colour="#FFD700")+
      geom_point(data=alertpoint,aes(x,y),shape=1,color="#FFD700")+
      geom_segment(data=actionline,aes(x=actionx,y=actiony,xend=actionxend,yend=actionyend),
                   colour="#CD6600")+
      geom_point(data=actionpoint,aes(x,y),shape=1,color="#CD6600")+
      annotate("text",
               x=as.Date(rep(alertx[1],2)),y=c(alerty[1],actiony[1]),
               label=c("Alert Limit","Action Limit"),size=3,hjust=0,vjust=-0.2) 
    }
    #old code
    #td_pw_tamc<-ggplot(dataset_PW_TAMC(),aes(x=Date,y=Result,color=Point))+geom_line()
    ggsave("pic_PW_TAMC.png",path=tempd_wfo())
    td_pw_tamc
  })
  
  output$download_pw_tamc <- downloadHandler(
    filename = "pic_PW_TAMC.png",
    content = function(targetfile){
      file.copy(paste(tempd_wfo(),"/pic_PW_TAMC.png",sep=""),targetfile)
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
    if(!is.null(dataset_PW_Conductivity())){
      #Filter viable creation.
      alertfilter<-dataset_PW_Conductivity()$Alert
      alertfilter[is.na(alertfilter)]<-pi
      actionfilter<-dataset_PW_Conductivity()$Action
      actionfilter[is.na(actionfilter)]<-pi
      alertund<-c(TRUE,diff(alertfilter)!=0)
      actionund<-c(TRUE,diff(actionfilter)!=0)
      #Alert limit line & blank point data process
      alertx<-as.Date(dataset_PW_Conductivity()$Date[alertund])
      alerty<-dataset_PW_Conductivity()$Alert[alertund]
      alertxend<-c(alertx[-1],as.Date(dataset_PW_Conductivity()$Date[length(dataset_PW_Conductivity()$Date)]))
      alertyend<-alerty
      alertline<-data.frame(alertx,alerty,alertxend,alertyend)
      alertline<-alertline[!is.na(alertline$alerty),]
      alertpoint<-data.frame(x=as.Date(alertx[-1]),y=alerty[-length(alerty)])
      alertpoint<-alertpoint[!is.na(alertpoint$y),]
      ##action limit line & blank point data process
      actionx<-as.Date(dataset_PW_Conductivity()$Date[actionund])
      actiony<-dataset_PW_Conductivity()$Action[actionund]
      actionxend<-c(actionx[-1],as.Date(dataset_PW_Conductivity()$Date[length(dataset_PW_Conductivity()$Date)]))
      actionyend<-actiony
      actionline<-data.frame(actionx,actiony,actionxend,actionyend)
      actionline<-actionline[!is.na(actionline$actiony),]
      actionpoint<-data.frame(x=as.Date(actionx[-1]),y=actiony[-length(actiony)])
      actionpoint<-actionpoint[!is.na(actionpoint$y),]
      ##Plot Creation
      td_pw_conductivity<-ggplot(dataset_PW_Conductivity())+
        geom_point(aes(x=Date,y=Result,colour=Point,shape=Point))+
        geom_line(aes(x=Date,y=Result,colour=Point))+
        scale_x_date(date_breaks="1 month")+
        theme(axis.text.x=element_text(angle=90,vjust=0.5))+
        ylim(c(0,max(dataset_PW_Conductivity()$Action)*1.1))+
        geom_segment(data=alertline,aes(x=alertx,y=alerty,xend=alertxend,yend=alertyend),
                     colour="#FFD700")+
        geom_point(data=alertpoint,aes(x,y),shape=1,color="#FFD700")+
        geom_segment(data=actionline,aes(x=actionx,y=actiony,xend=actionxend,yend=actionyend),
                     colour="#CD6600")+
        geom_point(data=actionpoint,aes(x,y),shape=1,color="#CD6600")+
        annotate("text",
                 x=as.Date(rep(alertx[1],2)),y=c(alerty[1],actiony[1]),
                 label=c("Alert Limit","Action Limit"),size=3,hjust=0,vjust=-0.2) 
    }
    ggsave("pic_PW_Conductivity.png",path=tempd_wfo())
    td_pw_conductivity
  })
  
  output$download_pw_conductivity <- downloadHandler(
    filename = "pic_PW_Conductivity.png",
    content = function(targetfile){
      file.copy(paste(tempd_wfo(),"/pic_PW_Conductivity.png",sep=""),targetfile)
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
    if(!is.null(dataset_PW_toc())){
      #Filter viable creation.
      alertfilter<-dataset_PW_toc()$Alert
      alertfilter[is.na(alertfilter)]<-pi
      actionfilter<-dataset_PW_toc()$Action
      actionfilter[is.na(actionfilter)]<-pi
      alertund<-c(TRUE,diff(alertfilter)!=0)
      actionund<-c(TRUE,diff(actionfilter)!=0)
      #Alert limit line & blank point data process
      alertx<-as.Date(dataset_PW_toc()$Date[alertund])
      alerty<-dataset_PW_toc()$Alert[alertund]
      alertxend<-c(alertx[-1],as.Date(dataset_PW_toc()$Date[length(dataset_PW_toc()$Date)]))
      alertyend<-alerty
      alertline<-data.frame(alertx,alerty,alertxend,alertyend)
      alertline<-alertline[!is.na(alertline$alerty),]
      alertpoint<-data.frame(x=as.Date(alertx[-1]),y=alerty[-length(alerty)])
      alertpoint<-alertpoint[!is.na(alertpoint$y),]
      ##action limit line & blank point data process
      actionx<-as.Date(dataset_PW_toc()$Date[actionund])
      actiony<-dataset_PW_toc()$Action[actionund]
      actionxend<-c(actionx[-1],as.Date(dataset_PW_toc()$Date[length(dataset_PW_toc()$Date)]))
      actionyend<-actiony
      actionline<-data.frame(actionx,actiony,actionxend,actionyend)
      actionline<-actionline[!is.na(actionline$actiony),]
      actionpoint<-data.frame(x=as.Date(actionx[-1]),y=actiony[-length(actiony)])
      actionpoint<-actionpoint[!is.na(actionpoint$y),]
      ##Plot Creation
      td_pw_toc<-ggplot(dataset_PW_toc())+
        geom_point(aes(x=Date,y=Result,colour=Point,shape=Point))+
        geom_line(aes(x=Date,y=Result,colour=Point))+
        scale_x_date(date_breaks="1 month")+
        theme(axis.text.x=element_text(angle=90,vjust=0.5))+
        ylim(c(0,max(dataset_PW_toc()$Action)*1.1))+
        geom_segment(data=alertline,aes(x=alertx,y=alerty,xend=alertxend,yend=alertyend),
                     colour="#FFD700")+
        geom_point(data=alertpoint,aes(x,y),shape=1,color="#FFD700")+
        geom_segment(data=actionline,aes(x=actionx,y=actiony,xend=actionxend,yend=actionyend),
                     colour="#CD6600")+
        geom_point(data=actionpoint,aes(x,y),shape=1,color="#CD6600")+
        annotate("text",
                 x=as.Date(rep(alertx[1],2)),y=c(alerty[1],actiony[1]),
                 label=c("Alert Limit","Action Limit"),size=3,hjust=0,vjust=-0.2) 
    }
      #td_pw_toc<-ggplot(dataset_PW_toc(),aes(x=Date,y=Result,color=Point))+geom_line()
    ggsave("pic_PW_TOC.png",path=tempd_wfo())
    td_pw_toc
  })
  
  output$download_pw_toc <- downloadHandler(
    filename = "pic_PW_TOC.png",
    content = function(targetfile){
     file.copy(paste(tempd_wfo(),"/pic_PW_TOC.png",sep=""),targetfile)
    }
  )
})

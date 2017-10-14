library(shiny)
library(shinythemes)

shinyUI(tagList(
  shinythemes::themeSelector(),
  navbarPage(
    "Utility Trend Analysis Application",
    #
    tabPanel("Water-WFO",
             sidebarPanel(
               fileInput("file_wfo","Please Select File"),
               dateRangeInput("daterange_wfo","Please input the date range",
                              start=Sys.Date()-360,end=Sys.Date(),
                              format="yyyy-mm-dd"),
               downloadButton('download_wfo', 'Download'),
               radioButtons("point_wfo_ai","Point Overall selection",choices=list("All-point selection will not work"="All","Individual"="Individual")),
               selectInput("point_wfo","Point Individual Selection - Multi allow",
                           choice=list(
                             "Point-1","Point-2","Point-3"),
                           multiple=TRUE
               )
             ),
             
             mainPanel(
               tabsetPanel(
                 tabPanel("TAMC",
                          plotOutput("plot_wfo")
                 )
               )
             )
    ),
    #
    tabPanel("Water-PW",
             sidebarPanel(
               fileInput("file_pw","Upload PW File"),
               dateRangeInput("daterange_pw","Please input the date range",
                              start=Sys.Date()-360,end=Sys.Date(),
                              format="yyyy-mm-dd"),
               radioButtons("point_pw_ai","Point Overall selection",choices=list("All-point selection will not work"="All","Individual"="Individual")),
               selectInput("point_pw","Point Individual Selection - Multi allow",
                           choice=list(
                             "Point-1","Point-2","Point-3"),
                           multiple=TRUE
               )
             ),
             mainPanel(
               tabsetPanel(
                 ##
                 tabPanel("TAMC",
                          br(),
                          h5("Plot can be download from the bellow button:"),
                          downloadButton('download_pw_tamc', 'Download'),
                          br(),
                          plotOutput("plot_pw_tamc")
                 ),
                 ##
                 tabPanel("Conductivity",
                          br(),
                          h5("Plot can be download from the bellow button:"),
                          downloadButton('download_pw_conductivity', 'Download'),
                          br(),
                          plotOutput("plot_pw_conductivity")        
                 ),
                 ##
                 tabPanel("TOC",
                          br(),
                          h5("Plot can be download from the bellow button:"),
                          downloadButton('download_pw_toc', 'Download'),
                          br(),
                          plotOutput("plot_pw_toc")
                 )
               )
             )
    )
  )))
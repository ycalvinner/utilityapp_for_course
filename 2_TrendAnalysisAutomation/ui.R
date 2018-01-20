library(shiny)
library(shinythemes)

shinyUI(tagList(
#  shinythemes::themeSelector(),
  navbarPage(
    "Utility Trend Analysis Application",
    theme=shinytheme("cerulean"),
	
    tabPanel("Water-WFO",
             fluidRow(
                column(3,
                      wellPanel(
                        fileInput("file_wfo","Please Select File"),
                        dateRangeInput("daterange_wfo","Please input the date range",
                                      start=Sys.Date()-360,end=Sys.Date(),
                                      format="yyyy-mm-dd"),
                        radioButtons("point_wfo_ai","Point Overall selection",choices=list("All-point selection will not work"="All","Individual"="Individual")),
                        conditionalPanel(
                                         condition='input.point_wfo_ai!="All"',
                                         uiOutput("ui_location_wfo")
                        ),
                        br(),
                        actionButton("action_ctl","Apply")
                      ),
                      wellPanel(
                        radioButtons("report_mode_wfo","Open/Close Report Mode",choices=c("Yes","No"),selected="No",inline=TRUE),
                        br(),
                        radioButtons("format_wfo","Document Format",choices = c("HTML","PDF","WORD"),inline = TRUE),
                        br(),
                        downloadButton('report_wfo', 'Download')
                      )
                ),
                column(9,
                       fluidRow(
                         h5(textOutput("currenttime"))
                       ),
                       fluidRow(
                                tabsetPanel(id="Tab_WFO",
                                  tabPanel("TAMC",
                                           conditionalPanel(
                                             condition='input.report_mode_wfo=="Yes"',
                                             textAreaInput("des_wfo_pre","Please Input the system descripton",value = "",width = '100%')
                                           ),
                                           plotOutput("plot_wfo",
                                                     click=clickOpts("wfo_click"),
                                                     dblclick = dblclickOpts(id="wfo_dbclick"),
                                                     hover = hoverOpts(id="wfo_hover"),
                                                     brush = brushOpts(id="wfo_brush")
                                          ),
                                          verbatimTextOutput("test"),
                                          fluidRow(
                                            tabsetPanel(
                                                        tabPanel("click",verbatimTextOutput("click_info_wfo")),
                                                        tabPanel("hover",verbatimTextOutput("hover_info_wfo")),
                                                        tabPanel("brush",verbatimTextOutput("brush_info_wfo"))
                                            )),
                                          conditionalPanel(
                                            condition='input.report_mode_wfo=="Yes"',
                                            textAreaInput("des_wfo_summary","Please Input conclusion",value="")
                                          )
                                        
                                  )
                                )
                       )
               )
            )
    ),
    #
    tabPanel("Water-PW",
             fluidRow(
                column(width=3,
                       wellPanel(
                          fileInput("file_pw","Upload PW File"),
                          dateRangeInput("daterange_pw","Please input the date range",
                                          start=Sys.Date()-360,end=Sys.Date(),
                                          format="yyyy-mm-dd"),
                          radioButtons("point_pw_ai","Point Overall selection",choices=list("All-point selection will not work"="All","Individual"="Individual")),
                          uiOutput("ui_location_pw")
                       )
                ),
                column(width=9,
                       fluidRow(
                         tabsetPanel(id="Tab_PW",
                           ##
                           tabPanel("TAMC",
                                    br(),
                                    h5("Plot can be download from the bellow button:"),
                                    downloadButton('download_pw_tamc', 'Download'),
                                    br(),
                                    plotOutput("plot_pw_tamc"),
                           value="TAMC"
                           ),
                           ##
                           tabPanel("Conductivity",
                                    br(),
                                    h5("Plot can be download from the bellow button:"),
                                    downloadButton('download_pw_conductivity', 'Download'),
                                    br(),
                                    plotOutput("plot_pw_conductivity")  ,
                           value="Conductivity"
                           ),
                           ##
                           tabPanel("TOC",
                                    br(),
                                    h5("Plot can be download from the bellow button:"),
                                    downloadButton('download_pw_toc', 'Download'),
                                    br(),
                                    plotOutput("plot_pw_toc"),
                           value="TOC"    
                           )
                         )
                       )
                )
             )
    ),
    navbarMenu("More",
    tabPanel("Other",
      h3("Page is on developing")
    ))
  )
))
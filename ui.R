# ashish Toppo
#Data Exploration app


# Load R packages
library(shiny)
library(shinythemes)
library(DT)
library(plotly)
library(rmarkdown)
library(knitr)

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  "Data Exploration Dashboard",
                  tabPanel("Upload & Plot",
                           
                           sidebarPanel(
                             fileInput('target_upload', 'Choose file to upload',
                                       accept = c(
                                         'text/csv',
                                         'text/comma-separated-values',
                                         '.csv'
                                       )),
                             radioButtons("separator","Separator: ",choices = c(",",";",":"), selected=",",inline=TRUE),
                             actionButton("go","Load Variables"),
                             h5("For Page-1(Univariate Analysis)"),
                             #h6("Select Variable For Univariate Analysis(Num)"),
                             uiOutput("col1"),
                             #h6("Select Variable For Univariate Analysis(Cat)"),
                             uiOutput("col2"),
                             h5("For Page-2 (Num Vs Num)"),
                             #h6("Select Variables for Bivariate Analysis(Num Vs Num)"),
                             uiOutput("col3"),
                             uiOutput("col4"),
                             uiOutput("col5"),
                             h5("For Page-3 (cat Vs Cat)"),
                             #h6("Select Variables for Bivariate Analysis(Cat Vs Cat)"),
                             uiOutput("col6"),
                             uiOutput("col7"),
                             #radioButtons('format', 'Document format', c('PDF', 'HTML'),
                             #inline = TRUE),
                             downloadButton("report", "Download report"),
                           ), # sidebarPanel
                           br(),
                           br(),
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Page-1", 
                                        fluidRow(
                                          actionButton("page1","Plot"),
                                          column(11,
                                                 plotlyOutput("plot1")
                                                 #plotOutput("plot1")
                                          ),
                                          column(11,
                                                 plotlyOutput("plot2")
                                                 #plotOutput("plot2")
                                          )
                                        )
                               ),
                               tabPanel("Page-2", 
                                        fluidRow(
                                          actionButton("page2","Plot"),
                                          column(11,
                                                 plotlyOutput("plot3")
                                                 #plotOutput("plot3")
                                          ),
                                          column(11,
                                                 plotlyOutput("plot4")
                                                 #plotOutput("plot4")
                                          )
                                        )
                               ),
                               tabPanel("Page-3", 
                                        fluidRow(
                                          actionButton("page3","Plot"),
                                          column(11,
                                                 plotlyOutput("plot5")
                                                 #plotOutput("plot5")
                                          ),
                                          dataTableOutput("tableop")
                                        )
                               )
                             )
                           )#mainpanel
                  ), # Navbar 1, tabPanel
                  
                  
                  tabPanel(
                    "Table",
                    dataTableOutput("sample_table")
                  ),# sidebarPanel
                  
                  tabPanel("About",
                           h1("About Me:"),
                           h4("Ashish Toppo"),
                           h4("Data Scientist"),
                           p("I am data scientist with experience in data visualizaion, model building and deployment. 
                             This small work is to show my experience and knowledge regarding Shiny, development and deployment of the app. 
                             I learned quite a few things while doing this project, no wonder I felt refreshed. 
                             I hope this small work will give you some intuition about my work. 
                             "),
                           h1("How to use this App!"),
                           pre(includeText("htu.txt")),
                           tags$a(href="https://github.com/Tashish97/Model3", "Click here!, For Github Page")
                  )
                  
                ) # navbarPage
)# fluidPage


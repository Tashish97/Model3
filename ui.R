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
                             actionButton("go","Click To Plot"),
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
                           h2("Ashish Toppo"),
                           h3("Data Scientist"),
                           p("I am data scientist with experience in data visualizaion, model building and deployment. 
                             This small work is to show my experience and knowledge regarding Shiny, development and deployment of the app. 
                             I learned quite a few things while doing this project, no wonder I felt refreshed. 
                             I hope this small work will give you some intuition about my work. 
                             "),
                           br(),
                           h1("How to use the app:"),
                           h2("The Plots are all available within a single page with controls on the left while navigation panel on the center top helps you to quickly swich between pages."),
                           p("The app is basically dividedin 3 Parts: Data uploading and visualization, Table based Filtering and About page."),
                           p("1. Upload a file with the available extension(.csv) and select the separator from the Radio Button Option."),
                           p("2. All the operations on the page require you to click the 'Click to Plot' Button."),
                           p("3. Once the file is uploaded and 'Click to plot' clicked, play around with the features which are already filtered as per their class types."),
                           p("4. Data Analysis can be done using the plots, and Particular plots can be downloaded to your Local Drive."),
                           p("5. A report can be generated using 'Download Report' Button."),
                           p("6. For large dataset(max 5MB) plotting might seem to be slow since there are many reactive values and reactive elements your patience is required."),
                           p("7. On the 'Table Page' you can choose the type of filters you want based on the nature of the variables."),
                           p("8. While downloading a filtered table you have to keep few things thing in mind:"),
                           p("   8.1 Always select Show Entries to All before Downloading."),
                           p("   8.2 It's important to note that only the section that you see on the page will be snapped as a PDF file, so it's much better to go save as Excel file to save the filtered table."),
                           p("   8.3 It's the same when large number of features are present. "),
                           p("   8.4 No need to specify any extensions just give the path."),
                           br(),
                           p("--Ashish Toppo"),
                           br(),
                           br(),
                           tags$a(href="https://github.com/Tashish97/Model3", "Click here!, For Github Page")
                  )
                  
                ) # navbarPage
)# fluidPage


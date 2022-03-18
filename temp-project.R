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

# Define server function  
server <- function(input, output) {
  # start of tabpanel1
  df_local <- eventReactive(input$go,{
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator, stringsAsFactors = TRUE)
    return(df)
  })
  output$sample_table <- renderDT(
    datatable(data=df_local(),
              filter = "top",
              class = "display nowrap compact",
              extensions = c("Buttons",'FixedColumns'),
              options = list( 
                scrollX = TRUE,
                fixedColumns = TRUE,
                dom = "Blfrtip"
                , buttons = 
                  list("copy", list(
                    extend = "collection"
                    , buttons = c("csv", "excel", "pdf")
                    , text = "Download"
                  ) ) # end of buttons customization
                
                # customize the length menu
                , lengthMenu = list( c(10, 20, 50, 100,-1) # declare values
                                     , c(10, 20, 50, 100,"All") # declare titles
                ) # end of lengthMenu customization
                , pageLength = 10
              )#end of options
    )
  )#renderDT
  output$report <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', 'html')
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <- list(df = df_local(),col1 = req(input$column1),col2 = req(input$column2),col3 = req(input$column3),
                     col4 = req(input$column4),col5 = req(input$column5),col6 = req(input$column6),
                     col7 = req(input$column7))
      
      library(rmarkdown)
      out <- render('report.Rmd', html_document(),params = params,
                    envir = new.env(parent = globalenv())
      )
      file.rename(out, file)
    }
  )
  
  
  # end of tabpanel1
  #############################################################################################################  
  # start of tabpanel2
  output$col1 <- renderUI({
    idx <- which(!grepl("factor",sapply(df_local(), class)))
    selectInput("column1","Select a Numerical Variable",
                choices=c(names(df_local()[idx])),
                selected = "not_sel")
  })
  output$tableop <- renderTable({
    summary(df_local()[req(input$column6)])
  })
  output$col2 <- renderUI({
    idx <- which(grepl("factor",sapply(df_local(), class)))
    selectInput("column2","Select a Categorical Variable",
                choices=c(names(df_local()[idx])),
                selected = "not_sel")
  })
  output$col3 <- renderUI({
    idx <- which(!grepl("factor",sapply(df_local(), class)))
    selectInput("column3","Select Column 1(Numeric Only)",
                choices=c(names(df_local()[idx])),
                selected = "not_sel")
  })
  
  output$col4 <- renderUI({
    idx <- which(!grepl("factor",sapply(df_local(), class)))
    selectInput("column4","Select Column 2(Numeric only)",
                choices=c(names(df_local()[idx])),
                selected = "not_sel")
  })
  output$col5 <- renderUI({
    idx <- which(grepl("factor",sapply(df_local(), class)))
    selectInput("column5","Select Hue For Num Vs Num (Categorical only)",
                choices=c(names(df_local()[idx])),
                selected = "not_sel")
  })
  output$col6 <- renderUI({
    idx <- which(grepl("factor",sapply(df_local(), class)))
    selectInput("column6","Select a Categorical Variable",
                choices=c(names(df_local()[idx])),
                selected = "not_sel")
  })
  output$col7 <- renderUI({
    idx <- which(grepl("factor",sapply(df_local(), class)))
    selectInput("column7","Select Hue For Categorical Variable",
                choices=c(names(df_local()[idx])),
                selected = "not_sel")
  })
  p1 <- eventReactive(input$page1,{
    ggplotly(df_local() %>% ggplot(aes_string(req(input$column1)))+geom_histogram(fill="green",color="blue")+labs(title = paste("Plot For",{req(input$column1)})))
  })
  p2 <- eventReactive(input$page1,{
    ggplotly(df_local() %>% ggplot(aes_string(req(input$column2)))+geom_bar()+labs(title = paste("Plot For",{req(input$column2)})))
  })
  p3 <- eventReactive(input$page2,{
    ggplotly(df_local() %>% ggplot(aes_string(req(input$column3),req(input$column4)))+geom_point()+geom_smooth()+labs(title = paste("Plot For",{req(input$column3)},"Vs",{req(input$column4)})))
  })
  p4 <- eventReactive(input$page2,{
    ggplotly(df_local() %>% ggplot(aes_string(req(input$column3),req(input$column4)))+geom_point(aes_string(color={req(input$column5)}))+labs(title = paste("Plot For",{req(input$column3)},"Vs",{req(input$column4)},"against",{req(input$column5)})))
  })
  p5 <- eventReactive(input$page3,{
    ggplotly(df_local() %>% ggplot(aes_string(req(input$column6)))+geom_bar(aes_string(fill={req(input$column7)}),position = "dodge")+labs(title = paste("Plot For",{req(input$column6)},"Vs",{req(input$column7)})))
  })
  output$plot1 <- renderPlotly({
    p1()
  })
  output$plot2 <- renderPlotly({
    p2()
  })
  output$plot3 <- renderPlotly({
    p3()
  })
  output$plot4 <- renderPlotly({
    p4()
  })
  output$plot5 <- renderPlotly({
    p5()
  })
  
  # end of tabpanel2
  #############################################################################################################  
  
  
} # server


#app call
shinyApp(ui,server)
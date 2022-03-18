#ashish toppo

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
  output$plot1 <- renderPlotly({
    #plot_ly(data = req(df_local()),x=as.formula(paste0('~',req(input$column1))),type = "histogram")#as.formula(paste0('~', my_x))
    ggplotly(df_local() %>% ggplot(aes_string(req(input$column1)))+geom_histogram(fill="green",color="blue")+labs(title = paste("Plot For",{req(input$column1)})))
  })
  output$plot2 <- renderPlotly({
    #plot_ly(data = req(dt_local()),x=as.formula(paste0('~',req(input$column1))),y=as.formula(paste0('~',req(input$column1))))
    ggplotly(df_local() %>% ggplot(aes_string(req(input$column2)))+geom_bar()+labs(title = paste("Plot For",{req(input$column2)})))
  })
  output$plot3 <- renderPlotly({
    ggplotly(df_local() %>% ggplot(aes_string(req(input$column3),req(input$column4)))+geom_point()+geom_smooth()+labs(title = paste("Plot For",{req(input$column3)},"Vs",{req(input$column4)})))
  })
  output$plot4 <- renderPlotly({
    ggplotly(df_local() %>% ggplot(aes_string(req(input$column3),req(input$column4)))+geom_point(aes_string(color={req(input$column5)}))+labs(title = paste("Plot For",{req(input$column3)},"Vs",{req(input$column4)},"against",{req(input$column5)})))
  })
  output$plot5 <- renderPlotly({
    ggplotly(df_local() %>% ggplot(aes_string(req(input$column6)))+geom_bar(aes_string(fill={req(input$column7)}),position = "dodge")+labs(title = paste("Plot For",{req(input$column6)},"Vs",{req(input$column7)})))
  })
  
  # end of tabpanel2
  #############################################################################################################  
  
  
} # server

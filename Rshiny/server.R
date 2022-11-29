library(ggplot2)
server <- function(session, input, output) {
  
  options(shiny.maxRequestSize=30*1024^2)
  
  
  data <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    inFile <- input$file1 
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$entêtes,
                       sep = input$sep
        )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    updateSelectInput(session,"varx",choices=colnames(df))
    updateSelectInput(session,"vary",choices=colnames(df))
    
      return(df)
})
  

  
  output$contents <- renderTable({
    x <- data()
    x <- x[1:30,1:10]
    return(x)
  })
  
  output$varx <- renderText({
    varx <- input$varx
    return(paste('Variable', varx))
  })
  
  output$vary <- renderText({
    vary <- input$vary
    return(paste('Variable', vary))
  })
  
  output$sumx <- renderPrint({
    df <- data()[,input$varx]
    summary(df)
  })
  
  output$sumy <- renderPrint({
    df <- data()[,input$vary]
    summary(df)
  })
  
  output$plot1 <- renderPlot({
    gg_miss_var(data())
  })
  
}


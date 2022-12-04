library(ggplot2)
setwd('/home/pierre/Documents/Master/Semestre_1/R/projet/PLSDA_R_Package/code')
source('./split_sample.r')
source('./sel_forward.R')
source('./plot.R')
source('./fit.R')
source('./cv.R')
source('./scale.r')
source('./dummies.r')
source('./nipals.r')
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
    updateSelectInput(session,"varcible",choices=colnames(df))
    updateSelectInput(session,"varexpli",choices=colnames(df))
    updateSelectInput(session,"varciblesel",choices=colnames(df))
    
      return(df)
})
  

  select_var <- eventReactive(input$submit_var,{
    
    varcible <- input$varcible
    varexpli <- as.vector(input$varexpli)
    chaine <- paste(varcible, paste(varexpli, collapse = '+'), sep = "~")
    return(length(plsda.split_sample(as.formula(chaine), data())$Xtrain))
  })
  
  select_forward <- eventReactive(input$submit_forward, {
    
    varciblef <- input$varciblesel
    form <- as.formula(paste(varciblef, '~', '.'))
    result <-sel.forward(form, data()) 
    return(result)
  })
  
    output$splitsample <- renderPrint({
      select_var()
    })
    
    
    output$forward <- renderPrint({
      select_forward()
    })
  
  output$contents <- renderTable({
    x <- data()
    if (ncol(x) < 10){
      return(x[1:30,1:ncol(x)])
    } else { return(x[1:30,1:10])}
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
  
  output$corr <- renderPlot({
    fit <- plslda.fit(Species~., iris)
    cercle_correlation.PLSDA(fit, 'PC1', 'PC2')
  })

  
}


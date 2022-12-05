library(ggplot2)
library(plslda)
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

  
  select_forward <- eventReactive(input$submit_forward, {
    varciblef <- input$varciblesel
    form <- as.formula(paste(varciblef, '~', '.')) 
    result <-colnames(plslda::sel.forward(form, data()))[-1] 
    chaine <- paste(varciblef, paste(result,collapse = '+'), sep='~')
    fit <- plslda::plslda.fit(as.formula(chaine), data())
    updateSelectInput(session,"nb_compx_cercle",choices=colnames((fit$comp_X)))
    updateSelectInput(session,"nb_compy_cercle",choices=colnames((fit$comp_Y)))
    updateSelectInput(session,"nb_compx_proj",choices=colnames((fit$comp_X)))
    updateSelectInput(session,"nb_compy_proj",choices=colnames((fit$comp_Y)))
    updateSelectInput(session,"used",choices=colnames((fit$comp_X)))
    
    return(list('sortie'=result, 'fit'=fit, 'chaine'=chaine))
  })
  
    
    output$forward <- renderPrint({
    plslda::summary.PLSDA(select_forward()$fit)
    })
    
    output$forward2 <- renderPrint({
      plslda::print.PLSDA(select_forward()$fit)
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
  
  cercle <- eventReactive(input$cercle_var, {
    return(plslda::cercle_correlation.PLSDA(select_forward()$fit, input$nb_compx_cercle, input$nb_compy_cercle))
  })
  
  projection <- eventReactive((input$proj_var), {
    return(plslda::plan_factoriel.PLSDA(select_forward()$fit, input$nb_compx_proj, input$nb_compy_proj))
  })
  
  matrice <- eventReactive(input$matrice, {
    return(plslda::correlationplot.PLSDA(select_forward()$fit, input$used))
  })

  output$matrice <- renderPlot({
    matrice()
  })
  
  output$cerclevar <- renderPlot({
    cercle()
  })
  
  output$proportion <- renderPlot({
    plslda::propz.PLSDA(select_forward()$fit)
  })
  
  output$projvar <- renderPlot({
    projection()
  })
}
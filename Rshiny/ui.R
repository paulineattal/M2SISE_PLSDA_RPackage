library(shiny)


ui <- fluidPage(
  
  tags$head(
    
    includeCSS("www/style.css")
  ),
  
  navbarPage("PLSDA PACKAGE - SISE",
             tabPanel(tags$h5("Importer ses données !"),
                      
                      fluidPage(
                        sidebarPanel(
                          
                            fileInput("file1", "Choose CSV File",
                                      accept = c(
                                        "text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")
                            ),
                            checkboxInput("entêtes", "En-têtes", TRUE),
                            
                            radioButtons("sep", "Separator",
                                         choices = c("Virgule" = ",",
                                                     "Point-virgule" = ";",
                                                     "Tabulation" = "\t",
                                                     "Space" = " "),
                                         selected = ",")
                            
                            ),
                            
                          mainPanel(
                            tableOutput("contents")
                          )
                      )
                      ),
                          
             tabPanel(
               
               tags$h5("Sélectionner ses variables"),
               fluidPage(
                 sidebarPanel(
                   selectInput(
                     'varx', 'Variables x :', choices = ""),
                   selectInput(
                     'vary', 'Variables y :', choices = "")
                 ),
                 
                 mainPanel(
                   fluidRow(
                     column(6,
                            textOutput('varx')),
                     column(12,
                            verbatimTextOutput('sumx')),
                     column(6,
                            textOutput('vary')),
                     column(12,
                            verbatimTextOutput('sumy')),
                     column(12,
                            tags$h2('Variables sélectionnées par le modèle'))
                   )
                 )
               )
             ),

             tabPanel(tags$h5("Résultats"),
                      fluidPage(
                        navlistPanel(
                          tabPanel('Visualisation graphique'),
                          tabPanel('Télécharger les résultats')
                        )
                      )
             ),
             
  
  tags$footer('Pauline Attal - Pierre Dubrulle - Ibtissam Slalmi', align='center')
  )
)


library(shiny)
library(shinyWidgets)


ui <- fluidPage(
  
  tags$head(
    
    includeCSS("www/style.css"),
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
                   )
                 )
               )
             ),

             tabPanel(tags$h5("Variables"),
                      fluidPage(
                        navlistPanel(
                          tabPanel("Choisir les variables explicatives",
                                   sidebarPanel(
                                     selectInput(
                                       'varcible','Variable à prédire', choices = '', multiple = F, selected = character(0)
                                     ),

                                     selectInput(
                                       'varexpli', 'Variables explicatives', choices = '', multiple = T, selected = character(0)),
                                     
                                   actionButton(
                                     inputId = 'submit_var', label = 'Lancer'
                                   )),
                                     mainPanel(
                                       textOutput('splitsample')
                                     )),
                          tabPanel('Sélection des variables par le modèle',
                                   sidebarPanel(
                                     selectInput(
                                       'varciblesel','Variable à prédire', choices = '', multiple = F, selected = character(0)
                                     ),
                                     actionButton(
                                       inputId = 'input$submit_forward', label = 'Lancer'
                                     )),
                                     mainPanel(
                                       textOutput('forward')
                                   )
                          )
                        )
                      )
             ),
             tabPanel(tags$h5('Fit'),
                      fluidPage(
                        navlistPanel(
                          tabPanel("Cercle de correlation",
                            sidebarPanel(
                              selectizeInput('nb_compx','Nombre de composantes à afficher en X', multiple = F, choices=c(1:10), options=list(maxItems=1)),
                              selectizeInput('nb_compy','Nombre de composantes à afficher en Y', multiple = F, choices=c(1:10), options=list(maxItems=1))
                            ),
                            mainPanel(
                              plotOutput('corr')
                            )
                        ),
                        tabPanel("Plan factoriel",
                                 sidebarPanel(
                                   selectizeInput('nb_facx','Nombre de composantes à afficher en X', multiple = F, choices=c(1:10), options=list(maxItems=1)),
                                   selectizeInput('nb_facy','Nombre de composantes à afficher en Y', multiple = F, choices=c(1:10), options=list(maxItems=1))
                                 ),
                                 mainPanel(
                                   plotOutput('factor')
                                 )
                        ),
                        tabPanel("Matrice de corrélation",
                                 sidebarPanel(
                                   selectizeInput('used_comp', "Used comp à utiliser", multiple=F, choices=c(1:10), options=list(maxItems=1))
                                 ),
                        mainPanel(
                          plotOutput('matrice')
                        )
                        ),
                        tabPanel('Proportion des variances expliquées',
                                  mainPanel('proportion')
                                 )
                          
                        )
                      )
             )
  ),
             
  
  tags$footer('Pauline Attal - Pierre Dubrulle - Ibtissam Slalmi', align='center')
)


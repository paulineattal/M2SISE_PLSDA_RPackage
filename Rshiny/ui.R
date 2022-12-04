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
               
               tags$h5("Visualiser vos données"),
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

             tabPanel(tags$h5("Sélection des variables explicatives"),
                      fluidPage(
                          tabPanel('Sélection des variables par le modèle',
                                   sidebarPanel(
                                     selectInput(
                                       'varciblesel','Variable à prédire', choices = '', multiple = F, selected = character(0)
                                     ),
                                     actionButton(
                                       inputId = 'submit_forward', label = 'Lancer'
                                     )
                                     ),
                                     mainPanel(
                                       textOutput('forward')
                                     )
                          )
                      )
             ),
                          navbarMenu(tags$h5('Sorties graphiques'),
                                     tabPanel('Cercle de corrélation',
                                              sidebarPanel(
                                                selectizeInput('nb_compx_cercle','Nombre de composantes à afficher en X', multiple = F, choices='', options=list(maxItems=1)),
                                                selectizeInput('nb_compy_cercle','Nombre de composantes à afficher en X', multiple = F, choices='', options=list(maxItems=1)),
                                                actionButton('cercle_var', 'Afficher le graphique')
                                              ),
                                              mainPanel(plotOutput('cerclevar'))
                                     ),
                                     tabPanel('Projection des variables',
                                              sidebarPanel(
                                              selectizeInput('nb_compx_proj','Nombre de composantes à afficher en X', multiple = F, choices='', options=list(maxItems=1)),
                                              selectizeInput('nb_compy_proj','Nombre de composantes à afficher en X', multiple = F, choices='', options=list(maxItems=1)),
                                              actionButton('proj_var', 'Afficher le graphique')
                                              ),
                                              mainPanel(plotOutput('projvar'))
                                     ),
                                     tabPanel('Matrice de corrélation',
                                              sidebarPanel(
                                                selectizeInput('used','Choisir le used comp', multiple=F, choices='', options=list(maxItems=1)),
                                                actionButton('matrice', 'Afficher la matrice')
                                              ),
                                              mainPanel(plotOutput('matrice'))
                                     ),
                                     tabPanel('Proportion des variances',
                                              mainPanel(
                                                plotOutput('proportion')
                                              ))
                                              
                                     
                                     
                          )
  ),
             

  tags$footer('Pauline Attal - Pierre Dubrulle - Ibtissam Slalmi', align='center')
)


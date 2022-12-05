library(shiny)
devtools::install_github('paulineattal/PLSDA_R_Package', subdir='/plslda')
library(plslda)
source('ui.R', local = TRUE)
source('server.R')


shinyApp(
  ui = ui,
  server = sever,
  options = list(height = 1080)
)

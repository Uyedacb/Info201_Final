library(shiny)
source("spider-functions.R")
source("main_ui.R")
source("main_server.R")
source("Analysis.R")

shinyApp(ui = main_ui, server = main_server)
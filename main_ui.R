# install.packages("rsconnect")
library(shiny)
library(rsconnect)
main_ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      "Q1",
    ),
    tabPanel(
      "Q2",
    ),
    tabPanel(
      "Q3",
    ),
    tabPanel(
      "Q4",
    )
  )
)
shinyUI(main_ui)

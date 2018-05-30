# install.packages("rsconnect")
library(shiny)
library(rsconnect)
library(plotly)
main_ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      "Q1",
      sidebarLayout(
        sidebarPanel(
          radioButtons('q1_radio', "Select data for last
                       30 days or diagonsed depression",
                       c('Last 30 days',' Diagnosed Depression')),
          sliderInput('q1_slider',
                      "How many days have been affected by mental health",
                      1, 30)
          
        ),
        mainPanel(
          plotOutput('q1_plot')
        )
      )
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

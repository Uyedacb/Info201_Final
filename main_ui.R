# install.packages("rsconnect")
library(shiny)
library(rsconnect)
library(dplyr)
library(radarchart)
source("spider-functions.R")

main_ui <- fluidPage(
  
  tabsetPanel(
    tabPanel(
      "Q1"
    ),
    tabPanel(
      "Q2"
    ),
    tabPanel(
      "Q3"
    ),
    tabPanel(
      "Q4",
      sidebarLayout(
        # side bar containing variable selections
        sidebarPanel(
          h5("First Group"),
          selectInput("q4_ethnicity_one", "Choose an Ethnic background to look at:", q4_ethnic_groups),
          selectInput("q4_age_range_one", "Choose an Age Group to look at:", q4_age_groups),
          h5("Second Group"),
          selectInput("q4_ethnicity_two", "Choose an Ethnic background to look at", q4_ethnic_groups),
          selectInput("q4_age_range_two", "Choose an Age Group to look at:", q4_age_groups)
        ),
        # outputs spider plot and plot description
        mainPanel(
          chartJSRadarOutput("q4_spiderplot"),
          textOutput("q4_descript")
        )
      )
    )
  )
)

shinyUI(main_ui)


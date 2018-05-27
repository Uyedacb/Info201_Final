library(dplyr)
library(shiny)
library(radarchart)
source("spider-functions.R")

my_ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h5("First Group"),
      selectInput("ethnicity_one", "Choose an Ethnic background to look at:", ethnic_groups),
      selectInput("age_range_one", "Choose an Age Group to look at:", age_groups),
      h5("Second Group"),
      selectInput("ethnicity_two", "Choose an Ethnic background to look at", ethnic_groups),
      selectInput("age_range_two", "Choose an Age Group to look at:", age_groups)
    ),
    mainPanel(
      chartJSRadarOutput("spiderplot"),
      textOutput("printTest")
    )
  )
)

shinyUI(my_ui)
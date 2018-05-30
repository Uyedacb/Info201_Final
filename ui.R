library(dplyr)
library(shiny)
library(radarchart)
source("spider-functions.R")

choices_q2 = c("Total Family Income" = "IRFAMIN3", "County Size" = "COUTYP2", 
               "Work Status" = "irwrkstat", "Age" = "CATAG3", "Education" = "IREDUHIGHST2", 
               "Sex" = "irsex")

my_ui <- fluidPage(
  sidebarLayout(
    # side bar containing variable selections
    sidebarPanel(
      h5("First Group"),
      selectInput("ethnicity_one", "Choose an Ethnic background to look at:", ethnic_groups),
      selectInput("age_range_one", "Choose an Age Group to look at:", age_groups),
      h5("Second Group"),
      selectInput("ethnicity_two", "Choose an Ethnic background to look at", ethnic_groups),
      selectInput("age_range_two", "Choose an Age Group to look at:", age_groups),
      radioButtons('size_choice', label = "Choose a variable to plot against the reported suicidal 
                   tendencies.", choices = choices_q2)
    ),
    # outputs spider plot and plot description
    mainPanel(
      chartJSRadarOutput("spiderplot"),
      textOutput("descript"),
      p("This plot shows how suicidal tendencies relate to", textOutput('size_choice', inline = T),
        "over time. The size of the points is dependent on the number of cases which match the criteria
        labeled on the axes. This helps demonstrate how those without suicial tendencies vary across
        different variables from those who are suicidal."),
      plotOutput("plot_suic", click = 'plot_click'),
      p("Number of Cases:", strong(textOutput('clicked', inline = T)))
    )
  )
)

shinyUI(my_ui)
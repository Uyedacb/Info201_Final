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
          p("This radar chart provides users with a visual way of comparing between two different group types.
            These groups can vary by age and ethnicity. The groups are compared by percentages of the total population
            within that group that meet the categories labeled on the corners of the chart.  
            Mousing over a point will show the exact percentage of people, in their respective group, that meet the category. 
            Here is what each category means:"),
          HTML({
            paste("<strong>Plans of Suicide</strong>
                  <li>Indicates what percentage of the selected group has 
                  had Recurrent thoughts of death or recurrent suicide ideation</li>",
                  "<strong>Depression</strong>
                  <li>indicates having five or more symptoms of Major Depressive Episodes</li>",
                  "<strong>Taking Prescription Medicine</strong>
                  <li>is currently taking medicine prescribed to treat depression</li>",
                  "<strong>Recieving Professional Treatment</strong>
                  <li>is currently recieving professional treatment</li>",
                  "<strong>Poverty Level</strong>
                  <li>household income meets the federal level of poverty</li>")
          })
          
        )
      )
    )
  )
)

shinyUI(main_ui)


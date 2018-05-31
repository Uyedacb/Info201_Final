# install.packages("rsconnect")
library(shiny)
library(rsconnect)
library(dplyr)
library(radarchart)
library(ggplot2)
library(plotly)
source("spider-functions.R")
source("Analysis.R")

choices_q2 = c("Total Family Income" = "IRFAMIN3", "County Size" = "COUTYP2", 
               "Work Status" = "irwrkstat", "Age" = "CATAG3", "Education" = "IREDUHIGHST2", 
               "Sex" = "irsex")

library(plotly)
main_ui <- fluidPage(titlePanel("Exploring Depression and Suicide in the United States"),
  
  tabsetPanel(
    tabPanel(
      "Q1",
      sidebarLayout(
        sidebarPanel(
          radioButtons('q1_radio', "Select data for last
                       30 days or diagonsed depression",
                       c('Last 30 days','Diagnosed Depression')),
          sliderInput('q1_slider',
                      "How many days have been affected by mental health",
                      1, 30, 30)
          
        ),
        mainPanel(
          p(''),
          plotlyOutput('q1_plot'),
          p('This map shows the relative densities of people who
            reported that they have been diagnosed with depression
            or the relative densities of people who have reported
            that 1 to 30 of the past 30 days have been affected by 
            poor mental health. The data was collected by randomly
            calling people in the United states, and with 486303 observations,
            we believe that this study has a large enough sample set to 
            deem it accurate. The data was sourced from the 
            Behavioral Risk Factor Surveillance System run by the CDC.
             You can learn more about their work here: 
            https://www.cdc.gov/brfss/annual_data/annual_data.htm 
            '),
          p('To use this map, first select the data set you would like 
            to analyze with the radio button selector (Either "Last 30 days",
            or "Diagnosed Depression". If you chose "Last 30 days" then the slider
            will manipulate the map in such a way that we are only considering people
            who have reported that number of days or less 
            have been affected by mental health')
        )
      )
    ),
    tabPanel(
      "Suicidal Tendencies by Variable",
      sidebarPanel(
        radioButtons('size_choice_q2', label = "Choose a variable to plot against the reported suicidal 
                   tendencies.", choices = choices_q2)
      ),
      mainPanel(
        plotOutput("plot_suic_q2", click = 'plot_click_q2', width = 830, height = 530),
        p("Number of Cases:", strong(textOutput('clicked_q2', inline = T))),
        p("This plot shows how suicidal tendencies relate to", textOutput('key_q2', inline = T),
          "over time. The size and fill of the points is dependent on the number of cases which match the criteria
          labeled on the axes. This helps demonstrate how those without suicial tendencies differ across
          numerous variables relative to those who are suicidal.")
      )
    ),
    tabPanel(
      "Frequencies of Suicidal Tendencies",
      h5("Frequencies"),
      h6("Had thoughts, planned, or attempted suicide in the past 12 months."),
      sidebarLayout( # Create layout with 2 panels
        sidebarPanel( # Specify content for sidebar panel
          # Create widget
          selectInput("select_gender", "Select a Gender:",
                      c(list("Both" = "Both Genders"),
                        "Male" = "Male",
                        "Female" = "Female"))
        ),
        mainPanel( # Specify content for main panel
          tabsetPanel(type = "tabs",
                      tabPanel("Visual", plotOutput("q3_plot"),
                               textOutput("description_1_q3")),
                      tabPanel("Table", tableOutput("q3_table")),
                      tabPanel("Pie Chart", plotOutput("q3_pie"),
                               textOutput("description_2_q3"))
          )
        )
      )
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

library(dplyr)
library(shiny)
library(radarchart)
library(rsconnect)
source("spider-functions.R")

q4_server <- function(input,output) {
  # use subset to subset data yo
  rv <- reactiveValues()
  rv$max_percent <- 0;
  
  spider_data <- reactive({
    age_one <- input$age_range_one
    age_two <- input$age_range_two
    ethnicity <- list(one = input$ethnicity_one, two = input$ethnicity_two)
    if (input$age_range_one == 0) {
      variable_one <- anti_subset_by_age(filtered_nsduh, 0, dp_filter[[2]], ethnicity$one)
    } else {
      variable_one <- subset(
        filtered_nsduh, NEWRACE2 == ethnicity$one & CATAG3 == age_one,
        select = c(dp_filter[[age_one]], "POVERTY3")
      )
    }
    if (input$age_range_two == 0) {
      variable_two <- anti_subset_by_age(filtered_nsduh, 0, dp_filter[[2]], ethnicity$two)
    } else {
      variable_two <- subset(
        filtered_nsduh, NEWRACE2 == ethnicity$two & CATAG3 == age_two,
        select = c(dp_filter[[age_two]], "POVERTY3")
      )
    }
    variable_one <- percentage_filter(variable_one, health_filter$one)
    variable_two <- percentage_filter(variable_two, health_filter$two)
    rv$max_percent <- round(max(variable_one,variable_two))
    return(list(variable_one,variable_two))
  })
  
  percentage_filter <- function(variable_data, variable_names) {
    percentage_list <- list()
    sum_data <- colSums(variable_data == 1, na.rm = TRUE)
    percentage_list[c(variable_names, "POVERTY3")] <- round((sum_data / nrow(variable_data)) * 100, 2)
  }
  
  anti_subset_by_age <- function(variable_data, age_num, extra_filter, ethnicity) {
    if (ethnicity == 0) {
      variable_data <- subset(
        filtered_nsduh, CATAG3 != !!age_num,
        select = c(extra_filter, "POVERTY3")
      )
    } else {
      variable_data <- subset(
        filtered_nsduh, NEWRACE2 == ethnicity & CATAG3 != !!age_num,
        select = c(extra_filter, "POVERTY3")
      )
    }
    return(variable_data)
  }
  output$spiderplot <- renderChartJSRadar({
    comparison_data <- spider_data()
    data_one <- comparison_data[[1]]
    data_two <- comparison_data[[2]]
    df <- data.frame("label" = c(
      "Plans of Suicide", "Depression",  "Taking Prescription Medicine", 
      "Recieving Professional Treatment", "Poverty Level"
      ), "First Variable" = data_one, "Second Variable" = data_two)
    chartJSRadar(
      scores = df, maxScale = round(rv$max_percent, -1), showToolTipLabel = TRUE,
      labelSize = 12, main = "Comparing Two People Groups"
      ) 
  })
  
  output$printTest <- renderText(
    "placeholder"
  )
}


shinyServer(q4_server)

library(plyr); library(dplyr)
library(shiny)
library(radarchart)
library(rsconnect)
source("spider-functions.R")

q4_server <- function(input,output) {
  # reactive data frame for finding the max percent
  # between both variables
  rv <- reactiveValues()
  rv$max_percent <- 0;
  
  # grabs user input and outputs the respective data
  # for variable one and two
  spider_data <- reactive({
    age_one <- as.numeric(input$age_range_one)
    age_two <- as.numeric(input$age_range_two)
    ethnicity_one <- as.numeric(input$ethnicity_one)
    ethnicity_two <- as.numeric(input$ethnicity_two)
    age_dp_one <- as.numeric(input$age_range_one)
    age_dp_two <- as.numeric(input$age_range_two)
    if (input$ethnicity_one == 0) {
      ethnicity_one <- seq(1,6)
    }
    if (input$ethnicity_two == 0) {
      ethnicity_two <- seq(1,6)
    }
    if (input$age_range_one == 6) {
      age_one <- seq(2,5)
    }
    if (input$age_range_two == 6) {
      age_two <- seq(2,5)
    }
    variable_one <- subset(
      filtered_nsduh, NEWRACE2 %in% ethnicity_one & CATAG3 %in% age_one,
      select = c(dp_filter[[age_dp_one]], "POVERTY3")
    )
    variable_two <- subset(
      filtered_nsduh, NEWRACE2 %in% ethnicity_two & CATAG3 %in% age_two,
      select = c(dp_filter[[age_dp_two]], "POVERTY3")
    )
    variable_one <- unname(percentage_filter(variable_one, 1))
    variable_two <- unname(percentage_filter(variable_two, 1))
    rv$max_percent <- max(variable_one, variable_two)
    return(list(variable_one, variable_two))
  })
  
  # grabs data and a variable to find the sum of within each respective column.
  # returns a list of percentages over total observations for each respective column.
  # (to two decimal places)
  percentage_filter <- function(variable_data, variable_filter) {
    percentage_list <- list()
    col_names <- colnames(variable_data)
    sum_data <- colSums(variable_data == variable_filter, na.rm = TRUE)
    percentage_list[col_names] <- round((sum_data / nrow(variable_data)) * 100, 2)
  }
  
  # outputs radar chart to render in UI
  output$spiderplot <- renderChartJSRadar({
    data <- spider_data()
    data_one <- data[[1]]
    data_two <- data[[2]]
    labs = c(
      "Plans of Suicide", "Depression",  "Taking Prescription Medicine", 
      "Recieving Professional Treatment", "Poverty Level"
    )
    df <- list("First Group" = data_one, "Second Group" = data_two)
    chartJSRadar(
      scores = df, labs = labs, maxScale = round_any(rv$max_percent, 5, f = ceiling), scaleStepWidth = 5,
      showToolTipLabel = TRUE, labelSize = 12, main = "Comparing Two People Groups"
      ) 
  })
  
  # placeholder text, will put description of graph in here at the last stage of development
  output$descript <- renderText({
    paste(input$ethnicity_one, input$ethnicity_two, input$age_range_one, input$age_range_two, rv$max_percent)
  })
}

shinyServer(q4_server)

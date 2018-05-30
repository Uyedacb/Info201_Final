library(plyr); library(dplyr)
library(shiny)
library(radarchart)
library(rsconnect)
source("spider-functions.R")

main_server <- function(input, output) {
  # reactive data frame for finding the max percent
  # between both variables
  q4_rv <- reactiveValues()
  q4_rv$max_percent <- 0;
  
  # grabs user input and outputs the respective data
  # for variable one and two
  q4_spider_data <- reactive({
    q4_age_one <- as.numeric(input$q4_age_range_one)
    q4_age_two <- as.numeric(input$q4_age_range_two)
    q4_ethnicity_one <- as.numeric(input$q4_ethnicity_one)
    q4_ethnicity_two <- as.numeric(input$q4_ethnicity_two)
    if (q4_ethnicity_one == 0) {
      q4_ethnicity_one <- seq(1,6)
    }
    if (q4_ethnicity_two == 0) {
      q4_ethnicity_two <- seq(1,6)
    }
    if (q4_age_one == 6) {
      q4_variable_one <- q4_all_ages_data(q4_ethnicity_one)
    } else {
      q4_variable_one <- subset(
        q4_filtered_nsduh, NEWRACE2 %in% q4_ethnicity_one & CATAG3 %in% q4_age_one,
        select = c(q4_dp_filter[[q4_age_one]], "POVERTY3")
      )
    }
    if (q4_age_two == 6) {
      q4_variable_two <- q4_all_ages_data(q4_ethnicity_two)
    } else {    
      q4_variable_two <- subset(
        q4_filtered_nsduh, NEWRACE2 %in% q4_ethnicity_two & CATAG3 %in% q4_age_two,
        select = c(q4_dp_filter[[q4_age_two]], "POVERTY3")
      )
    }
    q4_variable_one <- unname(q4_percentage_filter(q4_variable_one, 1))
    q4_variable_two <- unname(q4_percentage_filter(q4_variable_two, 1))
    q4_rv$max_percent <- max(q4_variable_one, q4_variable_two)
    return(list(q4_variable_one, q4_variable_two))
  })
  
  # crude way of getting data of all ages into just 5 columns
  # to return a data frame that can be calculated properly in
  # percentage_filter()
  q4_all_ages_data <- function(q4_ethnicity) {
    q4_age_cat <- seq(1,5)
    q4_variable_data <- subset(
      q4_filtered_nsduh, NEWRACE2 %in% q4_ethnicity & CATAG3 %in% q4_age_cat,
      select = c(q4_dp_filter[[1]], q4_dp_filter[[2]], "POVERTY3")
    )
    q4_existing <- match(q4_dp_filter[[1]], names(q4_variable_data))
    q4_newNames <- q4_dp_filter[[2]]
    names(q4_variable_data)[na.omit(q4_existing)] <- q4_newNames[which(!is.na(q4_existing))]
    q4_variable_data <- rbind(q4_variable_data[c(seq(1,4), 9)], q4_variable_data[c(seq(5,8), 9)])
  }
  
  # grabs data and a variable to find the sum of within each respective column.
  # returns a list of percentages over total observations for each respective column.
  # (to two decimal places)
  q4_percentage_filter <- function(q4_variable_data, q4_variable_filter) {
    q4_percentage_list <- list()
    q4_col_names <- colnames(q4_variable_data)
    q4_sum_data <- colSums(q4_variable_data == q4_variable_filter, na.rm = TRUE)
    q4_percentage_list[q4_col_names] <- round((q4_sum_data / nrow(q4_variable_data)) * 100, 2)
  }
  
  # outputs radar chart to render in UI
  output$q4_spiderplot <- renderChartJSRadar({
    q4_data <- q4_spider_data()
    q4_data_one <- q4_data[[1]]
    q4_data_two <- q4_data[[2]]
    q4_labs = c(
      "Plans of Suicide", "Depression",  "Taking Prescription Medicine", 
      "Recieving Professional Treatment", "Poverty Level"
    )
    q4_df <- list("First Group" = q4_data_one, "Second Group" = q4_data_two)
    chartJSRadar(
      scores = q4_df, labs = q4_labs, maxScale = round_any(q4_rv$max_percent, 5, f = ceiling), scaleStepWidth = 5,
      showToolTipLabel = TRUE, labelSize = 12, main = "Comparing Two People Groups"
    ) 
  })
  
  # placeholder text, will put description of graph in here at the last stage of development
  output$q4descript <- renderText({
    paste(input$q4_ethnicity_one, input$q4_ethnicity_two, input$q4_age_range_one, input$q4_age_range_two, q4_rv$max_percent)
  })
}


shinyServer(main_server)

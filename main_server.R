library(plyr);
library(dplyr)
library(shiny)
library(radarchart)
library(rsconnect)
library(ggplot2)
source("spider-functions.R")
source("question1.R")
data_q2 <- read.csv("Filtered2.csv")

main_server <- function(input, output) {
  ##Q1##
  q1_rv <- reactiveValues()
  observe({q1_rv$radio <- input$q1_radio})
  observe({q1_rv$slider <- input$q1_slider})
  output$q1_plot <- q1_make_map(q1_rv$slider, (q1_rv$radio == 'Diagnosed Depression'))
  
  ##End Q1##
  
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
  
##### Q2 #####
  
  # reactive values list
  val_q2 <- reactiveValues()
  
  # sets the clicked_q2 reactive value to a "" default
  val_q2$clicked_q2 <- ""
  
  # sets the key for the size_choice_q2 value to "" default
  val_q2$key_q2 <- ""
  
  # sets the size_choice_q2 variable to the size_choice_q2 input variable for rendering text output
  output$size_choice_q2 <- renderText({
    return(input$size_choice_q2)
  })
  
  # sets the size_choice2_q2 variable to the size_choice_q2 input variable for rendering text output
  output$size_choice2_q2 <- renderText({
    return(input$size_choice_q2)
  })
  
  # filters the data appropriately according to user input
  filtered_suic_q2 <- reactive({
    
    data_q2 <- data_q2 %>% filter(suicthnk == 1| suicthnk == 2)
    data_q2[colnames(data_q2)] <- lapply(data_q2[colnames(data_q2)], as.factor)
    suicthnk_data_q2 <- data_q2 %>% group_by_("suicthnk", input$size_choice_q2) %>% 
      tally()
    
    return(suicthnk_data_q2)
    
  })
  
  # renders a plot based on user input
  output$plot_suic_q2 <- renderPlot({
    
    if(input$size_choice_q2 == "IRFAMIN3") {
      y_label_q2 = "Total Family Income"
      y_ticks_q2 = c("<$10,000", "$10,000-$19,999", "$20,000-$29,999", "$30,000-$39,999", "$40,000-$49,999",
                  "$50,000-$74,999", ">$75,000")
    } else if (input$size_choice_q2 == "COUTYP2") {
      y_label_q2 = "Home County Size"
      y_ticks_q2 = c("Large Metro", "Small Metro", "Nonmetro")
    } else if (input$size_choice_q2 == "irsex") {
      y_label_q2 = "Sex"
      y_ticks_q2 = c("Male", "Female")
    } else if(input$size_choice_q2 == "IREDUHIGHST2") {
      y_label_q2 = "Education Level"
      y_ticks_q2 = c("Fifth Grade or less", "Sixth Grade", "Seventh Grade", "Eighth Grade", "Ninth Grade",
                  "Tenth Grade", "Eleventh or Twelfth Grade (no diploma)", "High School Diploma",
                  "Some college credit", "Associate's Degree", "College Graduate")
    } else if(input$size_choice_q2 == "irwrkstat") {
      y_label_q2 = "Work Status"
      y_ticks_q2 = c("Employed Full Time","Employed Part Time", "Unemployed", 
                  "Other (incl. not in work force)")
    } else {
      y_label_q2 = "Age"
      y_ticks_q2 = c("12-17 Years Old", "18-25 Years Old", "26-34 Years Old", "35-49 Years Old",
                  "50 or Older")
    }
    
    val_q2$key_q2 <- y_label_q2
    
    p_q2 <- ggplot(filtered_suic_q2(), aes_string("suicthnk", input$size_choice_q2)) +
      geom_point(aes(size = n, fill = n), shape = 21, color = "black") +
      scale_size_area(max_size = 23) +
      scale_fill_continuous() +
      labs(
        x = "Suicidal Tendencies",
        y = y_label_q2,
        size = "Number of Cases",
        fill = "",
        title = paste("Suicidal Tendencies versus", val_q2$key_q2)
      ) +
      scale_x_discrete(labels = c("Yes", "No")) +
      scale_y_discrete(labels = y_ticks_q2) +
      theme(text = element_text(size = 15))
    
    return(p_q2)
  })
  
  # when a point is clicked, this return the number of cases
  observeEvent(input$plot_click_q2, {
    
    clicked_suic_q2 <- nearPoints(filtered_suic_q2(), input$plot_click_q2)
    
    val_q2$clicked_q2 <- unique(clicked_suic_q2$n)
    
  })
  
  # renders the number of cases value for the when-clicked output
  output$clicked_q2 <- renderText({
    return(as.character(val_q2$clicked_q2))
  })
  
  # renders the key for the size_choice_q2 variable
  output$key_q2 <- renderText({
    return(val_q2$key_q2)
  })
  
### Q3 ###
  reactive_plot_q3 <- reactive({
    if (input$select_gender != "Male" | input$select_gender != "Female") {
      plot_both_q3 <- ggplot(data = behaviors_frequencies_q3, aes(x = Behavior)) +
        geom_bar(stat = "count", fill = "steelblue") +
        ggtitle("Frequencies of Behaviors in the Past 12 Months") +
        labs(x = "Behavior", y = "Number of People") +
        stat_count(aes(label = ..count..), vjust = 1, geom = "text", 
                   position = "identity", color = "white")
    }
    if (input$select_gender == "Male" | input$select_gender == "Female") {
      filtered_freq_q3 <- filter(behaviors_frequencies_q3, Gender == input$select_gender)
      plot_both <- ggplot(data = filtered_freq, aes(x = Behavior)) +
        geom_bar(stat = "count", fill = "steelblue") +
        ggtitle("Frequencies of Behaviors in the Past 12 Months") +
        labs(x = "Behavior", y = "Number of People") +
        stat_count(aes(label = ..count..), vjust = 1, geom = "text", 
                   position = "identity", color = "white")
    }
    return(plot_both_q3)
  })
  output$q3_plot <- renderPlot(
    reactive_plot_q3()
  )
  reactive_table_q3 <- reactive({
    if (input$select_gender != "Male" | input$select_gender != "Female") {
      filtered_table_q3 <- genders_comb_q3
    }
    if (input$select_gender == "Male" | input$select_gender == "Female") {
      filtered_table <- filter(genders_comb, Gender == input$select_gender)
    }
    return(filtered_table_q3)
  })
  output$q3_table <- renderTable(
    reactive_table_q3()
  )
  reactive_pie_q3 <- reactive({
    if (input$select_gender != "Male" | input$select_gender != "Female") {
      filtered_pie_all_q3 <- ggplot(behaviors_frequencies_q3, aes(x = factor(1), fill = factor(Behavior))) +
        geom_bar(width = 1)
      filtered_pie_q3 <- filtered_pie_all_q3 + coord_polar(theta = "y") +
        ggtitle("Frequencies of Behaviors in the Past 12 Months") +
        labs(x = "Behavior", y = "Number of People")
    }
    if (input$select_gender == "Male" | input$select_gender == "Female") {
      q3_filtered_beh_freq <- filter(behaviors_frequencies_q3, Gender == input$select_gender)
      filtered_pie_freq <- ggplot(q3_filtered_beh_freq, aes(x = factor(1), fill = factor(Behavior))) +
        geom_bar(width = 1)
      filtered_pie_q3 <- filtered_pie_freq + coord_polar(theta = "y") +
        ggtitle("Frequencies of Behaviors in the Past 12 Months") +
        labs(x = "Behavior", y = "Number of People")
    }
    return(filtered_pie_q3)
  })
  output$q3_pie <- renderPlot(
    reactive_pie_q3()
  )
  output$description_1_q3 <- renderText({
    paste0("This bar chart shows the frequencies of behaviors for ", input$select_gender, ".",
           " There were a total of ", total_responses_q3, " responses and each responder were able to select more than one choice.",
           "\nThere were a total of ", total_males_q3, " males and a total of ", total_females_q3, " females.",
           "\nBoth males and females had over half of its respondents select that they have seriously thought of suicide.",
           "\nBased off of both charts, we know that more than half of both males and females have seriously thought of suicide while less than half of both males and females have attempted suicide.")
  })
  output$description_2_q3 <- renderText({
    paste0("This pie chart shows the proportions of behaviors for ", input$select_gender, ".",
           " This information is consistent with the bar chart and table. ",
           "\nThere were a total of ", total_responses_q3, " responses and each responder were able to select more than one choice.",
           "\nThere were a total of ", total_males_q3, " males and a total of", total_females_q3, ".", "females.",
           "\nThis chart allows us to see that at least two-thirds of both males and females have seriously thought of suicide.",
           "\nThe responses for females are less disproportioned than males, given that nearly one-fourth of females have planned suicide.")
  })
}


shinyServer(main_server)

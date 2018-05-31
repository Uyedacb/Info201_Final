# Load libraries 
library(dplyr)
library("ggplot2")
library(shiny)

# Source
source("./Analysis.R")

# Define UI
q3_ui <- fluidPage(
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
                  verbatimTextOutput("description_1_q3")),
                  tabPanel("Table", tableOutput("q3_table")),
                  tabPanel("Pie Chart", plotOutput("q3_pie"),
                  verbatimTextOutput("description_2_q3"))
    )
  )
  )
  )

# Define server
q3_server <- function(input, output) {
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
      q3_filtered_pie_freq <- ggplot(q3_filtered_beh_freq, aes(x = factor(1), fill = factor(Behavior))) +
        geom_bar(width = 1)
      filtered_pie_q3 <- q3_filtered_pie_freq + coord_polar(theta = "y") +
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

# Combine UI and Server
shinyApp(ui = q3_ui, server = q3_server)
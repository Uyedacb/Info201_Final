# Load libraries 
library(dplyr)
library("ggplot2")
library(shiny)

# Source
source("./Analysis.R")

# Define UI
my_ui <- fluidPage(
  h5("Frequencies"),
  h6("Had thoughts, planned, or attempted suicide in the past 12 months."),
  sidebarLayout( # Create layout with 2 panels
    sidebarPanel( # Specify content for sidebar panel
      # Create widget
      selectInput("select_gender", "Select a Gender:",
                  c(list("Both" = "Both"),
                    "Male" = "Male",
                    "Female" = "Female"))
    ),
    mainPanel( # Specify content for main panel
      tabsetPanel(type = "tabs",
                  tabPanel("Visual", plotOutput("q3_plot")),
                  tabPanel("Table", tableOutput("q3_table")),
                  tabPanel("Pie Chart", plotOutput("q3_pie"))
    )
  )
  )
  )

# Define server
q3_server <- function(input, output) {
  reactive_plot <- reactive({
    if (input$select_gender != "Male" | input$select_gender != "Female") {
    plot_both <- ggplot(data = behaviors_frequencies, aes(x = Behavior)) +
      geom_bar(stat = "count", fill = "steelblue") +
      ggtitle("Frequencies of Behaviors in the Past 12 Months") +
      labs(x = "Behavior", y = "Number of People") +
      stat_count(aes(label = ..count..), vjust = 1, geom = "text", 
                 position = "identity", color = "white")
    }
    if (input$select_gender == "Male" | input$select_gender == "Female") {
    filtered_freq <- filter(behaviors_frequencies, Gender == input$select_gender)
    plot_both <- ggplot(data = filtered_freq, aes(x = Behavior)) +
      geom_bar(stat = "count", fill = "steelblue") +
      ggtitle("Frequencies of Behaviors in the Past 12 Months") +
      labs(x = "Behavior", y = "Number of People") +
      stat_count(aes(label = ..count..), vjust = 1, geom = "text", 
                 position = "identity", color = "white")
    }
  return(plot_both)
  })
  output$q3_plot <- renderPlot(
    reactive_plot()
  )
  reactive_table <- reactive({
    if (input$select_gender != "Male" | input$select_gender != "Female") {
    filtered_table <- genders_comb
    }
    if (input$select_gender == "Male" | input$select_gender == "Female") {
    filtered_table <- filter(genders_comb, Gender == input$select_gender)
    }
    return(filtered_table)
  })
  output$q3_table <- renderTable(
    reactive_table()
  )
  reactive_pie <- reactive({
    if (input$select_gender != "Male" | input$select_gender != "Female") {
      filtered_pie_all <- ggplot(behaviors_frequencies, aes(x = factor(1), fill = factor(Behavior))) +
        geom_bar(width = 1)
      filtered_pie <- filtered_pie_all + coord_polar(theta = "y")
    }
    if (input$select_gender == "Male" | input$select_gender == "Female") {
      filtered_beh_freq <- filter(behaviors_frequencies, Gender == input$select_gender)
      filtered_pie_freq <- ggplot(filtered_beh_freq, aes(x = factor(1), fill = factor(Behavior))) +
        geom_bar(width = 1)
      filtered_pie <- filtered_pie_freq + coord_polar(theta = "y")
    }
    return(filtered_pie)
  })
  output$q3_pie <- renderPlot(
    reactive_pie()
  )
}

# Combine UI and Server
shinyApp(ui = my_ui, server = q3_server)
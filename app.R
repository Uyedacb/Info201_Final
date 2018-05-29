library(shiny)
library(ggplot2)
library(dplyr)
library(shiny)
library(tidyr)

data <- read.csv("./Filtered2.csv", stringsAsFactors = F)

ui <- fluidPage(
  titlePanel("Suicidal Tendencies by Variable"),
  sidebarLayout(
    sidebarPanel(
      radioButtons('size_choice', label = "Choose a variable to plot against the reported suicidal 
                   tendencies.", 
                   choices = c("IRFAMIN3", "COUTYP2","PDEN10", "irwrkstat", "CATAG3", "IREDUHIGHST2", 
                               "irsex")
      )
    ),
    mainPanel(
      p("This plot shows how suicidal tendencies relate to", textOutput('size_choice', inline = T),
        "over time. The size of the points is dependent on the number of cases which match the criteria
        labeled on the axes."),
      plotOutput("plot", click = 'plot_click'),
      p(textOutput('size_choice2', inline = T), ":", strong(textOutput('clicked', inline = T)))
    )
  )
)

server <- function(input, output, session) {
  
  val <- reactiveValues()
  val$clicked <- ""
  
  output$size_choice <- renderText({
    return(input$size_choice)
  })
  
  output$size_choice2 <- renderText({
    return(input$size_choice)
  })
  
  filtered <- reactive({
    
    data <- data %>% filter(suicthnk == 1| suicthnk == 2)
    data[colnames(data)] <- lapply(data[colnames(data)], as.factor)
    suicthnk_data <- data %>% group_by_("suicthnk", input$size_choice) %>% 
      tally()
    
    return(suicthnk_data)
    
  })
  
  output$plot <- renderPlot({
    
    if(input$size_choice == "IRFAMIN3") {
      y_label = "Total Family Income"
    } else if (input$size_choice == "COUTYP2") {
      y_label = "Home County Size"
    } else if (input$size_choice == "irsex") {
      y_label = "Gender"
    } else if (input$size_choice == "PDEN10") {
      y_label = "Home Population Density"
    } else if(input$size_choice == "IREDUHIGHST2") {
      y_label = "Education Level"
    } else if(input$size_choice == "irwrkstat") {
      y_label = "Work Status"
    } else {
      y_label = "Age"
    }
    
    p <- ggplot(filtered(), aes_string("suicthnk", input$size_choice)) +
      geom_point(aes(size = n, fill = n), shape = 21, color = "black") +
      scale_size_area(max_size = 23) +
      scale_fill_continuous() +
      labs(
        x = "Suicidal Tendencies",
        y = y_label
      )
    
    return(p)
  })
    
    observeEvent(input$plot_click, {
      
      clicked <- nearPoints(filtered(), input$plot_click)
      
      val$clicked <- unique(clicked$n)
      
    })
    
    output$clicked <- renderText({
      return(as.character(val$clicked))
    })
}

shinyApp(ui, server)
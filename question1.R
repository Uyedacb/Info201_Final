library(dplyr)
library(shiny)
library(ggplot2)

q1_health_data <- read.csv('./stripped_data.csv', stringsAsFactors = F)
q1_health_data$X_STATE <- tolower(q1_health_data$X_STATE)

q1_state_data <- map_data('state')

q1_make_map <- function(days, depression_dataset ){
  
  mental_health_data <- q1_health_data %>% 
    select(X_STATE, MENTHLTH)
  total_mental_data <- mental_health_data %>%
    group_by(X_STATE) %>% 
    tally()
  last30_data <- mental_health_data %>% 
    filter(MENTHLTH <= 30) %>%
    group_by(X_STATE) %>% 
    tally()
  
  depression_data <- q1_health_data %>%
    select(X_STATE, ADDEPEV2)
  depressed_data <- depression_data %>%
    filter(ADDEPEV2 == 1) %>%
    group_by(X_STATE) %>% 
    tally()
  
  summarized_data <- full_join(total_mental_data,
                               last30_data,
                               by = "X_STATE")
  summarized_data <- summarized_data %>% mutate(total = n.x, recent = n.y)
  summarized_data <- full_join(summarized_data,
                               depressed_data,
                               by= "X_STATE")
  summarized_data <- summarized_data %>% mutate(depressed = n)
  summarized_data <- summarized_data %>%
    mutate(pct_depressed = depressed / total * 100,
           pct_recent = recent / total * 100,
           region = X_STATE)
  
  
  
  
  gg <- ggplot()
  gg <- gg + geom_map(data=q1_state_data, map=q1_state_data,
                      aes(x=long, y=lat, map_id=region),
                      fill="#ffffff", color="#ffffff", size=0.15)
  if(depression_dataset){
    gg <- gg + geom_map(data=summarized_data, map=q1_state_data,
                        aes(fill=pct_depressed, map_id=region),
                        color="#ffffff", size=0.15)
  }
  else{
    gg <- gg + geom_map(data=summarized_data, map=q1_state_data,
                        aes(fill=pct_recent, map_id=region),
                        color="#ffffff", size=0.15)
  }
  gg
}
q1_make_map(30, T)



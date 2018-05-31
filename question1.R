library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)
#library(maps)
library(tools)
#library(countrycode)
#install.packages("countrycode")

q1_health_data <- read.csv('./stripped_data.csv', stringsAsFactors = F)
q1_census_data <- read.csv('./census.csv', stringsAsFactors = F)

q1_state_data <- map_data('state')

q1_make_map <- function(days, depression_dataset){
  
  mental_health_data <- q1_health_data %>% 
    select(X_STATE, MENTHLTH)
  total_mental_data <- mental_health_data %>%
    group_by(X_STATE) %>% 
    tally()
  last30_data <- mental_health_data %>% 
    filter(MENTHLTH <= days) %>%
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
  summarized_data <- summarized_data %>% select(X_STATE, total, recent, depressed)
  
  q1_census_data <- q1_census_data %>% mutate(X_STATE = State)
  q1_census_data <- q1_census_data %>% select(X_STATE, X2018.Population)
  
  
  summarized_data <-left_join(summarized_data,
                            q1_census_data,
                            by= "X_STATE")
  
  summarized_data <- summarized_data %>%
    mutate(pct_depressed = depressed / total * 100,
           pct_recent = recent / total * 100,
           region = X_STATE,
           code = state.abb[match(region, state.name)],
           ppl_depressed = round(pct_depressed / 100 * X2018.Population),
           ppl_recent = round(pct_recent / 100 * X2018.Population))
  
  
  
  #gg <- ggplot()
  #gg <- gg + geom_map(data=q1_state_data, map=q1_state_data,
  #                    aes(x=long, y=lat, map_id=region),
  #                    fill="#ffffff", color="#ffffff", size=0.15)
  #if(depression_dataset){
  #  gg <- gg + geom_map(data=summarized_data, map=q1_state_data,
  #                      aes(fill=pct_depressed, map_id=region),
  #                      color="#ffffff", size=0.15)
  #}
  #else{
  #  gg <- gg + geom_map(data=summarized_data, map=q1_state_data,
  #                      aes(fill=pct_recent, map_id=region),
  #                      color="#ffffff", size=0.15)
  #}
  #gg
  
  # specify some map projection/options
  geo_opts <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = F,
    lakecolor = toRGB('white')
  )
  
 
  
  if (depression_dataset){
    summarized_data <- summarized_data %>%
      mutate(hover = paste0('Percent reported depressed: ',
                            as.character(round(pct_depressed)),
                            '%<br>Estimated people diagnosed: ',
                            as.character(ppl_depressed)))
    p <- plot_geo(summarized_data, locationmode = 'USA-states')
    p <- p %>% add_trace(
      z = ~pct_depressed, text = ~hover, locations = ~code,
      color = ~pct_depressed, colors = 'Purples'
    ) %>% colorbar(title = "Percentage<br>reported",
                   limits = c(0,25))
  }
  else{
    summarized_data <- summarized_data %>%
      mutate(hover = paste0('Percent reported poor mental health: ',
                            as.character(round(pct_recent)),
                            '%<br>Estimated people affected: ',
                            as.character(ppl_depressed)))
    
    p <- plot_geo(summarized_data, locationmode = 'USA-states')
    p <- p %>% add_trace(
      z = ~pct_recent, text = ~hover, locations = ~code,
      color = ~pct_recent, colors = 'Purples'
    ) %>% colorbar(title = "Percentage<br>reported",
                   limits = c(0,40))
  }

  p <- p %>%
    layout(
      title = '2016 Depression statistics<br>(Hover for breakdown)',
      geo = geo_opts
    )
    
  p
}
#q1_make_map(30,F)
#q1_make_map(1,F)
#q1_make_map(1,T)


# Contains functions for filtering data for the spider plot comparision applet
# install.packages("dyplr")
# install.packages("ggplot2")
# install.packages("shiny")
# install.packages("rsconnect")

library("dplyr")
library("shiny")
library("rsconnect")

q4_filtered_nsduh <- read.csv("Q4_data/Q4_comparision.csv")
q4_ethnic_groups <- list(
  "White" = 1, "African-American" = 2, 
  "Hispanic or Latino" = 7,
  "American Indian & Alaska Native" = 3, 
  "Asian" = 5, "Pacific Islander" = 4,
  "Multi-ethnic" = 6,
  "All Ethnicities" = 0)
q4_age_groups <- list(
  "12-17 Years Old" = 1, 
  "18-25 Years Old" = 2,
  "26-34 Years Old" = 3,
  "35-49 Years Old" = 4,
  "50 or Older" = 5,
  "All Ages" = 6
)
q4_dp_filter <- list(c("YO_MDEA9", "yodsmmde", "yorxnow", "yotmtnow"), c("AD_MDEA9", "adsmmdea","adrxnow", "adtmtnow"),
                  c("AD_MDEA9", "adsmmdea","adrxnow", "adtmtnow"), c("AD_MDEA9", "adsmmdea","adrxnow", "adtmtnow"),
                  c("AD_MDEA9", "adsmmdea","adrxnow", "adtmtnow"), c("AD_MDEA9", "adsmmdea","adrxnow", "adtmtnow"))

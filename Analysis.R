# Load libraries 
library(dplyr)
library(shiny)
library("ggplot2")
library(reshape2)
library(tibble)

# Load filtered data
filtered_data_q3 <- read.csv("Filtered2.csv")
# View(filtered_data)

# Select necessary columns
q3_filter <- select(filtered_data_q3, X, QUESTID2, filedate, irsex, suicthnk, suicplan, suictry)

# Filter behaviors
both_genders_q3 <- paste("\"Male\" | Gender == \"Female\"")
selected_q3 <- select(q3_filter, irsex, suicthnk, suicplan, suictry)


# Create new data frame for frequency counts for bar graph
selected_q3$irsex[selected_q3$irsex == 1] <- "Male"
selected_q3$irsex[selected_q3$irsex == 2] <- "Female"
selected_q3$suicthnk[selected_q3$suicthnk == 1] <- "Seriously thought of"
selected_q3$suicplan[selected_q3$suicplan == 1] <- "Planned"
selected_q3$suictry[selected_q3$suictry == 1] <- "Attempted"
selected_stacked_q3 <- melt(selected_q3, id.vars=1)
yes_only_q3 <- c("Seriously thought of", "Planned", "Attempted")
selected_stacked_2_q3 <- filter(selected_stacked_q3, value == yes_only_q3)
behaviors_frequencies_q3 <- select(selected_stacked_2_q3, irsex, value)
colnames(behaviors_frequencies_q3) <- c("Gender", "Behavior")


# Create new data frame for frequency table
male_filter_q3 <- filter(behaviors_frequencies_q3, Gender == "Male")
male_suicthnk_q3 <- nrow(filter(male_filter_q3, Behavior == "Seriously thought of")) 
# 329 males seriously thought
male_suicplan_q3 <- nrow(filter(male_filter_q3, Behavior == "Planned")) # 109 males planned
male_suictry_q3 <- nrow(filter(male_filter_q3, Behavior == "Attempted")) # 31 males attempted
female_filter_q3 <- filter(behaviors_frequencies_q3, Gender == "Female")
female_suicthnk_q3 <- nrow(filter(female_filter_q3, Behavior == "Seriously thought of"))
# 442 females seriously thought
female_suicplan_q3 <- nrow(filter(female_filter_q3, Behavior == "Planned")) # 153 females planned
female_suictry_q3 <- nrow(filter(female_filter_q3, Behavior == "Attempted")) # 92 females attempted
male_comb_q3 <- c(male_suicthnk_q3, male_suicplan_q3, male_suictry_q3)
female_comb_q3 <- c(female_suicthnk_q3, female_suicplan_q3, female_suictry_q3)
suicthnk_q3 <- nrow(filter(behaviors_frequencies_q3, Behavior == "Seriously thought of"))
suicplan_q3 <- nrow(filter(behaviors_frequencies_q3, Behavior == "Planned"))
suictry_q3 <- nrow(filter(behaviors_frequencies_q3, Behavior == "Attempted"))
both_comb_q3 <- c(suicthnk_q3, suicplan_q3, suictry_q3)
genders_comb_q3 <- rbind(male_comb_q3, female_comb_q3, both_comb_q3)
genders_comb_q3 <- as.data.frame(genders_comb_q3)
colnames(genders_comb_q3) <- c("Seriously thought", "Planned", "Attempted")
row.names(genders_comb_q3) <- c("Male", "Female", "Both")
genders_comb_q3 <- rownames_to_column(genders_comb_q3, var="Gender")

# Filter behaviors
both_genders_q3 <- paste("\"Male\" | Gender == \"Female\"")
selected_q3 <- select(q3_filter, irsex, suicthnk, suicplan, suictry)
total_responses_q3 <- nrow(behaviors_frequencies_q3)
total_males_q3 <- nrow(filter(behaviors_frequencies_q3, Gender == "Male")) #469
total_females_q3 <- nrow(filter(behaviors_frequencies_q3, Gender == "Female")) # 687
total_responses_q3 <- nrow(behaviors_frequencies_q3)
total_males_q3 <- nrow(filter(behaviors_frequencies_q3, Gender == "Male")) #469
total_females_q3 <- nrow(filter(behaviors_frequencies_q3, Gender == "Female")) # 687
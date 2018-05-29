# Load libraries 
library(dplyr)
library(shiny)
library("ggplot2")
library(reshape2)
library(tibble)

# Load filtered data
filtered_data <- read.csv("Filtered2.csv")
# View(filtered_data)

# Select necessary columns
q3_filter <- select(filtered_data, X, QUESTID2, filedate, irsex, suicthnk, suicplan, suictry)

# Filter behaviors
both_genders <- paste("\"Male\" | Gender == \"Female\"")
selected <- select(q3_filter, irsex, suicthnk, suicplan, suictry)

# Create new data frame for frequency counts for bar graph
selected$irsex[selected$irsex == 1] <- "Male"
selected$irsex[selected$irsex == 2] <- "Female"
selected$suicthnk[selected$suicthnk == 1] <- "Seriously thought of"
selected$suicplan[selected$suicplan == 1] <- "Planned"
selected$suictry[selected$suictry == 1] <- "Attempted"
selected_stacked <- melt(selected, id.vars=1)
yes_only <- c("Seriously thought of", "Planned", "Attempted")
selected_stacked_2 <- filter(selected_stacked, value == yes_only)
behaviors_frequencies <- select(selected_stacked_2, irsex, value)
colnames(behaviors_frequencies) <- c("Gender", "Behavior")

# Create new data frame for frequency table
male_filter <- filter(behaviors_frequencies, Gender == "Male")
male_suicthnk <- nrow(filter(male_filter, Behavior == "Seriously thought of")) 
# 329 males seriously thought
male_suicplan <- nrow(filter(male_filter, Behavior == "Planned")) # 109 males planned
male_suictry <- nrow(filter(male_filter, Behavior == "Attempted")) # 31 males attempted
female_filter <- filter(behaviors_frequencies, Gender == "Female")
female_suicthnk <- nrow(filter(female_filter, Behavior == "Seriously thought of"))
# 442 females seriously thought
female_suicplan <- nrow(filter(female_filter, Behavior == "Planned")) # 153 females planned
female_suictry <- nrow(filter(female_filter, Behavior == "Attempted")) # 92 females attempted
male_comb <- c(male_suicthnk, male_suicplan, male_suictry)
female_comb <- c(female_suicthnk, female_suicplan, female_suictry)
suicthnk <- nrow(filter(behaviors_frequencies, Behavior == "Seriously thought of"))
suicplan <- nrow(filter(behaviors_frequencies, Behavior == "Planned"))
suictry <- nrow(filter(behaviors_frequencies, Behavior == "Attempted"))
both_comb <- c(suicthnk, suicplan, suictry)
genders_comb <- rbind(male_comb, female_comb, both_comb)
genders_comb <- as.data.frame(genders_comb)
colnames(genders_comb) <- c("Seriously thought", "Planned", "Attempted")
row.names(genders_comb) <- c("Male", "Female", "Both")
genders_comb <- rownames_to_column(genders_comb, var="Gender")
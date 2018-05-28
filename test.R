source("spider-functions.R")
df <- data.frame("E" = c(1, 3, 2, 1, 1))
names <- c(1, 2, 3)
filtertest <- filter(df, E %in% names)
age <- seq(1,5)
existing <- match(dp_filter[[1]], names(df_sum))
newNames <- dp_filter[[2]]
names(df_sum)[na.omit(existing)] <- newNames[which(!is.na(existing))]
df_binded <- rbind(df_sum[c(1,2,3,4,9)], df_sum[5:9])
df_sum <- subset(filtered_nsduh,NEWRACE2 == 1 & CATAG3 %in% age, select= c(dp_filter[[2]],dp_filter[[1]], "POVERTY3"))
df_sum <- df_sum[1:20,]
df_sum[1,1] <- "test1"
df_sum[1,9] <- "testing"
df_sum[1,5] <- "test1"
df_sum_old <- select(df_sum, dp_filter[[2]])
sum_old <- colSums(df_sum_old == 1, na.rm = TRUE)

df_binded_sum <- colSums(df_binded == 1, na.rm = TRUE)
                                                         
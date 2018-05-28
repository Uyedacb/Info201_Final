df <- data.frame("E" = c(1, 3, 2, 1, 1))
names <- c(1, 2, 3)
filtertest <- filter(df, E %in% names)

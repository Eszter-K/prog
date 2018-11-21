
# Question 1 --------------------------------------------------------------

library(tidyverse)
forbes <- read_csv("forbes.csv")
forbes$rank <- parse_number(forbes$rank)
forbes$age <- parse_integer(forbes$age, na = "-")
forbes$net_worth[1572:1578] <- "0.9" #values recorded in millions are set to billions, 
#to avoid confusion after parsing
forbes$net_worth <- parse_number(forbes$net_worth)
type_convert(forbes)

# Question 2 --------------------------------------------------------------
forbes <- filter(forbes, net_worth >= 1)



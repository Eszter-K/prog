
# Question 1 --------------------------------------------------------------

library(tidyverse)
forbes <- read_csv("forbes.csv")
forbes$rank <- parse_number(forbes$rank)
forbes$age <- parse_integer(forbes$age, na = "-")
forbes$net_worth[1572:1578] <- "0.9" 
#values recorded in millions are set to billions, to avoid confusion after parsing
forbes$net_worth <- parse_number(forbes$net_worth)
type_convert(forbes)

# Question 2 --------------------------------------------------------------
forbes <- filter(forbes, net_worth >= 1)

# Question 3 --------------------------------------------------------------
library(ggplot2)
ggplot(forbes, aes(x = age, y = net_worth)) + geom_point() + geom_smooth()
ggplot(forbes, aes(x = age, y = log(net_worth))) + geom_point() + geom_smooth()

#Plotting log(net_worth) helps, since 
#There does not appear to be a strong relationship between age and net worth.

# Question 4 --------------------------------------------------------------
forbes %>% 
  group_by(country) %>%
  mutate(
    rank = min_rank(net_worth),
    diff = max(rank) - min(rank),
    n = n()) %>% 
  filter(n > 6) %>% 
  arrange(diff) %>% 
  
# Question 5 --------------------------------------------------------------

         
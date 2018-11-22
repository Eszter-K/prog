
# Question 1 --------------------------------------------------------------

library(tidyverse)
forbes <- read_csv("forbes.csv")
forbes$rank <- parse_number(forbes$rank)
forbes$age <- parse_integer(forbes$age, na = "-")
forbes$net_worth[1572:1578] <- "0.9" 
#values recorded in millions are set to billions, to avoid confusion after parsing
forbes$net_worth <- parse_number(forbes$net_worth)
forbes <- type_convert(forbes)

# Question 2 --------------------------------------------------------------
forbes <- filter(forbes, net_worth >= 1)

# Question 3 --------------------------------------------------------------
ggplot(forbes, aes(x = age, y = net_worth)) + geom_point() + geom_smooth()
ggplot(forbes, aes(x = age, y = log(net_worth))) + geom_point() + geom_smooth()

#Plotting log(net_worth) helps, since 
#There does not appear to be a strong relationship between age and net worth.

# Question 4 --------------------------------------------------------------
forbesdiff <- forbes %>% 
  group_by(country) %>%
  summarize(
    diffsize = max(net_worth) - min(net_worth), 
    n = n()) %>% 
  filter(n >= 6) %>% 
  arrange(diffsize) 
  
# Question 5 --------------------------------------------------------------
ggplot(forbesdiff) +
  geom_bar(aes(x = country, y = diffsize, fill = country),  stat = "identity", show.legend = FALSE) + 
  coord_flip() +
  theme_minimal() +
  ggtitle("Difference between highest and lowest net worth per country ") + 
  ylab("Difference in net worth (in billion dollars)") +
  xlab("Country")

# Question 6 --------------------------------------------------------------
ggplot(forbesdiff) +
  geom_bar(aes(x = reorder(country, -diffsize), y = diffsize, fill = country),  stat = "identity", show.legend = FALSE) + 
  coord_flip() +
  theme_minimal() +
  ggtitle("Difference between highest and lowest net worth per country ") + 
  ylab("Difference in net worth (in billion dollars)") +
  xlab("Countries ordered by (ascending) difference in net worth")

# Question 7 --------------------------------------------------------------



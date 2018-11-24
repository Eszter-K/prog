
# Question 1 --------------------------------------------------------------

library(tidyverse)
forbes <- read_csv("forbes.csv")
forbes$rank <- parse_number(forbes$rank)
forbes$age <- parse_integer(forbes$age, na = "-")
forbes$net_worth[1572:1578] <- "0.9"
# values recorded in millions are set to billions, 
#to avoid confusion after parsing
forbes$net_worth <- parse_number(forbes$net_worth)
forbes <- type_convert(forbes)

# Question 2 --------------------------------------------------------------
forbes <- filter(forbes, net_worth >= 1)

# Question 3 --------------------------------------------------------------
ggplot(forbes, aes(x = age, y = net_worth)) + geom_point() + geom_smooth()

# The highest net_worth is 82.1 bill, but the bulk of the values in 
# net_worth are below 30 bill. As a result, this plot is rather "compressed", 
# with many points overlapping at the bottom. This makes the relationship 
#between age and net_worth difficult to see.

ggplot(forbes, aes(x = age, y = log(net_worth))) + geom_point() + 
  geom_smooth()

# Using a log scale makes the plot more readable, as it reduces the range of
# net_worth, making differences at the low end of the scale more visible.

# In this plot, the points are rather spread out, not showing a clear trend.
# There does not seem to be a relationship between age and net worth.

# Question 4 --------------------------------------------------------------
forbesdiff <- forbes %>%
  group_by(country) %>%
  summarize(
    diffsize = max(net_worth) - min(net_worth),
    n = n()) %>%
  filter(n >= 6) %>%
  arrange(diffsize)

View(forbesdiff)

# Question 5 --------------------------------------------------------------
ggplot(forbesdiff) +
  geom_bar(aes(x = country, y = diffsize, fill = country), 
           stat = "identity", 
           show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  ggtitle("Difference between highest and lowest net worth per country ") +
  ylab("Difference in net worth (in billion dollars)") +
  xlab("Country")

# Question 6 --------------------------------------------------------------
ggplot(forbesdiff) +
  geom_bar(aes(x = reorder(country, -diffsize), y = diffsize, fill = country), stat = "identity", show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  ggtitle("Difference between highest and lowest net worth per country ") +
  ylab("Difference in net worth (in billion dollars)") +
  xlab("Countries ordered by (ascending) difference in net worth")

# Question 7 --------------------------------------------------------------
forbes %>%
  group_by(rank) %>%
  summarize(shared_by = n()) %>%
  filter(shared_by > 1)

# Question 8 --------------------------------------------------------------
forbes %>%
  group_by(rank) %>%
  mutate(
    shared_by = n(),
    r = rank,
    sumrank = seq.int(from = r, length.out = length(shared_by)),
    average_rank = ifelse(shared_by > 1, sum(sumrank) / shared_by, rank)) %>%
  select(rank, average_rank)

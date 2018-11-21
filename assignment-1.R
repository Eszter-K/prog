
# Read forbes.csv ---------------------------------------------------------

library(tidyverse)

forbes <- read_csv("forbes.csv")
forbes$rank <- parse_number(forbes$rank)
forbes$net_worth <- parse_number(forbes$net_worth)
forbes$age <- parse_integer(forbes$age, na = "-")
type_convert(forbes)


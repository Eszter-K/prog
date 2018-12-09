library(tidyverse)

# Question 1 ------------------------------------------------------------------------------------------------------

#' Gather columns starting with a certain set of characters
#'
#' @param data A dataframe to be tidied.
#' @param column_prefix A character vector containing the string to use. The function gathers all 
#' columns that start with this string.
#'
#' @return A tibble with the relevant columns gathered. Columns that do not start with 
#' the given prefix are retained as is.
#'
tidy_df <- function(data, column_prefix){
  tidy_df <- gather(data, key = "variable", value = "value", starts_with(column_prefix))
  return(tidy_df)
}

#test function
tidy_df(austen_text, column_prefix = "t") %>% 
  select(id, variable)

# Question 2 ------------------------------------------------------------------------------------------------------

#' Get the Jane Austen data
#'
#' It will attempt to install the right package for you. If it does not work,
#'   try to install it manually.
#'
#' @return A data frame with Jane Austen texts, one line per row
get_jane_austen_data <- function(){
  
  tryCatch({library(gutenbergr)}, error = function(e){install.packages("gutenbergr")})
  library(gutenbergr)
  
  austen_text <- gutenberg_works(author == "Austen, Jane") %>% 
    gutenberg_download(meta_fields = "title") %>% mutate(id = row_number(gutenberg_id))
  assign("austen_text", austen_text, envir=.GlobalEnv)
  invisible()
}

# extract_possible_names 

df %>%
  filter(str_detect(df$text, "^[capitals]"))


# Question 3 ------------------------------------------------------------------------------------------------------

# filter_names



# Question 4 ------------------------------------------------------------------------------------------------------

# count_names_per_book

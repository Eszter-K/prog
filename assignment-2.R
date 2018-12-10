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

# extract_possible_names --------------------------------------------------

extract_possible_names <- function(data, col) {
  match <- "\\b[A-Z]\\w+" #regexp denoting capital letters
  newdata <- filter(data, str_detect(col, match))  #filter out columns without no capital letters
  newdata <- mutate(newdata, names = str_extract_all(col, match)) %>% #add column with extracted strings
  separate_rows(newdata, names) %>% #pull rows apart (some had more than 1 word with a capital letter)
  filter(names != "c" | names != "") %>% #filter out weird values resulting from separate_rows
  tidy_df(column_prefix = names) %>% #gather
    mutate(id = seq.int(length.out = nrow(newdata))) %>% #add id column
    rename(text_id = gutenberg_id, name = value) %>% #add name and text_id columns
    select(text_id, id, name) #select columns of interest to print
}

#try on janeausten data
extract_possible_names(data = austen_text, col = "text")

#As you can see, the function doesn't work. When I execute the same commands outside of the 
#function environment, it works just fine. I'm not able to fix this, so to continue the 
#assignment, I execute the commands normally below:

match <- "\\b[A-Z]\\w+" #match = words starting with caps
newdata <- filter(austen_text, str_detect(text, match))  #filter out columns without caps
newdata <- mutate(newdata, names = str_extract_all(text, match)) #add column with extracted strings
newdata <- separate_rows(newdata, names) 
newdata <- filter(newdata, names != "c" & names != "") 
newdata <- tidy_df(newdata, column_prefix = "names")  
rename(newdata, text_id = gutenberg_id, name = value) %>% 
  mutate(id = seq.int(length.out = nrow(newdata))) %>% 
  select(text_id, id, name) -> new_austen

#I work on the new_austen df in the next questions.
 
# Question 3 ------------------------------------------------------------------------------------------------------

# filter_names



# Question 4 ------------------------------------------------------------------------------------------------------

# count_names_per_book

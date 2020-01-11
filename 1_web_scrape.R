library(rvest)
library(tidyverse)
library(purrr)
library(stringr)
library(magrittr)

# define functions ----
read_slowly <- function(x, ...){
  output <- read_html(x)
  Sys.sleep(5)
  return(output)
}
read_safely <- possibly(read_slowly, NA)

#### PANCAKES -------------------------------------------------------------------------------
base_url <- "https://www.allrecipes.com/recipes/151/breakfast-and-brunch/pancakes/?internalSource=hubcard&referringContentType=Search&clickId=cardslot%201&page="
pages <- c(2:23)
pancake_urls <- paste0(base_url, pages)

# read all recipes from base pages ----
# map the read_html function to read all the pages at once
pancakes <- map(pancake_urls, read_slowly)

pancake_results <- pancakes %>% map(~html_nodes(.,".fixed-recipe-card__title-link"))

pancake_links <- pancake_results %>% map(~html_attr(.,"href")) %>% flatten_chr()

# remove NAs
pancake_links <- pancake_links[!is.na(pancake_links)]

# scrape ingredients ----
pancakes_html <- pancake_sample %>%
  map(~{
    read_safely(.x)
  })

# remove NA values
pancake_sample <- pancake_sample[!is.na(pancakes_html)]
pancakes_html <- pancakes_html[!is.na(pancakes_html)]

# extract ingredient list ----
pancake_ingredients <- pancakes_html %>% map(~html_nodes(., ".added")) %>% map(html_text)

# get num of servings
pancake_servings <- pancakes_html %>% 
  map(~html_nodes(., ".subtext")) %>%
  map(html_text) %>% 
  map(~str_extract(.,"(?<=yields\\s)\\d+")) %>% 
  map(., ~ifelse(identical(., character(0)), NA, .)) #replaces character(0) with NA

# get recipe ID
names_pancakes <- str_extract(pancake_sample, "(?<=/)\\d+")

# turn each list element to DF
pancake_ingredients %<>% 
  map(~ as_tibble(.))

# mutate to add column for num of servings
pancake_ingredients <- map2(pancake_ingredients, pancake_servings, ~mutate(.x, servings = as.integer(.y)))

# mutate to add column for recipe ID
pancake_ingredients <- map2(pancake_ingredients, names_pancakes, ~mutate(.x, recipe_id = as.integer(.y)))

# mutate to add pancake class
pancake_ingredients <- map(pancake_ingredients, ~mutate(.x, is_pancake = 1))

write_rds(pancake_ingredients, "pancakes_raw.rds")

#### BREAD (category actually includes variety of recipes) -------------------------------------------------------------------------------
bread_base_url <- "https://www.allrecipes.com/recipes/156/bread/?internalSource=hub%20nav&referringId=339&referringContentType=Recipe%20Hub&referringPosition=1&linkName=hub%20nav%20exposed&clickId=hub%20nav%203&page="
bread_pages <- c(2:167)
bread_urls <- paste0(bread_base_url, bread_pages)

bread <- map(bread_urls, read_slowly)

bread_results <- bread %>% map(~html_nodes(.,".fixed-recipe-card__title-link"))

bread_links <- bread_results %>% map(~html_attr(.,"href")) %>% flatten_chr()

# remove NAs
bread_links <- bread_links[!is.na(bread_links)]

# scrape ingredients ----

bread_html <- map(bread_sample, ~{
    read_safely(.x)
  })

# remove NA values
bread_sample <- bread_sample[!is.na(bread_html)]
bread_html <- bread_html[!is.na(bread_html)]

# extract ingredient list ----
bread_ingredients <- bread_html %>% map(~html_nodes(., ".added")) %>% map(html_text)

# get num of servings
bread_servings <- bread_html %>% 
  map(~html_nodes(., ".subtext")) %>%
  map(html_text) %>% 
  map(~str_extract(.,"(?<=yields\\s)\\d+")) %>% 
  map(., ~ifelse(identical(., character(0)), NA, .)) # some come through as character(0)--this removes

# get recipe ID
names_bread <- str_extract(bread_sample, "(?<=/)\\d+")

# turn each list elemtent to DF
bread_ingredients %<>% 
  map(~ as_tibble(.))

# mutate to add column for num of servings
bread_ingredients <- map2(bread_ingredients, bread_servings, ~mutate(.x, servings = as.integer(.y)))

# mutate to add column for recipe ID
bread_ingredients <- map2(bread_ingredients, names_bread, ~mutate(.x, recipe_id = as.integer(.y)))

# mutate to add pancake class
bread_ingredients <- map(bread_ingredients, ~mutate(.x, is_pancake = 0))

write_rds(bread_ingredients, "bread_raw.rds")

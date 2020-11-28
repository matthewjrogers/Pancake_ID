library(tidyverse)
library(purrr)
library(stringr)
library(magrittr)
library(stringi)

raw_tidy_df <- read_csv("./Cleaned Data/pancake_dataset.csv")

# engineering initial features of interest ------
# NOTE: Some values for servings very large: spot checked ~ a dozen, and all seem to have been read in properly
# std_df <- raw_tidy_df %>% # standardize all ingredients by number of servings
#   mutate_at(vars(-c(recipe_id, is_pancake, servings)), funs(. / servings))

engineered_df <- raw_tidy_df %>% 
  group_by(recipe_id) %>% 
  mutate(egg = eggs * (3.25*0.0625), # eggs to volume--assuming all eggs are 'large' (average 3.25 tbsp)
         total_volume = sum(-c(servings, is_pancake, fruit),na.rm = TRUE),
         total_liquid = sum(buttermilk, fruit_juice, milk, water, yogurt, egg, sour_cream, na.rm = TRUE), 
         prop_liquid = total_liquid/ total_volume, # given that pancakes are a batter, we can expect a high proportion of liquid
         total_fat = sum(butter, oil, shortening, na.rm = TRUE),
         prop_fat = total_fat / total_volume) %>% 
  ungroup() %>% 
  mutate(is_pancake = case_when(is_pancake == 1 ~ "pancake", # some algorithms prefer factors
                                TRUE ~ "other")) %>% 
  filter(total_liquid > 0) # assume that all recipes require some amount of liquid/wet ingredients

# create training and test sets ----
# split and re-combine to ensure a reasonable number of pancake recipes fall into each set
pancakes <- engineered_df %>% 
  filter(is_pancake == "pancake")

not_pancakes <- engineered_df %>% 
  filter(is_pancake == "other")

# randomly reorder data
rand_pancake_data <- pancakes[sample(nrow(pancakes)),]
rand_not_pancakes_data <- not_pancakes[sample(nrow(not_pancakes)),]

# get split for 2/3rds of data--for training/test split
pancake_split <- round(nrow(rand_pancake_data) * .66)
not_pancakes_split <- round(nrow(rand_not_pancakes_data) * .66) 

# split data
pancake_test <- rand_pancake_data[pancake_split:nrow(rand_pancake_data),]
pancake_train <- rand_pancake_data[1:pancake_split,]

not_pancake_test <- rand_not_pancakes_data[not_pancakes_split:nrow(rand_not_pancakes_data),]
not_pancakes_train <- rand_not_pancakes_data[1:not_pancakes_split,]


 # write CSVs
pancake_train %>% 
  bind_rows(not_pancakes_train) %>% 
  write_csv("pancakes_training_set2.csv")

pancake_test %>% 
  bind_rows(not_pancake_test) %>% 
  write_csv("pancakes_validation_set2.csv")




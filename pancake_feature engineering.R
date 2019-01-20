.libPaths("D:/R/win-library/3.4")
library(tidyverse)
library(purrr)
library(stringr)
library(magrittr)
library(stringi)

raw_tidy_df <- read_csv("pancake_dataset.csv")

engineered_df <- raw_tidy_df %>% 
  group_by(recipe_id) %>% 
  mutate(total_volume = sum(baking_powder, baking_soda, bran, butter, buttermilk, cheese, chocolate, cornmeal, cream, cream_cheese,
                            flour, fruit_juice, honey, low_cal_sweetener, margarine, mayonnaise, milk, mix, molasses, oats, oil, other,
                            salt, shortening, sour_cream, spice, starch, sugar, syrup, vanilla, vinegar, water, wheat_germ, yeast, yogurt,
                            na.rm = T),
         std_volume = total_volume / servings,
         total_liquid = sum(buttermilk, fruit_juice, milk, water, yogurt, sour_cream, na.rm = T),
         std_liquid = total_liquid / servings,
         prop_liquid = total_liquid/ total_volume,
         total_fat = sum(butter, oil, shortening, na.rm = T),
         prop_fat = total_fat / total_volume) %>% 
  ungroup() %>% 
  select(is_pancake, recipe_id, servings, flour, eggs, baking_powder, baking_soda, butter, oil, buttermilk, fruit, milk, sugar, water,
         total_volume, std_volume, total_liquid, std_liquid, prop_liquid, total_fat, prop_fat)

# create training and validation sets ----
pancakes <- engineered_df %>% 
  filter(is_pancake == 1)

not_pancakes <- engineered_df %>% 
  filter(is_pancake == 0)

rand_pancake_rows <- sample(nrow(pancakes)) # create vector with scrambled row ids
rand_pancake_data <- pancakes[rand_pancake_rows,] # randomly reorder data using vector

pancake_split <- round(nrow(rand_pancake_data) * .66) # assign two thirds of data to training set

pancake_validation <- rand_pancake_data[pancake_split:nrow(rand_pancake_data),]
pancake_train <- rand_pancake_data[1:pancake_split,]


rand_not_pancakes_rows <- sample(nrow(not_pancakes)) # create vector with scrambled row ids
rand_not_pancakes_data <- not_pancakes[rand_not_pancakes_rows,] # randomly reorder data using vector

not_pancakes_split <- round(nrow(rand_not_pancakes_data) * .66) # assign two thirds of data to training set

not_pancakes_validation <- rand_not_pancakes_data[not_pancakes_split:nrow(rand_not_pancakes_data),]
not_pancakes_train <- rand_not_pancakes_data[1:not_pancakes_split,]

 # write CSVs
pancake_train %>% 
  bind_rows(not_pancakes_train) %>% 
  write_csv("pancakes_training_set.csv")

pancake_validation %>% 
  bind_rows(not_pancakes_validation) %>% 
  write_csv("pancakes_validation_set.csv")
library(tidyverse)
library(purrr)
library(stringr)
library(magrittr)
library(stringi)

# Define Functions --------------------------------------------------------

multi_detect <- function(string, pattern){
  map_lgl(string, ~stri_detect_regex(., pattern) %>% any(.))
}

# Join Data ---------------------------------------------------------------

pancake_ingredients <- read_rds("pancakes_raw.rds")
bread_ingredients <- read_rds("bread_raw.rds")

pancake_df <- pancake_ingredients %>% 
  bind_rows()

bread_df <- bread_ingredients %>% 
  bind_rows()

all_recipes <- bind_rows(pancake_df, bread_df) %>% 
  filter(value != "Add all ingredients to list")

# ingredient groups --------------------------------------------

fruits <- c("\\w*apple", "\\w*berr(y|ies)", "raisin", "lemon", "banana",
            "orange", "pear", "peach", "rhubarb", "plum", "cherry", "fruit", "lime", "mango", "currant", "cherr(y|ies)")
nuts <- c("\\w*nut", "almond", "pecan", "seed")
veggies <- c("carrot", "pumpkin", "potato", "zucchini", "\\bcorn\\b", "\\byam")
spices <- c("cinnamon", "nutmeg", "clove", "ginger", "spice", "cardamom", "pepper")

# Extract Ingredients -----------------------------------------------------


all_recipes %<>%  
  mutate(value = tolower(value)) %>% 
  mutate(amount = str_extract(.$value, "^[\\d, /]+"),
         units = str_extract(.$value, "(\\(.+\\)|[^0-9, /]+)"), 
         ingredient = case_when(
           str_detect(.$value, "flour") ~ "flour",
           str_detect(.$value, "starch") ~ "starch",
           str_detect(.$value, "corn\\s?meal") ~ "cornmeal",
           str_detect(.$value, "bran") ~ "bran",
           str_detect(.$value, "\\boat") ~ "oats", 
           str_detect(.$value, "wheat germ") ~ "wheat germ",
           str_detect(.$value, "sugar") ~ "sugar",
           str_detect(.$value, "juice") ~ "fruit juice",
           str_detect(.$value, "honey") ~ "honey",
           str_detect(.$value, "agave") ~ "agave",
           str_detect(.$value, "syrup") ~ "syrup",
           str_detect(.$value, "molasses") ~ "molasses",
           str_detect(.$value, "frosting") ~ "frosting",
           str_detect(.$value, "buttermilk") ~ "buttermilk", 
           str_detect(.$value, "milk") ~ "milk",
           str_detect(.$value, "cream cheese") ~ "cream cheese",
           str_detect(.$value, "sour cream") ~ "sour cream",
           str_detect(.$value, "tartar") ~ "other", 
           str_detect(.$value, "\\bcream\\b") ~ "cream", 
           str_detect(.$value, "yogurt") ~ "yogurt",
           str_detect(.$value, "cheese") ~ "cheese", 
           str_detect(.$value, "baking soda")  ~ "baking soda",
           str_detect(.$value, "baking powder")~ "baking powder",
           str_detect(.$value,  "yeast") ~ "yeast",
           str_detect(.$value, "\\boil\\b") ~ "oil",
           str_detect(.$value, "shortening") ~ "shortening",
           str_detect(.$value, "margarine") ~ "margarine",
           str_detect(.$value, "\\bsalt\\b") ~ "salt", 
           str_detect(.$value, "water") ~ "water",
           str_detect(.$value, "vanilla extract")  ~ "vanilla",
           str_detect(.$value, "chocolate") ~ "chocolate",
           str_detect(.$value, "cocoa") ~ "chocolate",
           str_detect(.$value, "egg") ~ "eggs",
           str_detect(.$value, "vinegar") ~ "vinegar",
           multi_detect(.$value, spices) ~ "spice",
           multi_detect(.$value, nuts) ~ "nut",
           multi_detect(.$value, fruits) ~ "fruit",
           multi_detect(.$value, veggies) ~ "vegetable",
           str_detect(.$value, "butter") ~ "butter",
           TRUE ~ "other"))



all_recipes %<>% 
  filter(units != "wet",
         units != "dry",
         !is.na(amount)) %>% 
  mutate(amount = str_trim(amount)) %>% 
  mutate(dec_amount = str_replace(amount, "\\s", "+")) %>% 
  mutate(dec_amount = sapply(dec_amount, function(x) eval(parse(text = x))))

all_recipes %<>%
  mutate(dec_amount = ifelse(str_detect(units, "\\(.+ (ounce|oz.)"),
                             as.numeric(str_extract(units, "\\.?\\d+\\.?\\d*")) * dec_amount,
                             dec_amount),
         units = ifelse(str_detect(units, "\\(.+ (ounce|oz.)"), "ounces", units))

all_recipes %<>%
  mutate(dec_amount = ifelse(units == "pinch" & is.na(amount), 1, dec_amount))

# Standardize Units -------------------------------------------------------

conversion_units <- c("tablespoon", "teaspoon", "cup", "ounce", "pint", "pinch")

all_recipes %<>% # standardize variable units to cups
  mutate(std_amount = case_when(ingredient == "eggs" ~ dec_amount,
                                str_detect(units, "cup") ~ dec_amount,
                                str_detect(ingredient, "fruit") ~ dec_amount,
                                str_detect(units, "pinch") ~ dec_amount / 768,
                                str_detect(units, "teaspoon") ~ dec_amount * 0.0208333,
                                str_detect(units, "tablespoon") ~ dec_amount * 0.0625,
                                str_detect(units, "ounce") ~ dec_amount * 0.125,
                                str_detect(units, "pint") ~ dec_amount * 2)) %>%
  mutate(std_units = case_when(multi_detect(units, conversion_units) ~ "cups",
                               ingredient == "eggs"               ~ "eggs"))


# Group Duplicate Ingredients ---------------------------------------------


final_recipes <- all_recipes %>% 
  group_by(is_pancake, recipe_id, ingredient) %>%
  summarise(amount_sum = sum(std_amount), # collapse multiple obs into single value (e.g. white sugar and brown sugar)
            units_std = first(std_units),
            servings = first(servings),
            value = paste(value, collapse = ";")
  ) %>%
  ungroup()


# Tidy Data (Recipe as Unique ID), ingredients as features -----------------------------------------

final_recipes %<>% 
  mutate(ingredient = str_replace(ingredient, " ", "_"), # tidy up variable names preemptively
         ingredient = str_replace(ingredient, "-", "_")) %>% 
  select(-c(value, units_std)) %>% # full value and std units not necessary (all in cups except eggs)
  group_by(is_pancake, recipe_id) %>% 
  spread(ingredient, amount_sum) %>% # turn each ingredient into a feature
  summarise_all(.funs = sum) %>% # collapse such that each recipe is a unique observation
  ungroup()

tidy_recipes <- all_recipes %>% 
  distinct(recipe_id, servings) %>% # filter out a few duplicates
  left_join(final_recipes) %>% # the summarise_all call above created large observations for servings, joining to 
  distinct(recipe_id, .keep_all = T) %>% 
  select(recipe_id, is_pancake, servings, everything()) %>% # reorder
  filter(flour != is.na(flour)) # remove flourless recipes

# checked to make sure that no variables were entirely missing

write_csv(tidy_recipes, "pancake_dataset.csv")


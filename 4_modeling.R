library(tidyverse) # data manipulation / loading
library(caret) # diverse package interface
library(mlbench)
library(ranger) # random forests
library(rpart.plot)
library(rattle) # Decision tree plots
library(pROC) # AUC/ROC plotting
library(randomForest)
library(xgboost)
library(Matrix)
library(class)
library(recipes)
set.seed(123)


# read data ---------------------------------------------------------------

train_full <- read_csv("pancakes_training_set2.csv") %>% 
  select(-c(recipe_id)) %>% 
  mutate_all(list(~replace(., is.na(.), 0))) %>% 
  mutate_if(is.character, as.factor)

test_full <- read_csv("pancakes_validation_set2.csv") %>% 
  select(-c(recipe_id)) %>% 
  mutate_all(list(~replace(., is.na(.), 0))) %>% 
  mutate_if(is.character, as.factor)

# pre-processing ----------------------------------------------------------

pancake_recipe <- recipe(is_pancake ~ ., data = train_full %>% select(-c(servings, eggs)))

pancake_recipe <- pancake_recipe %>% 
  step_nzv(all_predictors()) 
  # step_scale(all_predictors()) %>% 
  # step_center(all_predictors())

prepped_pancakes <- prep(pancake_recipe, data = train_full)

processed_train <- bake(prepped_pancakes, train_full)
processed_test <- bake(prepped_pancakes, test_full)

# xgboost processing --------------------------------------------------------


train_matrix <- processed_train %>% 
  select(-is_pancake) %>% 
  as("matrix")

train_outcomes <- processed_train %>% 
  select(is_pancake) %>% 
  mutate(is_pancake = case_when(is_pancake == "pancake" ~ 1,
                                TRUE ~ 0)) %>% 
  pull()

xg_train <- list(data = train_matrix, 
                 label = train_outcomes)


test_matrix <- processed_test %>% 
  select(-is_pancake) %>% 
  as("matrix")

test_outcomes <- processed_test %>% 
  select(is_pancake) %>% 
  mutate(is_pancake = case_when(is_pancake == "pancake" ~ 1,
                                TRUE ~ 0)) %>% 
  pull()

xg_test <- list(data = test_matrix, 
                label = test_outcomes)


# smote -------------------------------------------------------------------
library(DMwR)

# SMOTE chokes on tibbles
smote_train = SMOTE(is_pancake ~., data = as.data.frame(processed_train))

smote_train_xg <- smote_train %>% 
  mutate(is_pancake = case_when(is_pancake == "pancake" ~ 1L,
                                TRUE ~ 0L))

test_xg <- processed_test %>% 
  mutate(is_pancake = case_when(is_pancake == "pancake" ~ 1L,
                                TRUE ~ 0L))
# xgboost modelling -- xgboost package ------------------------------------


xg_model <- xgboost(data = as.matrix(smote_train_xg %>% select(-is_pancake)), 
                    label = smote_train_xg$is_pancake, 
                    max.depth = 15,
                    eta = 1, 
                    nthread = 2, 
                    nrounds = 10, 
                    objective = "binary:logistic")


xg_pred <- predict(xg_model, as.matrix(processed_test %>% select(-is_pancake)))
xg_prediction <- as.numeric(xg_pred > 0.55)
average_error <- mean(as.numeric(xg_pred > 0.40) != test_xg$is_pancake)
print(average_error)

table(xg_prediction, test_xg$is_pancake)


# random forest model -----------------------------------------------------

# define custom train control object
trCont <- trainControl(method = "cv", number = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE
                       )

rf_model <- train(is_pancake ~., smote_train,
                  method = "ranger", # preferred RF method
                  tuneLength = 10,
                  importance = "impurity", # set to "impurity" or "permutation" to get variable importance [varImp()]
                  trControl = trCont
)



plot(rf_model)

pred <- predict(rf_model, processed_test %>% select(-is_pancake))

table(pred, processed_test$is_pancake)

plot(varImp(rf_model), top = 15)


library(data.table)
test_recipe <- data.table(baking_powder = 3.25*0.0208333,
                          flour = 1.5,
                          sugar = .0625,
                          milk = 1.25,
                          egg = 3.25*.0625,
                          butter = 3*.0625
                          )

test_recipe[, total_volume := rowSums(.SD)]
test_recipe[, `:=`(total_liquid = milk + egg,
                   total_fat = butter
                   )]
test_recipe[, `:=`(prop_liquid = total_liquid / total_volume,
                   prop_fat = total_fat / total_volume
)]

test_recipe[, names(processed_test)[!names(processed_test) %in% c(names(test_recipe), 'is_pancake')] := 0]

processed_test_rec <- bake(prepped_pancakes, test_recipe)

predict(rf_model, processed_test_rec) %>% as.character()

# model for export to app -------------------------------------------------

full_data <- bind_rows(train_full, test_full)
pancake_recipe <- recipe(is_pancake ~ ., data = full_data %>% select(-c(servings, eggs)))

pancake_recipe <- pancake_recipe %>% 
  step_nzv(all_predictors()) 
# step_scale(all_predictors()) %>% 
# step_center(all_predictors())

prepped_pancakes <- prep(pancake_recipe, data = full_data)



full_processed_data <- bake(prepped_pancakes, full_data)

smote_full = SMOTE(is_pancake ~., data = as.data.frame(full_processed_data))

trCont <- trainControl(method = "cv", number = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE
)

rf_model <- train(is_pancake ~., smote_full,
                  method = "ranger", # preferred RF method
                  tuneLength = 10,
                  importance = "impurity", # set to "impurity" or "permutation" to get variable importance [varImp()]
                  trControl = trCont
)



save(file = "pancake_rf_no_servings.RDS", rf_model)
save(file = "pancake_recipe_no_servings.RDS", prepped_pancakes)

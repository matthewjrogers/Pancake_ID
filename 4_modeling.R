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

train_full <- read_csv("pancakes_training_set.csv") %>% 
  select(-c(recipe_id)) %>% 
  mutate_all(funs(replace(., is.na(.), 0))) %>% 
  mutate_if(is.character, as.factor)

test_full <- read_csv("pancakes_validation_set.csv") %>% 
  select(-c(recipe_id)) %>% 
  mutate_all(funs(replace(., is.na(.), 0))) %>% 
  mutate_if(is.character, factor)
# pre-processing ----------------------------------------------------------

pancake_recipe <- recipe(is_pancake ~ ., data = train_full)

pancake_recipe <- pancake_recipe %>% 
  step_nzv(all_predictors()) %>% 
  step_scale(all_predictors()) %>% 
  step_center(all_predictors())

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
  mutate(is_pancake = case_when(is_pancake == "pancake" ~ 1,
                                TRUE ~ 0))


# xgboost modelling -- xgboost package ------------------------------------


xg_model <- xgboost(data = as.matrix(smote_train_xg %>% select(-is_pancake)), 
                    label = smote_train$is_pancake, 
                    max.depth = 15,
                    eta = 1, 
                    nthread = 2, 
                    nrounds = 10, 
                    objective = "binary:logistic")


xg_pred <- predict(xg_model, xg_test$data)
xg_prediction <- as.numeric(xg_pred > 0.6)
average_error <- mean(as.numeric(xg_pred > 0.40) != xg_test$label)
print(average_error)

table(xg_prediction, xg_test$label)


# random forest model -----------------------------------------------------

# define custom train control object
trCont <- trainControl(method = "cv", number = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE)

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

save(file = "pancake_rf.RDS", rf_model)
save(file = "pancake_recipe.RDS", prepped_pancakes)

library(tidyverse)
library(tidymodels)
library(vroom)
library(glmnet)
library(rpart)
library(ranger)
library(bonsai)
library(lightgbm)

## Read in Training Data & Clean

trainData <- vroom('train.csv') %>%
  select(-casual, -registered) %>%
  mutate(count = log(count))

## Read in Test Data

testData <- vroom('test.csv')

## Feature Engineering

bike_recipe <- recipe(count~., data=trainData) %>%
  step_mutate(weather=ifelse(weather == 4, 3, weather)) %>%
  step_mutate(weather=as.factor(weather)) %>%
  step_mutate(holiday=as.factor(holiday)) %>%
  step_mutate(workingday=as.factor(workingday)) %>%
  step_time(datetime, features=c("hour")) %>%
  step_mutate(season=factor(season)) %>%
  step_date(datetime, features="dow") %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_rm(datetime)

## Create Regression Tree for Tuning

reg_tree <- decision_tree(tree_depth = tune(),
                          cost_complexity = tune(),
                          min_n=tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

## Try Random Forest for Tuning

rand_for <- rand_forest(mtry = tune(),
                        min_n=tune(),
                        trees=1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

## Boosted Tree

boost_model <- boost_tree(tree_depth=tune(),
                          trees=tune(),
                          learn_rate=tune()) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")

## Create Workflow

bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(boost_model)

## Tuning

### Grid of Values

grid_of_tuning_params <- grid_regular(tree_depth(),
                                      trees(), 
                                      learn_rate(),
                                      levels = 5)

### Split Data & Run CV

folds <- vfold_cv(trainData, v = 10, repeats=1)
CV_results <- bike_workflow %>%
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params,
            metrics=metric_set(rmse, mae, rsq))

### Best Parameters

bestTune <- CV_results %>%
  select_best(metric="rmse")

## Finalize Workflow & Fit

final_wf <-
  bike_workflow %>%
  finalize_workflow(bestTune) %>%
  fit(data=trainData)

## Run Predictions

bike_predictions <- predict(final_wf, new_data = testData) %>%
  mutate(.pred = exp(.pred))

## Format the Predictions for Submission to Kaggle

kaggle_submission <- bike_predictions %>%
  bind_cols(., testData) %>%
  select(datetime, .pred) %>% 
  rename(count=.pred) %>% 
  mutate(count=pmax(0, count)) %>%
  mutate(datetime=as.character(format(datetime)))

## Write Submission File

vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=',')
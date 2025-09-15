library(tidyverse)
library(tidymodels)
library(vroom)
library(glmnet)

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

## Create Penalized Regression Model

### Combo 1: Penalty 0 and Mixture 0

preg_model <- linear_reg(penalty=0, mixture=0) %>%
  set_engine("glmnet")

### Combo 2: Penalty 1 and Mixture 0.5

#preg_model <- linear_reg(penalty=1, mixture=0.5) %>%
  #set_engine("glmnet")

### Combo 3: Penalty 1 and Mixture 1

#preg_model <- linear_reg(penalty=1, mixture=1) %>%
  #set_engine("glmnet")

### Combo 4: Penalty 5 and Mixture 0.25

#preg_model <- linear_reg(penalty=5, mixture=0.25) %>%
  #set_engine("glmnet")

### Combo 5: Penalty 2 and Mixture 0.75

#preg_model <- linear_reg(penalty=2, mixture=0.75) %>%
  #set_engine("glmnet")

## Create Workflow

bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model) %>%
  fit(data=trainData)

## Run Predictions

bike_predictions <- predict(bike_workflow, new_data = testData) %>%
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